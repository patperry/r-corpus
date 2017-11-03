/*
 * Copyright 2017 Patrick O. Perry.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "rcorpus.h"


static int filter_logical(SEXP filter, const char *key, int nullval)
{
	SEXP val = getListElement(filter, key);

	if (val == R_NilValue) {
		return nullval;
	}

	return (LOGICAL(val)[0] == TRUE);
}


static int filter_type_kind(SEXP filter)
{
	int kind;

	kind = 0;

	if (filter == R_NilValue) {
		kind |= UTF8LITE_TEXTMAP_CASE;
		kind |= UTF8LITE_TEXTMAP_QUOTE;
		kind |= UTF8LITE_TEXTMAP_RMDI;
		return kind;
	}

	if (filter_logical(filter, "map_case", 0)) {
		kind |= UTF8LITE_TEXTMAP_CASE;
	}

	if (filter_logical(filter, "map_quote", 0)) {
		kind |= UTF8LITE_TEXTMAP_QUOTE;
	}

	if (filter_logical(filter, "remove_ignorable", 0)) {
		kind |= UTF8LITE_TEXTMAP_RMDI;
	}

	return kind;
}


SEXP as_text_filter_connector(SEXP value)
{
	SEXP con;
	const uint8_t *ptr, *str;
	int32_t ch;
	int len;

	if (value == R_NilValue) {
		error("'connector' cannot be NULL");
	}

	con = STRING_ELT(value, 0);
	if (con == NA_STRING) {
		error("'connector' cannot be NA");
	}

	str = (const uint8_t *)CHAR(con);
	len = LENGTH(con);

	ptr = str;
	utf8lite_decode_utf8(&ptr, &ch);
	if (ptr != str + len) {
		error("'connector' must be a single-character string");
	}

	if (utf8lite_isspace(ch)) {
		error("'connector' cannot be a white space character");
	}

	return value;
}


static int32_t filter_connector(SEXP filter)
{
	SEXP value, con;
	const uint8_t *str;
	int32_t connector = '_';

	value = getListElement(filter, "connector");
	if (value == R_NilValue) {
		return connector;
	}

	con = STRING_ELT(value, 0);
	if (con == NA_STRING) {
		return connector;
	}

	str = (const uint8_t *)CHAR(con);
	utf8lite_decode_utf8(&str, &connector);
	return connector;
}



static const char *filter_stemmer_snowball(SEXP alg)
{
	const char *name;
	name = CHAR(STRING_ELT(alg, 0));
	return name;
}


static int filter_flags(SEXP filter)
{
	int flags = CORPUS_FILTER_KEEP_ALL;

	if (filter_logical(filter, "drop_letter", 0)) {
		flags |= CORPUS_FILTER_DROP_LETTER;
	}

	if (filter_logical(filter, "drop_number", 0)) {
		flags |= CORPUS_FILTER_DROP_NUMBER;
	}

	if (filter_logical(filter, "drop_punct", 0)) {
		flags |= CORPUS_FILTER_DROP_PUNCT;
	}

	if (filter_logical(filter, "drop_symbol", 0)) {
		flags |= CORPUS_FILTER_DROP_SYMBOL;
	}

	return flags;
}


static void add_terms(int (*add_term)(void *, const struct utf8lite_text *),
		      void *f, SEXP sterms)
{
	const struct utf8lite_text *terms;
	R_xlen_t i, n;
	int err = 0;

	if (sterms == R_NilValue) {
		return;
	}

	PROTECT(sterms = coerce_text(sterms));
	terms = as_text(sterms, &n);

	for (i = 0; i < n; i++) {
		if (!terms[i].ptr) {
			continue;
		}

		TRY(add_term(f, &terms[i]));
	}
out:
	UNPROTECT(1);
	CHECK_ERROR(err);
}


static int add_stem_except(void *obj, const struct utf8lite_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_stem_except(f, x);
}


static int add_drop(void *obj, const struct utf8lite_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_drop(f, x);
}


static int add_drop_except(void *obj, const struct utf8lite_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_drop_except(f, x);
}


static int add_combine(void *obj, const struct utf8lite_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_combine(f, x);
}


struct corpus_filter *text_filter(SEXP x)
{
	SEXP handle, filter, combine, stemmer;
	struct rcorpus_text *obj;
	const char *snowball;
	int32_t connector;
	int err = 0, nprot = 0, type_kind, flags, stem_dropped;

	handle = getListElement(x, "handle");
	obj = R_ExternalPtrAddr(handle);

	// check the stemmer for errors
	if (obj->has_stemmer && obj->stemmer.error) {
		obj->valid_filter = 0;
	}

	if (obj->has_filter) {
		if (obj->valid_filter && !obj->filter.error) {
			return &obj->filter;
		} else {
			corpus_filter_destroy(&obj->filter);
			obj->has_filter = 0;
			if (obj->has_stemmer) {
				stemmer_destroy(&obj->stemmer);
				obj->has_stemmer = 0;
			}
		}
	}
	obj->valid_filter = 0;

	filter = getListElement(x, "filter");
	type_kind = filter_type_kind(filter);
	combine = getListElement(filter, "combine");
	connector = filter_connector(filter);
	flags = filter_flags(filter);
	stem_dropped = filter_logical(filter, "stem_dropped", 0);

	if (obj->has_stemmer && obj->stemmer.error) {
		stemmer_destroy(&obj->stemmer);
		obj->has_stemmer = 0;
	}

	if (!obj->has_stemmer) {
		stemmer = getListElement(filter, "stemmer");
		
		if (stemmer == R_NilValue) {
			stemmer_init_none(&obj->stemmer);
		} else if (TYPEOF(stemmer) == STRSXP) {
			snowball = filter_stemmer_snowball(stemmer);
			stemmer_init_snowball(&obj->stemmer, snowball);
		} else if (isFunction(stemmer)) {
			stemmer_init_rfunc(&obj->stemmer, stemmer,
					   R_GlobalEnv);
		} else {
			error("invalid filter 'stemmer' value");
		}

		obj->has_stemmer = 1;
	}

	TRY(corpus_filter_init(&obj->filter, flags, type_kind,
			       connector, obj->stemmer.stem_func,
			       obj->stemmer.stem_context));
	obj->has_filter = 1;

	if (!stem_dropped) {
		add_terms(add_stem_except, &obj->filter,
			  getListElement(filter, "drop"));
	}
	add_terms(add_stem_except, &obj->filter,
		  getListElement(filter, "stem_except"));
add_terms(add_drop, &obj->filter,
		  getListElement(filter, "drop"));
	add_terms(add_drop_except, &obj->filter,
		  getListElement(filter, "drop_except"));
	add_terms(add_combine, &obj->filter, combine);
out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	obj->valid_filter = 1;
	return &obj->filter;
}


static int sentfilter_flags(SEXP filter)
{
	int flags = CORPUS_SENTSCAN_SPCRLF;

	if (filter == R_NilValue) {
		return flags;
	}

	if (filter_logical(filter, "sent_crlf", 0)) {
		flags &= ~CORPUS_SENTSCAN_SPCRLF;
	}

	return flags;
}


static int add_suppress(void *obj, const struct utf8lite_text *x)
{
	struct corpus_sentfilter *f = obj;
	return corpus_sentfilter_suppress(f, x);
}


struct corpus_sentfilter *text_sentfilter(SEXP x)
{
	SEXP handle, filter, abbrev_kind, suppress;
	struct rcorpus_text *obj;
	int err = 0, nprot = 0, flags;

	handle = getListElement(x, "handle");
	obj = R_ExternalPtrAddr(handle);

	if (obj->has_sentfilter) {
		if (obj->valid_sentfilter && !obj->sentfilter.error) {
			return &obj->sentfilter;
		} else {
			corpus_sentfilter_destroy(&obj->sentfilter);
			obj->has_sentfilter = 0;
		}
	}
	obj->valid_sentfilter = 0;

	filter = getListElement(x, "filter");
	flags = sentfilter_flags(filter);

	if (filter == R_NilValue) {
		PROTECT(abbrev_kind = mkString("english")); nprot++;
		PROTECT(suppress = abbreviations(abbrev_kind)); nprot++;
	} else {
		suppress = getListElement(filter, "sent_suppress");
	}

	TRY(corpus_sentfilter_init(&obj->sentfilter, flags));
	obj->has_sentfilter = 1;

	add_terms(add_suppress, &obj->sentfilter, suppress);

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	obj->valid_sentfilter = 1;
	return &obj->sentfilter;
}
