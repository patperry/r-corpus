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
		kind |= CORPUS_TYPE_MAPCASE;
		kind |= CORPUS_TYPE_MAPQUOTE;
		kind |= CORPUS_TYPE_RMDI;
		return kind;
	}

	if (filter_logical(filter, "map_case", 0)) {
		kind |= CORPUS_TYPE_MAPCASE;
	}

	if (filter_logical(filter, "map_quote", 0)) {
		kind |= CORPUS_TYPE_MAPQUOTE;
	}

	if (filter_logical(filter, "remove_ignorable", 0)) {
		kind |= CORPUS_TYPE_RMDI;
	}

	return kind;
}


static uint32_t filter_connector(SEXP filter)
{
	(void)filter;
	return CORPUS_FILTER_CONNECTOR;
}


struct stemmer {
	const char *abbrev;
	const char *name;
};

static const struct stemmer stemmers[]  = {
	{ "ar", "arabic" },
	{ "da", "danish" },
	{ "de", "german" },
	{ "en", "english" },
	{ "es", "spanish" },
	{ "fi", "finnish" },
	{ "fr", "french" },
	{ "hu", "hungarian" },
	{ "it",  "italian" },
	{ "nl", "dutch" },
	{ "no", "norwegian" },
	{ "porter", "porter" },
	{ "pt", "portuguese" },
	{ "ro", "romanian" },
	{ "ru", "russian" },
	{ "sv", "swedish" },
	{ "ta", "tamil" },
	{ "tr", "turkish" },
	{ NULL, NULL } };

static const char *filter_stemmer(SEXP filter)
{
	const struct stemmer *stem = stemmers;
	SEXP alg = getListElement(filter, "stemmer");
	const char *val;

	if (alg == R_NilValue) {
		return NULL;
	}

	val = CHAR(STRING_ELT(alg, 0));

	while (stem->abbrev) {
		if (strcmp(val, stem->abbrev) == 0) {
			return stem->name;
		}
		stem++;
	}

	error("unrecognized stemmer: '%s'", val);
	return NULL;
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


static void add_terms(int (*add_term)(void *, const struct corpus_text *),
		      void *f, struct corpus_typemap *map, SEXP sterms)
{
	const struct corpus_text *terms;
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

		if (map) {
			TRY(corpus_typemap_set(map, &terms[i]));
			TRY(add_term(f, &map->type));
		} else {
			TRY(add_term(f, &terms[i]));
		}
	}
out:
	UNPROTECT(1);
	if (err && map) {
		corpus_typemap_destroy(map);
	}
	CHECK_ERROR(err);
}


static int add_stem_except(void *obj, const struct corpus_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_stem_except(f, x);
}


static int add_drop(void *obj, const struct corpus_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_drop(f, x);
}


static int add_drop_except(void *obj, const struct corpus_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_drop_except(f, x);
}


static int add_combine(void *obj, const struct corpus_text *x)
{
	struct corpus_filter *f = obj;
	return corpus_filter_combine(f, x);
}


struct corpus_filter *text_filter(SEXP x)
{
	SEXP handle, filter, combine;
	struct rcorpus_text *obj;
	struct corpus_typemap map;
	const char *stemmer;
	uint32_t connector;
	int err = 0, has_map = 0, nprot = 0, type_kind, flags, stem_dropped;

	handle = getListElement(x, "handle");
	obj = R_ExternalPtrAddr(handle);

	if (obj->has_filter) {
		if (obj->valid_filter && !obj->filter.error) {
			return &obj->filter;
		} else {
			corpus_filter_destroy(&obj->filter);
			obj->has_filter = 0;
		}
	}
	obj->valid_filter = 0;

	filter = getListElement(x, "filter");
	type_kind = filter_type_kind(filter);
	combine = getListElement(filter, "combine");
	connector = filter_connector(filter);
	stemmer = filter_stemmer(filter);
	flags = filter_flags(filter);
	stem_dropped = filter_logical(filter, "stem_dropped", 0);

	TRY(corpus_typemap_init(&map, type_kind));
	has_map = 1;

	if (stemmer) {
		if (!obj->has_snowball) {
			TRY(corpus_stem_snowball_init(&obj->snowball,
						      stemmer));
			obj->has_snowball = 1;
		}
		TRY(corpus_filter_init(&obj->filter, flags, type_kind,
				       connector, corpus_stem_snowball,
				       &obj->snowball));
	} else {
		TRY(corpus_filter_init(&obj->filter, flags, type_kind,
				       connector, NULL, NULL));
	}
	obj->has_filter = 1;

	if (!stem_dropped) {
		add_terms(add_stem_except, &obj->filter, &map,
			  getListElement(filter, "drop"));
	}
	add_terms(add_stem_except, &obj->filter, &map,
		  getListElement(filter, "stem_except"));
	add_terms(add_drop, &obj->filter, &map,
		  getListElement(filter, "drop"));
	add_terms(add_drop_except, &obj->filter, &map,
		  getListElement(filter, "drop_except"));
	add_terms(add_combine, &obj->filter, &map, combine);
out:
	if (has_map) {
		corpus_typemap_destroy(&map);
	}
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


static int add_suppress(void *obj, const struct corpus_text *x)
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

	add_terms(add_suppress, &obj->sentfilter, NULL, suppress);

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	obj->valid_sentfilter = 1;
	return &obj->sentfilter;
}
