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

#include <stdlib.h>
#include <Rdefines.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "rcorpus.h"


#define FILTER_TAG install("corpus::filter")


static struct corpus_filter *filter_new(int type_kind, const char *stemmer,
					int flags)
{
	struct corpus_filter *obj = NULL;
	struct corpus_filter f;

	if (corpus_filter_init(&f, type_kind, stemmer, flags) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			corpus_filter_destroy(&f);
			error("failed allocating memory (%u bytes)",
			      (unsigned)sizeof(*obj));
		}
		*obj = f;
	} else {
		error("failed creating new filter object");
	}

	return obj;
}


static void filter_free(struct corpus_filter *f)
{
	if (f) {
		corpus_filter_destroy(f);
		free(f);
	}
}


static void free_filter(SEXP sfilter)
{
        struct corpus_filter *f = R_ExternalPtrAddr(sfilter);
	filter_free(f);
}


static int token_filter_logical(SEXP filter, const char *key, int nullval)
{
	SEXP val = getListElement(filter, key);
	int ans;

	if (val == R_NilValue) {
		return nullval;
	}

	PROTECT(val = coerceVector(val, LGLSXP));

	if (XLENGTH(val) > 1) {
		error("invalid token filter '%s' value", key);
	}

	if (XLENGTH(val) == 0 || INTEGER(val)[0] == NA_LOGICAL) {
		ans = nullval;
	} else {
		ans = INTEGER(val)[0] ? 1 : 0;
	}
	UNPROTECT(1);

	return ans;
}


static int token_filter_type_kind(SEXP filter)
{
	int kind;

	if (filter == R_NilValue) {
		return 0;
	}

	kind = 0;

	if (token_filter_logical(filter, "map_case", 0)) {
		kind |= CORPUS_TYPE_MAPCASE;
	}
	if (token_filter_logical(filter, "map_compat", 0)) {
		kind |= CORPUS_TYPE_MAPCOMPAT;
	}
	if (token_filter_logical(filter, "map_quote", 0)) {
		kind |= CORPUS_TYPE_MAPQUOTE;
	}
	if (token_filter_logical(filter, "remove_ignorable", 0)) {
		kind |= CORPUS_TYPE_RMDI;
	}

	return kind;
}


static const char *token_filter_stemmer(SEXP filter)
{
	SEXP alg = getListElement(filter, "stemmer");
	SEXP val;

	if (alg == R_NilValue) {
		return NULL;
	}

	if (TYPEOF(alg) != STRSXP || XLENGTH(alg) != 1) {
		error("invalid token filter 'stemmer' value");
	}
	val = STRING_ELT(alg, 0);

	return CHAR(val);
}


static int token_filter_flags(SEXP filter)
{
	int flags = CORPUS_FILTER_IGNORE_SPACE;

	if (token_filter_logical(filter, "drop_letter", 0)) {
		flags |= CORPUS_FILTER_DROP_LETTER;
	}

	if (token_filter_logical(filter, "drop_mark", 0)) {
		flags |= CORPUS_FILTER_DROP_MARK;
	}

	if (token_filter_logical(filter, "drop_number", 0)) {
		flags |= CORPUS_FILTER_DROP_NUMBER;
	}

	if (token_filter_logical(filter, "drop_punct", 0)) {
		flags |= CORPUS_FILTER_DROP_PUNCT;
	}

	if (token_filter_logical(filter, "drop_symbol", 0)) {
		flags |= CORPUS_FILTER_DROP_SYMBOL;
	}

	if (token_filter_logical(filter, "drop_other", 0)) {
		flags |= CORPUS_FILTER_DROP_OTHER;
	}

	return flags;
}


static void add_terms(const char *name,
		      int (*add_term)(struct corpus_filter *,
			              const struct corpus_text *),
		      struct corpus_filter *f,
		      struct corpus_typemap *map,
		      SEXP sterms)
{
	const struct corpus_text *terms;
	R_xlen_t i, n;
	int err;

	if (sterms == R_NilValue) {
		return;
	}

	PROTECT(sterms = coerce_text(sterms));
	terms = as_text(sterms, &n);

	for (i = 0; i < n; i++) {
		if (!terms[i].ptr) {
			continue;
		}

		if ((err = corpus_typemap_set(map, &terms[i]))) {
			goto out;
		}

		if ((err = add_term(f, &map->type))) {
			goto out;
		}
	}
out:
	if (err) {
		corpus_typemap_destroy(map);
		error("failed adding term to token filter %s list", name);
	}

	UNPROTECT(1);

}


SEXP alloc_filter(SEXP props)
{
	SEXP sfilter;
	struct corpus_filter *f;
	struct corpus_typemap map;
	const char *stemmer;
	int err, type_kind, flags, stem_dropped;

	type_kind = token_filter_type_kind(props);
	stemmer = token_filter_stemmer(props);
	flags = token_filter_flags(props);
	stem_dropped = token_filter_logical(props, "stem_dropped", 0);

	f = filter_new(type_kind, stemmer, flags);
	PROTECT(sfilter = R_MakeExternalPtr(f, FILTER_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sfilter, free_filter, TRUE);

	if ((err = corpus_typemap_init(&map, type_kind, NULL))) {
		error("memory allocation failure");
	}

	if (!stem_dropped) {
		add_terms("stem exception", corpus_filter_stem_except, f,
			  &map, getListElement(props, "drop"));
	}

	add_terms("stem exception", corpus_filter_stem_except, f, &map,
		  getListElement(props, "stem_except"));

	add_terms("drop", corpus_filter_drop, f, &map,
		  getListElement(props, "drop"));

	add_terms("drop exception", corpus_filter_drop_except, f, &map,
		  getListElement(props, "drop_except"));

	add_terms("combine", corpus_filter_combine, f, &map,
		  getListElement(props, "combine"));

	corpus_typemap_destroy(&map);

	UNPROTECT(1);
	return sfilter;
}


int is_filter(SEXP sfilter)
{
	return ((TYPEOF(sfilter) == EXTPTRSXP)
		&& (R_ExternalPtrTag(sfilter) == FILTER_TAG));
}


struct corpus_filter *as_filter(SEXP sfilter)
{
	if (!is_filter(sfilter)) {
		error("invalid 'filter' object");
	}
	return R_ExternalPtrAddr(sfilter);
}
