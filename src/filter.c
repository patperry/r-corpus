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


static int text_filter_logical(SEXP filter, const char *key, int nullval)
{
	SEXP val = getListElement(filter, key);
	int ans;

	if (val == R_NilValue) {
		return nullval;
	}

	PROTECT(val = coerceVector(val, LGLSXP));

	if (XLENGTH(val) > 1) {
		error("invalid value for text_filter$%s", key);
	}

	if (XLENGTH(val) == 0 || INTEGER(val)[0] == NA_LOGICAL) {
		ans = nullval;
	} else {
		ans = INTEGER(val)[0] ? 1 : 0;
	}
	UNPROTECT(1);

	return ans;
}


static int text_filter_type_kind(SEXP filter)
{
	int kind;

	if (filter == R_NilValue) {
		return 0;
	}

	kind = 0;

	if (text_filter_logical(filter, "map_case", 0)) {
		kind |= CORPUS_TYPE_CASEFOLD;
	}
	if (text_filter_logical(filter, "map_compat", 0)) {
		kind |= CORPUS_TYPE_COMPAT;
	}
	if (text_filter_logical(filter, "map_dash", 0)) {
		kind |= CORPUS_TYPE_DASHFOLD;
	}
	if (text_filter_logical(filter, "map_quote", 0)) {
		kind |= CORPUS_TYPE_QUOTFOLD;
	}
	if (text_filter_logical(filter, "remove_control", 0)) {
		kind |= CORPUS_TYPE_RMCC;
	}
	if (text_filter_logical(filter, "remove_ignorable", 0)) {
		kind |= CORPUS_TYPE_RMDI;
	}
	if (text_filter_logical(filter, "remove_space", 0)) {
		kind |= CORPUS_TYPE_RMWS;
	}

	return kind;
}


static const char *text_filter_stemmer(SEXP filter)
{
	SEXP alg = getListElement(filter, "stemmer");
	SEXP val;

	if (alg == R_NilValue) {
		return NULL;
	}

	if (TYPEOF(alg) != STRSXP || XLENGTH(alg) != 1) {
		error("invalid text filter 'stemmer' value");
	}

	val = STRING_ELT(alg, 0);
	if (val == NA_STRING || XLENGTH(val) == 0) {
		return NULL;
	}

	return CHAR(val);
}


static int text_filter_flags(SEXP filter)
{
	int flags = 0;

	if (text_filter_logical(filter, "ignore_empty", 0)) {
		flags |= CORPUS_FILTER_IGNORE_EMPTY;
	}

	if (text_filter_logical(filter, "drop_symbol", 0)) {
		flags |= CORPUS_FILTER_DROP_SYMBOL;
	}

	if (text_filter_logical(filter, "drop_number", 0)) {
		flags |= CORPUS_FILTER_DROP_NUMBER;
	}

	if (text_filter_logical(filter, "drop_letter", 0)) {
		flags |= CORPUS_FILTER_DROP_LETTER;
	}

	if (text_filter_logical(filter, "drop_kana", 0)) {
		flags |= CORPUS_FILTER_DROP_KANA;
	}

	if (text_filter_logical(filter, "drop_ideo", 0)) {
		flags |= CORPUS_FILTER_DROP_IDEO;
	}

	return flags;
}


SEXP alloc_filter(SEXP props)
{
	SEXP sfilter;
	struct corpus_filter *f;
	const char *stemmer;
	int type_kind, flags;

	type_kind = text_filter_type_kind(props);
	stemmer = text_filter_stemmer(props);
	flags = text_filter_flags(props);

	f = filter_new(type_kind, stemmer, flags);
	PROTECT(sfilter = R_MakeExternalPtr(f, FILTER_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sfilter, free_filter, TRUE);

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
	if (!is_filter(sfilter))
		error("invalid 'filter' object");
	return R_ExternalPtrAddr(sfilter);
}
