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

#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "rcorpus.h"


int is_text_filter(SEXP filter)
{
	SEXP names;

	if (filter == R_NilValue) {
		return 1;
	}
	
	if (!inherits(filter, "text_filter")) {
		return 0;
	}

	if (!IS_VECTOR(filter)) {
		return 0;
	}

	names = getAttrib(filter, R_NamesSymbol);
	if (!IS_CHARACTER(names)) {
		return 0;
	}

	if (XLENGTH(names) != XLENGTH(filter)) {
		return 0;
	}

	return 1;
}


static SEXP lookup_text_filter(SEXP filter, const char *key)
{
	SEXP names;
	R_xlen_t i, n;

	if (!is_text_filter(filter)) {
		error("invalid text_filter object");
	}

	if (filter == R_NilValue) {
		return R_NilValue;
	}

	n = XLENGTH(filter);
	names = getAttrib(filter, R_NamesSymbol);

	for (i = 0; i < n; i++) {
		if(strcmp(CHAR(STRING_ELT(names, i)), key) == 0) {
			return VECTOR_ELT(filter, i);
		}
	}

	return R_NilValue;
}


static int text_filter_logical(SEXP filter, const char *key, int nullval)
{
	SEXP val = lookup_text_filter(filter, key);
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


int text_filter_drop_empty(SEXP filter)
{
	return text_filter_logical(filter, "drop_empty", 0);
}


int text_filter_type_kind(SEXP filter)
{
	int kind;

	if (filter == R_NilValue) {
		return 0;
	}

	kind = 0;

	if (text_filter_logical(filter, "fold_case", 0)) {
		kind |= TYPE_CASEFOLD;
	}
	if (text_filter_logical(filter, "fold_dash", 0)) {
		kind |= TYPE_DASHFOLD;
	}
	if (text_filter_logical(filter, "fold_quote", 0)) {
		kind |= TYPE_QUOTFOLD;
	}
	if (text_filter_logical(filter, "map_compatible", 0)) {
		kind |= TYPE_COMPAT;
	}
	if (text_filter_logical(filter, "remove_control", 0)) {
		kind |= TYPE_RMCC;
	}
	if (text_filter_logical(filter, "remove_ignorable", 0)) {
		kind |= TYPE_RMDI;
	}
	if (text_filter_logical(filter, "remove_whitespace", 0)) {
		kind |= TYPE_RMWS;
	}

	return kind;
}


const char *text_filter_stemmer(SEXP filter)
{
	SEXP alg = lookup_text_filter(filter, "stemmer");
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
