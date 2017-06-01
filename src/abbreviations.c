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
#include "corpus/src/tree.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"


SEXP abbreviations(SEXP skind)
{
	SEXP ans;
	const char **strs;
	const char *kind;
	int i, n;

	if (skind == R_NilValue) {
		return R_NilValue;
	}

        PROTECT(skind = coerceVector(skind, STRSXP));
	if (STRING_ELT(skind, 0) == NA_STRING) {
		UNPROTECT(1);
		return R_NilValue;
	}

	kind = translateCharUTF8(STRING_ELT(skind, 0));
	strs = (const char **)corpus_sentsuppress_list(kind, &n);

	if (!strs) {
		error("unknown abbreviations kind: '%s'", kind);
	}

	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
		SET_STRING_ELT(ans, i, mkCharCE(strs[i], CE_UTF8));
	}

	UNPROTECT(2);
	return ans;
}
