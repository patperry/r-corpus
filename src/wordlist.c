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


static SEXP wordlist(const uint8_t **(*callback)(const char *, int *),
		     SEXP skind)
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

	// assume utf8 encoding
	kind = CHAR(STRING_ELT(skind, 0));
	strs = (const char **)callback(kind, &n);

	if (!strs) {
		error("unknown kind (\"%s\")", kind);
	}

	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		SET_STRING_ELT(ans, i, mkCharCE(strs[i], CE_UTF8));
	}

	UNPROTECT(2);
	return ans;
}


SEXP abbreviations(SEXP skind)
{
	return wordlist(corpus_sentsuppress_list, skind);
}


SEXP stopwords(SEXP skind)
{
	return wordlist(corpus_stopword_list, skind);
}
