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

#include <stdint.h>
#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "rcorpus.h"


SEXP stopwords(SEXP skind)
{
	SEXP ans;
	const char **words;
	const char *kind;
	int i, n;

        PROTECT(skind = coerceVector(skind, STRSXP));

	if (STRING_ELT(skind, 0) == NA_STRING) {
		UNPROTECT(1);
		return R_NilValue;
	}

	kind = translateCharUTF8(STRING_ELT(skind, 0));
	words = (const char **)corpus_stopwords(kind, &n);

	if (!words) {
		error("unknown stopwords kind: '%s'", kind);
	}

	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
		SET_STRING_ELT(ans, i, mkCharCE(words[i], CE_UTF8));
	}

	UNPROTECT(2);
	return ans;
}
