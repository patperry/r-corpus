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

#include <string.h>
#include "rcorpus.h"


SEXP char_bytes(SEXP x)
{
	SEXP ans, str, buf;
	R_xlen_t i, n, len;

	if (x == R_NilValue) {
		return R_NilValue;
	}

	n = XLENGTH(x);
	PROTECT(ans = allocVector(VECSXP, n));
	setAttrib(ans, R_NamesSymbol, getAttrib(x, R_NamesSymbol));

	for (i = 0; i < n; i++) {
		str = STRING_ELT(x, i);
		if (str == NA_STRING) {
			SET_VECTOR_ELT(ans, i, R_NilValue);
			continue;
		}

		len = LENGTH(str);
		buf = allocVector(RAWSXP, len);
		memcpy(RAW(buf), CHAR(str), len);

		SET_VECTOR_ELT(ans, i, buf);
	}

	UNPROTECT(1);
	return ans;
}
