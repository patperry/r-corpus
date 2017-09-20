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

#include <inttypes.h>
#include <stdint.h>
#include <string.h>
#include <Rdefines.h>
#include "rcorpus.h"


/* based on R-Exts Section 5.9.6 "handling lists" */
int findListElement(SEXP list, const char *str)
{
	SEXP names;
	int i, n;
	int nprot = 0;
	int ans = -1;

	if (list == R_NilValue) {
		goto out;
	}

	PROTECT(names = getAttrib(list, R_NamesSymbol)); nprot++;
	if (names == R_NilValue) {
		goto out;
	}

	n = LENGTH(list);
	for (i = 0; i < n; i++) {
		if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
			ans = i;
			goto out;
		}
	}
out:
	UNPROTECT(nprot);
	return ans;
}


SEXP getListElement(SEXP list, const char *str)
{
	int i = findListElement(list, str);
	if (i < 0) {
		return R_NilValue;
	}
	return VECTOR_ELT(list, i);
}


double *as_weights(SEXP sweights, R_xlen_t n)
{
	R_xlen_t n0;

	if (sweights == R_NilValue) {
		return NULL;
	}

	n0 = XLENGTH(sweights);
	if (n0 != n) {
		error("invalid 'weights' vector;"
		      " length is %"PRIu64" but should be %"PRIu64,
		      (uint64_t)n0, (uint64_t)n);
	}

	return REAL(sweights);
}
