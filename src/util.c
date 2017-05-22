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
#include <Rdefines.h>
#include "rcorpus.h"


/* based on R-Exts Section 5.9.6 "handling lists" */
int findListElement(SEXP list, const char *str)
{
	SEXP names;
	int i, n;

	if (list == R_NilValue) {
		return -1;
	}

	names = getAttrib(list, R_NamesSymbol);
	if (names == R_NilValue) {
		return -1;
	}

	n = LENGTH(list);
	for (i = 0; i < n; i++) {
		if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
			return i;
		}
	}
	return -1;
}


SEXP getListElement(SEXP list, const char *str)
{
	int i = findListElement(list, str);
	if (i < 0) {
		return R_NilValue;
	}
	return VECTOR_ELT(list, i);
}
