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
#include "rcorpus.h"


SEXP print_frame(SEXP sx, SEXP squote, SEXP sna_print, SEXP sprint_gap,
		 SEXP sright, SEXP smax)
{
	SEXP row_names, col_names;
	const uint8_t *na_print;
	int i, j, nrow, ncol;
	int quote, print_gap, right, max;
	int *colwidths;

	row_names = GetRowNames(sx);
	col_names = GetColNames(sx);
	nrow = nrows(sx);
	ncol = ncols(sx);

	quote = LOGICAL(squote)[0] == TRUE;
	na_print = (const uint8_t *)CHAR(STRING_ELT(sna_print, 0));
	print_gap = INTEGER(sprint_gap)[0];
	right = LOGICAL(sright)[0] == TRUE;
	max = INTEGER(smax)[0];
}
