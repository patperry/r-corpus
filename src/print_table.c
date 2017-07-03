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

#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "corpus/src/array.h"
#include "corpus/src/unicode.h"
#include "rcorpus.h"


#define NEEDS(n) \
	do { \
		if ((n) && (nbuf > nbuf_max - (n))) { \
			nbuf_max0 = nbuf_max; \
			corpus_array_size_add(&nbuf_max, 1, nbuf, (n)); \
			buf = S_realloc(buf, nbuf_max, nbuf_max0, 1); \
		} \
	} while (0)

#define PRINT_SPACES(n) \
	do { \
		NEEDS(n); \
		if ((n) > 0) { \
			memset(buf + nbuf, ' ', (n)); \
			nbuf += (n); \
		} \
	} while (0)

#define PRINT_CHAR(ch) \
	do { \
		NEEDS(1); \
		buf[nbuf] = (ch); \
		nbuf++; \
	} while (0)

#define PRINT_STRING(str, n) \
	do { \
		NEEDS(n); \
		memcpy(buf + nbuf, str, (n)); \
		nbuf += (n); \
	} while (0)

#define PRINT_NOQUOTE(str, n, pad) \
	do { \
		if (right) PRINT_SPACES(pad); \
		PRINT_STRING(str, n); \
		if (!right) PRINT_SPACES(pad); \
	} while (0)

#define PRINT_QUOTE(str, n, pad) \
	do { \
		if (right) PRINT_SPACES(pad); \
		PRINT_CHAR('"'); \
		PRINT_STRING(str, n); \
		PRINT_CHAR('"'); \
		if (!right) PRINT_SPACES(pad); \
	} while (0)

#define PRINT_ENTRY(str, n, pad) \
	do { \
		if (quote) { \
			PRINT_QUOTE(str, n, pad); \
		} else { \
			PRINT_NOQUOTE(str, n, pad); \
		} \
	} while (0)

#define FLUSH() \
	do { \
		PRINT_CHAR('\0'); \
		Rprintf("%s", buf); \
		nbuf = 0; \
	} while (0)

static void print_range(SEXP sx, int begin, int end, int quote,
			const char *na_print, int na_width, int print_gap,
			int right, int namewidth, const int *colwidths)
{
	SEXP elt, name, dim_names, row_names, col_names;
	R_xlen_t ix;
	const char *str;
	char *buf;
	int nbuf, nbuf_max, nbuf_max0;
	int i, j, nrow, n, w, width, na_size;

	dim_names = getAttrib(sx, R_DimNamesSymbol);
	row_names = VECTOR_ELT(dim_names, 0);
	col_names = VECTOR_ELT(dim_names, 1);
	nrow = nrows(sx);

	na_size = strlen(na_print);

	nbuf = 0;
	nbuf_max = 128;
	buf = R_alloc(nbuf_max + 1, 1);

	if (col_names != R_NilValue) {
		PRINT_SPACES(namewidth);

		for (j = begin; j < end; j++) {
			name = STRING_ELT(col_names, j);
			if (name == NA_STRING) {
				str = "NA";
				w = 2;
				n = 2;
			} else {
				str = translateChar(name);
				w = charsxp_width(name);
				n = strlen(str);
			}
			PRINT_SPACES(print_gap);
			PRINT_NOQUOTE(str, n, colwidths[j] - w);
		}
		PRINT_CHAR('\n');
		FLUSH();
	}

	for (i = 0; i < nrow; i++) {
		if (row_names == R_NilValue) {
			NEEDS(namewidth + 1); // +1, sprints adds NUL
			w = sprintf(buf + nbuf, "%d", i + 1);
			nbuf += w;
			PRINT_SPACES(namewidth - w);
		} else {
			name = STRING_ELT(row_names, i);
			if (name == NA_STRING) {
				str = "NA";
				w = 2;
				n = 2;
			} else {
				str = translateChar(name);
				w = charsxp_width(name);
				n = strlen(str);
			}

			PRINT_STRING(str, n);
			PRINT_SPACES(namewidth - w);
		}

		for (j = begin; j < end; j++) {
			width = colwidths[j];
			ix = (R_xlen_t)i + (R_xlen_t)j * (R_xlen_t)nrow;
			elt = STRING_ELT(sx, ix);

			PRINT_SPACES(print_gap);

			if (elt == NA_STRING) {
				PRINT_NOQUOTE(na_print, na_size,
					      width - na_width);
			} else {
				str = translateChar(elt);
				w = charsxp_width(elt) + (quote ? 2 : 0);
				n = strlen(str);
				PRINT_ENTRY(str, n, width - w);
			}
		}

		PRINT_CHAR('\n');
		FLUSH();
	}
}


SEXP print_table(SEXP sx, SEXP squote, SEXP sna_print, SEXP sprint_gap,
		 SEXP sright, SEXP swidth)
{
	SEXP elt, dim_names, row_names, col_names;
	const char *na_print;
	R_xlen_t ix, nx;
	int i, j, nrow, ncol;
	int quote, na_width, print_gap, right, width;
	int begin, end, w, linewidth, namewidth, *colwidths;

	dim_names = getAttrib(sx, R_DimNamesSymbol);
	row_names = VECTOR_ELT(dim_names, 0);
	col_names = VECTOR_ELT(dim_names, 1);
	nrow = nrows(sx);
	ncol = ncols(sx);
	nx = XLENGTH(sx);

	quote = (LOGICAL(squote)[0] == TRUE) ? 1 : 0;
	sna_print = STRING_ELT(sna_print, 0);
	na_print = translateChar(sna_print);
	na_width = charsxp_width(sna_print);
	print_gap = INTEGER(sprint_gap)[0];
	right = LOGICAL(sright)[0] == TRUE;
	width = INTEGER(swidth)[0];

	namewidth = 0;
	if (row_names == R_NilValue) {
		if (nrow == 0) {
			namewidth = 0;
		} else {
			namewidth = (int)floor(log10((double)nrow)) + 1;
		}
	} else {
		for (i = 0; i < nrow; i++) {
			elt = STRING_ELT(row_names, i);
			w = (elt == NA_STRING) ? 2 : charsxp_width(elt);
			if (w > namewidth) {
				namewidth = w;
			}
		}
	}

	colwidths = (void *)R_alloc(ncol, sizeof(*colwidths));
	memset(colwidths, 0, ncol * sizeof(*colwidths));
	if (col_names != R_NilValue) {
		for (j = 0; j < ncol; j++) {
			elt = STRING_ELT(col_names, j);
			if (elt == NA_STRING) {
				colwidths[j] = 2;
			} else {
				colwidths[j] = charsxp_width(elt);
			}
		}
	}

	j = 0;
	for (ix = 0; ix < nx; ix++) {
		elt = STRING_ELT(sx, ix);
		if (elt == NA_STRING) {
			w = na_width;
		} else {
			w = charsxp_width(elt) + (quote ? 2 : 0);
		}

		if (w > colwidths[j]) {
			colwidths[j] = w;
		}

		if ((ix + 1) % nrow == 0) {
			j++;
		}
	}

	begin = 0;
	while (begin != ncol) {
		linewidth = namewidth;
		end = begin;

		while (end != ncol) {
			// break if including the column puts us over the
			// width; we do the calculations like this to
			// avoid integer overflow
			if (linewidth >= width - print_gap) {
				break;
			}
			linewidth += print_gap;

			if (linewidth >= width - colwidths[end]) {
				break;
			}
			linewidth += colwidths[end];

			end++;
		}

		if (begin == end) {
			// include at least one column, even if it
			// puts us over the width
			end++;
		}

		print_range(sx, begin, end, quote, na_print, na_width,
			    print_gap, right, namewidth, colwidths);

		begin = end;
	}

	return R_NilValue;
}
