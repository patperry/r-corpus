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
#include "corpus/src/text.h"
#include "corpus/src/tree.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"

// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_sentfilter
#ifdef error
#  undef error
#endif



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
		Rf_error("unknown abbreviations kind: '%s'", kind);
	}

	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
		SET_STRING_ELT(ans, i, mkCharCE(strs[i], CE_UTF8));
	}

	UNPROTECT(2);
	return ans;
}

#define BAIL(msg) \
	do { \
		free(block); \
		free(parent); \
		corpus_sentfilter_destroy(&filter); \
		Rf_error(msg); \
	} while (0)


SEXP text_split_sentences(SEXP sx, SEXP ssize, SEXP scrlf_break, SEXP ssuppress)
{
	SEXP ans, handle, sources, psource, prow, pstart,
	     ptable, source, row, start, stop, index, sparent, stext, names,
	     sclass, row_names;
	const struct corpus_text *text, *suppress;
	struct corpus_text *block, *block1;
	R_xlen_t *parent, *parent1;
	struct corpus_sentfilter filter;
	R_xlen_t src, i, n, isupp, nsupp, iblock, nblock, nblock_max;
	double r, size;
	int j, flags, off, len, nprot, err;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");

	// size
        PROTECT(ssize = coerceVector(ssize, REALSXP)); nprot++;
	size = REAL(ssize)[0];
	if (!(size >= 1)) {
		size = 1;
	}

	// crlf_break
	PROTECT(scrlf_break = coerceVector(scrlf_break, LGLSXP)); nprot++;
	if (LOGICAL(scrlf_break)[0] == TRUE) {
		flags = CORPUS_SENTSCAN_STRICT;
	} else {
		flags = CORPUS_SENTSCAN_SPCRLF;
	}

	if ((err = corpus_sentfilter_init(&filter, flags))) {
		Rf_error("memory allocation failure");
	}

	// suppress
	if (ssuppress != R_NilValue) {
		PROTECT(ssuppress = coerce_text(ssuppress)); nprot++;
		suppress = as_text(ssuppress, &nsupp);

		for (isupp = 0; isupp < nsupp; isupp++) {
			if (!suppress[isupp].ptr) {
				continue;
			}

			err = corpus_sentfilter_suppress(&filter,
							 &suppress[isupp]);
			if (err) {
				corpus_sentfilter_destroy(&filter);
				Rf_error("failed adding break suppression"
					 " to sentence filter");
			}
		}
	}

	nblock = 0;
	nblock_max = 256;
	block = malloc(nblock_max * sizeof(*block));
	parent = malloc(nblock_max * sizeof(*parent));;
	if (block == NULL || parent == NULL) {
		BAIL("memory allocation failure");
	}

	for (i = 0; i < n; i++) {
		if (!text[i].ptr) { // missing value
			continue;
		}

		if ((err = corpus_sentfilter_start(&filter, &text[i]))) {
			BAIL("memory allocation failure");
		}

		while (corpus_sentfilter_advance(&filter)) {
			if (nblock == nblock_max) {
				nblock_max = 2 * nblock_max;

				block1 = realloc(block,
						nblock_max * sizeof(*block));
				if (!block1) {
					BAIL("memory allocation failure");
				}
				block = block1;

				parent1 = realloc(parent,
						  nblock_max * sizeof(*parent));
				if (!parent1) {
					BAIL("memory allocation failure");
				}
				parent = parent1;
			}

			block[nblock] = filter.current;
			parent[nblock] = i;
			nblock++;
		}

		if (filter.error) {
			BAIL("memory allocation failure");
		}
	}

	// free excess memory
	block = realloc(block, nblock * sizeof(*block));
	parent = realloc(parent, nblock * sizeof(*parent));

	PROTECT(source = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(row = allocVector(REALSXP, nblock)); nprot++;
	PROTECT(start = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(stop = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(sparent = allocVector(REALSXP, nblock)); nprot++;
	PROTECT(index = allocVector(INTSXP, nblock)); nprot++;

	i = -1;
	j = 0;
	off = 0;
	r = NA_REAL;
	src = NA_INTEGER;

	for (iblock = 0; iblock < nblock; iblock++) {
		if (parent[iblock] != i) {
			i = parent[iblock];
			j = 0;
			src = INTEGER(psource)[i];
			r = REAL(prow)[i];
			off = INTEGER(pstart)[i];
		}
		len = (int)CORPUS_TEXT_SIZE(&block[iblock]);

		INTEGER(source)[iblock] = src;
		REAL(row)[iblock] = r;
		INTEGER(start)[iblock] = off;
		INTEGER(stop)[iblock] = off + (len - 1);
		INTEGER(index)[iblock] = j + 1;
		REAL(sparent)[iblock] = (double)i + 1;

		j++;
		off += len;
	}
	free(parent);
	corpus_sentfilter_destroy(&filter);

	PROTECT(stext = alloc_text(sources, source, row, start, stop)); nprot++;

	handle = getListElement(stext, "handle");
	R_SetExternalPtrAddr(handle, block);

	PROTECT(ans = allocVector(VECSXP, 3)); nprot++;
	SET_VECTOR_ELT(ans, 0, sparent);
	SET_VECTOR_ELT(ans, 1, index);
	SET_VECTOR_ELT(ans, 2, stext);

	PROTECT(names = allocVector(STRSXP, 3)); nprot++;
	SET_STRING_ELT(names, 0, mkChar("parent"));
	SET_STRING_ELT(names, 1, mkChar("index"));
	SET_STRING_ELT(names, 2, mkChar("text"));
	setAttrib(ans, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2)); nprot++;
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)nblock;
	setAttrib(ans, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 1)); nprot++;
        SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(nprot);
	return ans;
}


SEXP text_split_tokens(SEXP sx, SEXP ssize, SEXP sfilter)
{
	(void)sx;
	(void)ssize;
	(void)sfilter;
	return R_NilValue;
}
