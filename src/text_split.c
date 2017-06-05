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

#include <stddef.h>
#include <stdlib.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"

// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_sentfilter
#ifdef error
#  undef error
#endif


static SEXP make_blocks(SEXP sx, struct corpus_text *block, R_xlen_t *parent,
			R_xlen_t nblock);


#define BAIL(msg) \
	do { \
		free(block); \
		free(parent); \
		corpus_sentfilter_destroy(&filter); \
		Rf_error(msg); \
	} while (0)


#define GROW_BLOCK_ARRAYS() \
	do { \
		if (nblock == nblock_max) { \
			nblock_max = 2 * nblock_max; \
			block1 = realloc(block, \
					 nblock_max * sizeof(*block)); \
			if (!block1) { \
				BAIL("memory allocation failure"); \
			} \
			block = block1; \
			parent1 = realloc(parent, \
					  nblock_max * sizeof(*parent)); \
			if (!parent1) { \
				BAIL("memory allocation failure"); \
			} \
			parent = parent1; \
		} \
	} while (0)


SEXP text_split_sentences(SEXP sx, SEXP ssize, SEXP scrlf_break, SEXP ssuppress)
{
	SEXP ans;
	struct corpus_sentfilter filter;
	const struct corpus_text *text, *suppress;
	struct corpus_text *block, *block1, current;
	R_xlen_t *parent, *parent1;
	R_xlen_t i, n, isupp, nsupp, nblock, nblock_max;
	size_t attr, size;
	double s, block_size;
	int flags, nprot, err;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	// size
        PROTECT(ssize = coerceVector(ssize, REALSXP)); nprot++;
	block_size = REAL(ssize)[0];
	if (!(block_size >= 1)) {
		block_size = 1;
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

		if (CORPUS_TEXT_SIZE(&text[i]) == 0) { // empty text
			GROW_BLOCK_ARRAYS();

			block[nblock] = text[i];
			parent[nblock] = i;
			nblock++;
			continue;
		}

		if ((err = corpus_sentfilter_start(&filter, &text[i]))) {
			BAIL("memory allocation failure");
		}

		s = 0;

		while (corpus_sentfilter_advance(&filter)) {
			if (s == 0) {
				current.ptr = filter.current.ptr;
				attr = 0;
				size = 0;
			}

			size += CORPUS_TEXT_SIZE(&filter.current);
			attr |= CORPUS_TEXT_BITS(&filter.current);
			s++;

			if (s < block_size) {
				continue;
			}

			GROW_BLOCK_ARRAYS();

			current.attr = attr | size;
			block[nblock] = current;
			parent[nblock] = i;
			nblock++;

			s = 0;
		}

		if (filter.error) {
			BAIL("memory allocation failure");
		}

		if (s > 0) {
			GROW_BLOCK_ARRAYS();

			current.attr = attr | size;
			block[nblock] = current;
			parent[nblock] = i;
			nblock++;
		}
	}

	// free excess memory
	block = realloc(block, nblock * sizeof(*block));
	parent = realloc(parent, nblock * sizeof(*parent));
	corpus_sentfilter_destroy(&filter);

	PROTECT(ans = make_blocks(sx, block, parent, nblock)); nprot++;
	UNPROTECT(nprot);
	return ans;
}


#undef BAIL
#define BAIL(msg) \
	do { \
		free(block); \
		free(parent); \
		Rf_error(msg); \
	} while (0)


SEXP text_split_tokens(SEXP sx, SEXP ssize, SEXP sfilter)
{
	SEXP ans;
	struct corpus_filter *filter;
	const struct corpus_text *text;
	struct corpus_text *block, *block1, current;
	R_xlen_t *parent, *parent1;
	R_xlen_t i, n, nblock, nblock_max;
	size_t attr, size;
	double s, block_size;
	int nprot, err;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	// size
        PROTECT(ssize = coerceVector(ssize, REALSXP)); nprot++;
	block_size = REAL(ssize)[0];
	if (!(block_size >= 1)) {
		block_size = 1;
	}

	// filter
	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

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

		if (CORPUS_TEXT_SIZE(&text[i]) == 0) { // empty text
			GROW_BLOCK_ARRAYS();

			block[nblock] = text[i];
			parent[nblock] = i;
			nblock++;
			continue;
		}

		if ((err = corpus_filter_start(filter, &text[i],
					       CORPUS_FILTER_SCAN_TOKENS))) {
			BAIL("memory allocation failure");
		}

		// start with an empty block
		s = 0;
		size = 0;

		while (corpus_filter_advance(filter)) {
			// if we encounter a non-dropped, non-ignored
			// token and the block is already full, add it
			if (filter->type_id >= 0 && s == block_size) {
				GROW_BLOCK_ARRAYS();

				current.attr = attr | size;
				block[nblock] = current;
				parent[nblock] = i;
				nblock++;
				size = 0;
				s = 0;
			}

			// if the block is empty, initialize it
			if (size == 0) {
				current.ptr = filter->current.ptr;
				size = 0;
				attr = 0;
			}

			// update the block size and attributes
			size += CORPUS_TEXT_SIZE(&filter->current);
			attr |= CORPUS_TEXT_BITS(&filter->current);

			// possibly update the non-dropped token count
			if (filter->type_id >= 0) {
				s++;
			}
		}

		if (filter->error) {
			BAIL("memory allocation failure");
		}

		if (size > 0) {
			GROW_BLOCK_ARRAYS();

			current.attr = attr | size;
			block[nblock] = current;
			parent[nblock] = i;
			nblock++;
		}
	}

	// free excess memory
	block = realloc(block, nblock * sizeof(*block));
	parent = realloc(parent, nblock * sizeof(*parent));

	PROTECT(ans = make_blocks(sx, block, parent, nblock)); nprot++;
	UNPROTECT(nprot);
	return ans;
}


SEXP make_blocks(SEXP sx, struct corpus_text *block, R_xlen_t *parent,
		 R_xlen_t nblock)
{
	SEXP ans, handle, sources, psource, prow, pstart,
	     ptable, source, row, start, stop, index, sparent, stext, names,
	     sclass, row_names;
	R_xlen_t src, i, iblock;
	double r;
	int j, off, len, nprot;

	nprot = 0;

	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");

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
