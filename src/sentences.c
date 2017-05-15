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
#include "corpus/src/sentscan.h"
#include "rcorpus.h"


SEXP sentences_text(SEXP sx)
{
	SEXP ans, handle, sources, psource, prow, pstart,
	     ptable, source, row, start, stop, index, sparent, stext, names,
	     sclass, row_names;
	const struct corpus_text *text;
	struct corpus_text *sent, *sent1;
	R_xlen_t *parent, *parent1;
	struct corpus_sentscan scan;
	R_xlen_t i, src, n, isent, nsent, nsent_max;
	double r;
	int j, off, len;

	PROTECT(sx = coerce_text(sx));
	text = as_text(sx, &n);
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");

	nsent = 0;
	nsent_max = 256;
	sent = malloc(nsent_max * sizeof(*sent));
	parent = malloc(nsent_max * sizeof(*parent));;
	if (sent == NULL || parent == NULL) {
		free(sent);
		free(parent);
		error("memory allocation failure");
	}

	for (i = 0; i < n; i++) {
		if (!text[i].ptr) { // missing value
			continue;
		}

		corpus_sentscan_make(&scan, &text[i]);
		while (corpus_sentscan_advance(&scan)) {
			if (nsent == nsent_max) {
				nsent_max = 2 * nsent_max;

				sent1 = realloc(sent,
						nsent_max * sizeof(*sent));
				if (!sent1) {
					free(sent);
					free(parent);
					error("memory allocation failure");
				}
				sent = sent1;

				parent1 = realloc(parent,
						  nsent_max * sizeof(*parent));
				if (!parent1) {
					free(sent);
					free(parent);
					error("memory allocation failure");
				}
				parent = parent1;
			}

			sent[nsent] = scan.current;
			parent[nsent] = i;
			nsent++;
		}
	}

	// free excess memory
	sent = realloc(sent, nsent * sizeof(*sent));
	parent = realloc(parent, nsent * sizeof(*parent));

	PROTECT(source = allocVector(INTSXP, nsent));
	PROTECT(row = allocVector(REALSXP, nsent));
	PROTECT(start = allocVector(INTSXP, nsent));
	PROTECT(stop = allocVector(INTSXP, nsent));
	PROTECT(sparent = allocVector(REALSXP, nsent));
	PROTECT(index = allocVector(INTSXP, nsent));

	i = -1;
	j = 0;
	off = 0;
	r = NA_REAL;
	src = NA_INTEGER;

	for (isent = 0; isent < nsent; isent++) {
		if (parent[isent] != i) {
			i = parent[isent];
			j = 0;
			src = INTEGER(psource)[i];
			r = REAL(prow)[i];
			off = INTEGER(pstart)[i];
		}
		len = (int)CORPUS_TEXT_SIZE(&sent[isent]);

		INTEGER(source)[isent] = src;
		REAL(row)[isent] = r;
		INTEGER(start)[isent] = off;
		INTEGER(stop)[isent] = off + (len - 1);
		INTEGER(index)[isent] = j + 1;
		REAL(sparent)[isent] = (double)i + 1;

		j++;
		off += len;
	}
	free(parent);

	PROTECT(stext = alloc_text(sources, source, row, start, stop));
	handle = getListElement(stext, "handle");
	R_SetExternalPtrAddr(handle, sent);

	PROTECT(ans = allocVector(VECSXP, 3));
	SET_VECTOR_ELT(ans, 0, sparent);
	SET_VECTOR_ELT(ans, 1, index);
	SET_VECTOR_ELT(ans, 2, stext);

	PROTECT(names = allocVector(STRSXP, 3));
	SET_STRING_ELT(names, 0, mkChar("parent"));
	SET_STRING_ELT(names, 1, mkChar("index"));
	SET_STRING_ELT(names, 2, mkChar("text"));
	setAttrib(ans, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2));
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)nsent;
	setAttrib(ans, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 1));
        SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(12);
	return ans;
}
