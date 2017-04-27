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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "corpus/src/symtab.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/xalloc.h"
#include "rcorpus.h"


SEXP sentences_flat_text(SEXP sx)
{
	SEXP ans, handle, sources, pnames, psource, prow, pstart, pstop,
	     ptable, source, row, start, stop, index, pname, names;
	const struct text *text;
	struct text *sent, *sent1;
	R_xlen_t *parent, *parent1;
	struct sentscan scan;
	R_xlen_t i, src, n, isent, nsent, nsent_max;
	double r;
	int j, off, len, iname;

	PROTECT(sx = coerce_text(sx));
	text = as_text(sx, &n);
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");
	pstop = getListElement(ptable, "stop");

	pnames = names_text(sx);

	nsent = 0;
	nsent_max = 256;
	sent = xmalloc(nsent_max * sizeof(*sent));
	parent = xmalloc(nsent_max * sizeof(*parent));;
	if (sent == NULL || parent == NULL) {
		free(sent);
		free(parent);
		error("memory allocation failure");
	}

	for (i = 0; i < n; i++) {
		if (!text[i].ptr) {
			continue;
		}

		sentscan_make(&scan, &text[i]);
		while (sentscan_advance(&scan)) {
			if (nsent == nsent_max) {
				nsent_max = 2 * nsent_max;

				sent1 = xrealloc(sent,
						 nsent_max * sizeof(*sent));
				if (!sent1) {
					free(sent);
					free(parent);
					error("memory allocation failure");
				}
				sent = sent1;

				parent1 = xrealloc(parent,
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
	sent = xrealloc(sent, nsent * sizeof(*sent));
	parent = xrealloc(parent, nsent * sizeof(*parent));

	PROTECT(source = allocVector(INTSXP, nsent));
	PROTECT(row = allocVector(REALSXP, nsent));
	PROTECT(start = allocVector(INTSXP, nsent));
	PROTECT(stop = allocVector(INTSXP, nsent));
	PROTECT(index = allocVector(INTSXP, nsent));

	i = -1;
	j = 0;
	for (isent = 0; isent < nsent; isent++) {
		if (parent[isent] != i) {
			i = parent[isent];
			j = 0;
			src = INTEGER(psource)[i];
			r = REAL(prow)[i];
			off = INTEGER(pstart)[i];
		}
		len = (int)TEXT_SIZE(&sent[isent]);

		INTEGER(source)[isent] = src;
		REAL(row)[isent] = r;
		INTEGER(start)[isent] = off;
		INTEGER(stop)[isent] = off + (len - 1);
		INTEGER(index)[isent] = j + 1;
		j++;
		off += len;
	}

	PROTECT(ans = alloc_text(sources, source, row, start, stop));
	handle = getListElement(ans, "handle");
	R_SetExternalPtrAddr(handle, sent);

	i = -1;
	j = 0;
	if (pnames != R_NilValue) {
		PROTECT(names = allocVector(STRSXP, nsent));
		for (isent = 0; isent < nsent; isent++) {
			if (parent[isent] != i) {
				i = parent[isent];
				j = 0;
				pname = STRING_ELT(pnames, i);
			}
			// TODO concatenate pname + (j+1)
			SET_STRING_ELT(names, isent, pname);
			j++;
		}

		iname = findListElement(ans, "names");
		SET_VECTOR_ELT(ans, iname, names);

		UNPROTECT(1);
	}

	free(parent);

	UNPROTECT(7);
	return ans;
}
