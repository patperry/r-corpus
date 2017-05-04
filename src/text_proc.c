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
#include "corpus/src/wordscan.h"
#include "corpus/src/xalloc.h"
#include "rcorpus.h"


SEXP sentences_text(SEXP sx)
{
	SEXP ans, handle, sources, pnames, psource, prow, pstart, pstop,
	     ptable, source, row, start, stop, index, sparent, stext, names,
	     sclass, row_names;
	const struct text *text;
	struct text *sent, *sent1;
	R_xlen_t *parent, *parent1;
	struct sentscan scan;
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
		if (!text[i].ptr) { // missing value
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
	PROTECT(sparent = allocVector(REALSXP, nsent));
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


SEXP tokens_text(SEXP sx, SEXP sfilter)
{
	SEXP ans, ans_i, names, stext;
	SEXP *types;
	const struct text *text, *type;
	struct text empty;
	struct wordscan scan;
	struct symtab symtab;
	R_xlen_t i, j, n, nbuf, nbuf_max;
	int *buf, kind, token_id, type_id, nadd, ntype, ntype_max;
	int drop_empty;

	PROTECT(stext = coerce_text(sx));
	text = as_text(stext, &n);

	kind = text_filter_type_kind(sfilter);
	drop_empty = text_filter_drop_empty(sfilter);

	PROTECT(ans = allocVector(VECSXP, n));
	names = names_text(stext);
	setAttrib(ans, R_NamesSymbol, names);

	if (symtab_init(&symtab, kind, NULL) != 0) {
		error("memory allocation failure");
	}

	nbuf_max = 256;
	nbuf = 0;
	buf = (void *)R_alloc(nbuf_max, sizeof(*buf));

	ntype_max = 256;
	ntype = 0;
	types = (void *)R_alloc(ntype_max, sizeof(*types));

	// add the empty type, and protect it
	empty.ptr = NULL;
	empty.attr = 0;
	if (symtab_add_type(&symtab, &empty, &type_id) != 0) {
		error("memory allocation failure");
	}
	assert(type_id == 0);
	PROTECT(types[0] = mkCharLenCE(NULL, 0, CE_UTF8));
	ntype++;

	for (i = 0; i < n; i++) {
		if (!text[i].ptr) {
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		}

		nbuf = 0;
		nadd = 0;
		wordscan_make(&scan, &text[i]);

		while (wordscan_advance(&scan)) {
			if (symtab_add_token(&symtab, &scan.current,
						&token_id) != 0) {
				error("memory allocation failure");
			}
			type_id = symtab.tokens[token_id].type_id;
			type = &symtab.types[type_id].text;

			if (type_id == ntype) {
				if (ntype == ntype_max) {
					ntype_max = 2 * ntype_max;
					types = (void *)S_realloc(
							(void *)types,
							ntype_max,
							ntype,
							sizeof(*types));
				}
				types[ntype] = mkCharLenCE((char *)type->ptr,
							   TEXT_SIZE(type),
							   CE_UTF8);
				PROTECT(types[ntype]);
				ntype++;
				nadd++;
			}

			if (TEXT_SIZE(type) == 0 && drop_empty) {
				continue;
			}

			if (nbuf == nbuf_max) {
				nbuf_max = 2 * nbuf_max;
				buf = (void *)S_realloc((void *)buf, nbuf_max,
							nbuf, sizeof(*buf));
			}
			buf[nbuf] = type_id;
			nbuf++;
		}

		SET_VECTOR_ELT(ans, i, (ans_i = allocVector(STRSXP, nbuf)));
		for (j = 0; j < nbuf; j++) {
			SET_STRING_ELT(ans_i, j, types[buf[j]]);
		}

		// no need to protect the new words any more, since they
		// are protected by ans
		UNPROTECT(nadd);
	}

	symtab_destroy(&symtab);

	UNPROTECT(3);
	return ans;
}


SEXP word_counts_text(SEXP sx, SEXP sfilter)
{
	(void)sx;
	(void)sfilter;
	error("not implemented");
	return R_NilValue;
}
