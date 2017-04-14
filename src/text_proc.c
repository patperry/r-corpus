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
#include <string.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "corpus/src/symtab.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/wordscan.h"
#include "rcorpus.h"


SEXP sentences_text(SEXP sx)
{
	SEXP ans, ans_i, prot, names, stext;
	const struct text *text;
	struct text *buf, *text_i;
	struct sentscan scan;
	R_xlen_t i, n, nbuf, nbuf_max;

	PROTECT(stext = coerce_text(sx));
	text = as_text(stext, &n);
	prot = R_ExternalPtrProtected(stext);

	PROTECT(ans = allocVector(VECSXP, n));
	names = getAttrib(sx, R_NamesSymbol);
	setAttrib(ans, R_NamesSymbol, names);

	buf = NULL;
	nbuf_max = 0;

	for (i = 0; i < n; i++) {
		if (!text[i].ptr) {
			SET_VECTOR_ELT(ans, i, R_NilValue);
			continue;
		}

		nbuf = 0;
		sentscan_make(&scan, &text[i]);

		while (sentscan_advance(&scan)) {
			if (nbuf == nbuf_max) {
				if (nbuf_max == 0) {
					nbuf_max = 1;
				} else {
					nbuf_max = 2 * nbuf_max;
				}
				buf = (void *)S_realloc((void *)buf, nbuf_max,
							nbuf, sizeof(*buf));
			}

			buf[nbuf] = scan.current;
			nbuf++;
		}

		SET_VECTOR_ELT(ans, i, (ans_i = alloc_text(nbuf, prot)));
		text_i = as_text(ans_i, NULL);
		memcpy(text_i, buf, nbuf * sizeof(*buf));
	}

	UNPROTECT(2);
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
	names = getAttrib(sx, R_NamesSymbol);
	setAttrib(ans, R_NamesSymbol, names);

	if (symtab_init(&symtab, kind) != 0) {
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
