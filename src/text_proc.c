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
#include "corpus/src/text.h"
#include "corpus/src/sentscan.h"
#include "rcorpus.h"


SEXP sentences_text(SEXP sx)
{
	SEXP ans, ans_i, prot, names, stext;
	const struct text *text;
	struct text *buf, *text_i;
	struct sentscan scan;
	R_xlen_t i, n, nbuf, nbuf_max;

	prot = R_ExternalPtrProtected(stext);

	PROTECT(stext = coerce_text(sx));
	text = as_text(stext, &n);

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

		ans_i = alloc_text(nbuf, prot);
		text_i = as_text(ans_i, NULL);
		memcpy(text_i, buf, nbuf * sizeof(*buf));
		SET_VECTOR_ELT(ans, i, ans_i);
	}

	UNPROTECT(2);
	return ans;
}


SEXP tokens_text(SEXP sx, SEXP sfilter)
{
	(void)sx;
	(void)sfilter;
	return R_NilValue;
}


SEXP word_counts_text(SEXP sx, SEXP sfilter)
{
	(void)sx;
	(void)sfilter;
	return R_NilValue;
}
