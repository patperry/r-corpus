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

#include "rcorpus.h"

static SEXP trunc_left(struct mkchar *mk, const struct utf8lite_text *text,
		       int chars)
{
	struct utf8lite_graphscan scan;
	struct utf8lite_text sub;
	int err = 0, width = 0, w;

	sub.ptr = text->ptr;
	sub.attr = UTF8LITE_TEXT_BITS(text);

	utf8lite_graphscan_make(&scan, text);
	while (utf8lite_graphscan_advance(&scan)) {
		TRY(utf8lite_graph_measure(&scan.current, 0, &w));
		if (w > 0) {
			if (width > chars - w) {
				break;
			}
			width += w;
		}
	}
	sub.attr |= (size_t)(scan.ptr - text->ptr);
out:
	CHECK_ERROR(err);
	return mkchar_get(mk, &sub);
}


static SEXP trunc_right(struct mkchar *mk, const struct utf8lite_text *text,
			int chars)
{
	struct utf8lite_graphscan scan;
	struct utf8lite_text sub;
	const uint8_t *end;
	int err = 0, width = 0, w;

	sub.ptr = NULL;
	sub.attr = UTF8LITE_TEXT_BITS(text);
	end = text->ptr + UTF8LITE_TEXT_SIZE(text);

	utf8lite_graphscan_make(&scan, text);
	utf8lite_graphscan_skip(&scan);
	while (utf8lite_graphscan_retreat(&scan)) {
		TRY(utf8lite_graph_measure(&scan.current, 0, &w));
		if (w > 0) {
			if (width > chars - w) {
				break;
			}
			width += w;
		}
	}
	utf8lite_graphscan_retreat(&scan);
	sub.ptr = (uint8_t *)scan.ptr;
	sub.attr |= (size_t)(end - sub.ptr);
out:
	CHECK_ERROR(err);
	return mkchar_get(mk, &sub);
}


SEXP text_trunc(SEXP sx, SEXP schars, SEXP sright)
{
        SEXP ans, names, elt;
	struct mkchar mk;
        const struct utf8lite_text *text;
	R_xlen_t i, n;
	int nprot = 0, chars, right;

	text = as_text(sx, &n);
	chars = INTEGER(schars)[0];
	right = LOGICAL(sright)[0] == TRUE;
	mkchar_init(&mk);

	PROTECT(ans = allocVector(STRSXP, n)); nprot++;
	PROTECT(names = names_text(sx)); nprot++;
	setAttrib(ans, R_NamesSymbol, names);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) {
			elt = NA_STRING;
		} else if (right) {
			elt = trunc_right(&mk, &text[i], chars);
		} else {
			elt = trunc_left(&mk, &text[i], chars);
		}
		SET_STRING_ELT(ans, i, elt);
	}

	UNPROTECT(nprot);
	return ans;
}
