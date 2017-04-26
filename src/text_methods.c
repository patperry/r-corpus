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

#include <inttypes.h>
#include <float.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <Rdefines.h>
#include "corpus/src/text.h"
#include "corpus/src/unicode.h"
#include "rcorpus.h"


SEXP names_text(SEXP stext)
{
	if (!is_text(stext)) {
		error("invalid 'text' object");
	}

	return getListElement(stext, "names");
}


SEXP length_text(SEXP stext)
{
	R_xlen_t len;
	as_text(stext, &len);
	return ScalarReal((double)len);
}


SEXP as_character_text(SEXP stext)
{
	SEXP ans, str;
	struct text *text;
	struct text_iter it;
	const uint8_t *ptr;
	uint8_t *buf, *end;
	size_t buf_len, len;
	R_xlen_t i, n;

	text = as_text(stext, &n);

	// allocate temporary buffer for decoding
	buf = NULL;
	buf_len = 0;

	PROTECT(ans = allocVector(STRSXP, n));

	for (i = 0; i < n; i++) {
		ptr = text[i].ptr;
		len = TEXT_SIZE(&text[i]);

		if (ptr == NULL) {
			str = NA_STRING;
		} else {
			if (TEXT_HAS_ESC(&text[i])) {
				// grow buffer if necessary
				if (buf_len < len) {
					buf_len = len;
					buf = (uint8_t *)R_alloc(buf_len, 1);
				}

				text_iter_make(&it, &text[i]);
				end = buf;

				while (text_iter_advance(&it)) {
					encode_utf8(it.current, &end);
				}

				ptr = buf;
				len = end - ptr;
			} else {
				len = TEXT_SIZE(&text[i]);
			}
			str = mkCharLenCE((char *)ptr, len, CE_UTF8);
		}
		SET_STRING_ELT(ans, i, str);
	}

	UNPROTECT(1);
	return ans;
}


SEXP is_na_text(SEXP stext)
{
	SEXP ans;
	struct text *text;
	R_xlen_t i, n;
	int *isna;

	text = as_text(stext, &n);
	PROTECT(ans = allocVector(LGLSXP, n));
	isna = LOGICAL(ans);

	for (i = 0; i < n; i++) {
		if (text[i].ptr) {
			isna[i] = FALSE;
		} else {
			isna[i] = TRUE;
		}
	}

	UNPROTECT(1);
	return ans;
}


SEXP anyNA_text(SEXP stext)
{
	struct text *text;
	R_xlen_t i, n;
	int anyNA;

	text = as_text(stext, &n);

	anyNA = FALSE;
	for (i = 0; i < n; i++) {
		if (!text[i].ptr) {
			anyNA = TRUE;
			break;
		}
	}

	return ScalarLogical(anyNA);
}
