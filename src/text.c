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
#include "text.h"
#include "unicode.h"
#include "rcorpus.h"

#define TEXT_TAG install("corpus::text")


struct rtext {
	R_xlen_t length;
	struct text items[];
};


static void free_text(SEXP text);


SEXP alloc_text(R_xlen_t n, SEXP prot)
{
	SEXP stext, sclass;
	struct rtext *text;
	size_t size;

	if (n > (1ull << DBL_MANT_DIG)) {
		error("length (%"PRIu64") exceeds maximum (2^%d)",
			(uint64_t)n, DBL_MANT_DIG);
	} else if (n > (SIZE_MAX - sizeof(struct rtext))
			/ sizeof(struct text)) {
		error("cannot allocate 'text' array of length %"PRIu64,
			(uint64_t)n);
	}

	size = sizeof(struct rtext) + n * sizeof(struct text);
	text = (struct rtext *)Calloc(size, char);
	text->length = n;

	PROTECT(stext = R_MakeExternalPtr(text, TEXT_TAG, prot));
	R_RegisterCFinalizerEx(stext, free_text, TRUE);

	PROTECT(sclass = allocVector(STRSXP, 1));
	SET_STRING_ELT(sclass, 0, mkChar("text"));
	setAttrib(stext, R_ClassSymbol, sclass);

	UNPROTECT(2);
	return stext;;
}


SEXP coerce_text(SEXP sx)
{
	SEXP stext, str;
	struct text *text;
	const char *ptr;
	R_xlen_t len, i, n;
	bool duped = false;

	if (is_text(sx)) {
		return sx;
	} else if (is_dataset(sx)) {
		return as_text_dataset(sx);
	}

	PROTECT(sx = coerceVector(sx, STRSXP));
	n = XLENGTH(sx);

	PROTECT(stext = alloc_text(n, sx));
	text = as_text(stext, NULL);

	for (i = 0; i < n; i++) {
		str = STRING_ELT(sx, i);
		if (str == NA_STRING) {
			text[i].ptr = NULL;
			text[i].attr = 0;
		} else {
			ptr = translateCharUTF8(str);
			if (ptr != CHAR(str)) {
				if (!duped) {
					PROTECT(sx = duplicate(sx));
					R_SetExternalPtrProtected(stext, sx);
					duped = true;
				}
				str = mkCharCE(ptr, CE_UTF8);
				SET_STRING_ELT(sx, i, str);
				ptr = CHAR(str);
			}
			len = XLENGTH(str);
			if (len > TEXT_SIZE_MAX) {
				error("text size (%zu bytes)"
				      " exceeds maximum (%zu bytes)",
				      len, TEXT_SIZE_MAX);
			}
			if (text_assign(&text[i], (uint8_t *)ptr, (size_t)len,
					TEXT_NOESCAPE) != 0) {
				warning("invalid UTF-8 in character object");
				text[i].ptr = NULL;
				text[i].attr = 0;
			}
		}
	}

	UNPROTECT(2);
	if (duped) {
		UNPROTECT(1);
	}
	return stext;
}


void free_text(SEXP stext)
{
        struct rtext *text = R_ExternalPtrAddr(stext);
        Free(text);
}


int is_text(SEXP stext)
{
	return ((TYPEOF(stext) == EXTPTRSXP)
		&& (R_ExternalPtrTag(stext) == TEXT_TAG));
}


struct text *as_text(SEXP stext, R_xlen_t *lenptr)
{
	struct rtext *text;

	if (!is_text(stext))
		error("invalid 'text' object");

	text = R_ExternalPtrAddr(stext);
	if (lenptr) {
		*lenptr = text->length;
	}
	return &text->items[0];
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
	double rnchar_max;

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


SEXP subset_text(SEXP stext, SEXP si)
{
	SEXP ans, prot;
	const double *subset;
	const struct text *src;
	struct text *dst;
	R_xlen_t i, n;
	R_xlen_t s, ns;
	double ri, rn;

	src = as_text(stext, &n);
	rn = (double)n;

	PROTECT(si = coerceVector(si, REALSXP));
	ns = XLENGTH(si);
	subset = REAL(si);

	prot = R_ExternalPtrProtected(stext);
	PROTECT(ans = alloc_text(ns, prot));

	dst = as_text(ans, NULL);
	for (s = 0; s < ns; s++) {
		ri = subset[s] - 1;
		if (!(0 <= ri && ri < rn)) {
			error("invalid index (%g) at position %zu",
			      subset[s], s + 1);
		}
		i = (R_xlen_t)ri;
		dst[s] = src[i];
	}

	UNPROTECT(2);
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
