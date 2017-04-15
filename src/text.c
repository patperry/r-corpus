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

#define TEXT_TAG install("corpus::text")


struct rtext {
	R_xlen_t length;
	struct text items[];
};


static void free_text(SEXP text);


SEXP alloc_text(R_xlen_t n, SEXP prot)
{
	SEXP ans, shandle, sclass, snames;
	struct rtext *text;
	size_t size;

	if ((uint64_t)n > (1ull << DBL_MANT_DIG)) {
		error("length (%"PRIu64") exceeds maximum (2^%d)",
			(uint64_t)n, DBL_MANT_DIG);
	} else if ((size_t)n > (SIZE_MAX - sizeof(struct rtext))
			/ sizeof(struct text)) {
		error("cannot allocate 'text' array of length %"PRIu64,
			(uint64_t)n);
	}

	size = sizeof(struct rtext) + n * sizeof(struct text);
	text = (struct rtext *)Calloc(size, char);
	text->length = n;

	PROTECT(shandle = R_MakeExternalPtr(text, TEXT_TAG, prot));
	R_RegisterCFinalizerEx(shandle, free_text, TRUE);

	PROTECT(ans = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(ans, 0, shandle);
	SET_VECTOR_ELT(ans, 1, R_NilValue);

	PROTECT(snames = allocVector(STRSXP, 2));
	SET_STRING_ELT(snames, 0, mkChar("handle"));
	SET_STRING_ELT(snames, 1, mkChar("names"));
	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(sclass = allocVector(STRSXP, 1));
	SET_STRING_ELT(sclass, 0, mkChar("text"));
	setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(4);
	return ans;
}


SEXP alloc_na_text(void)
{
	SEXP ans;
	struct text *text;

	PROTECT(ans = alloc_text(1, R_NilValue));
	text = as_text(ans, NULL);
	text[0].ptr = NULL;
	text[0].attr = 0;
	UNPROTECT(1);

	return ans;
}


void free_text(SEXP stext)
{
        struct rtext *text = R_ExternalPtrAddr(stext);
        Free(text);
}

/* modified from R-Exts Section 5.9.6 "handling lists" */
static int findListElement(SEXP list, const char *str)
{
	SEXP names = getAttrib(list, R_NamesSymbol);
	int i, n;

	n = LENGTH(list);
	for (i = 0; i < n; i++) {
		if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
			return i;
		}
	}
	return -1;
}

static SEXP getListElement(SEXP list, const char *str)
{
	int i = findListElement(list, str);
	if (i < 0) {
		return R_NilValue;
	}
	return VECTOR_ELT(list, i);
}


int is_text(SEXP stext)
{
	SEXP handle;

	if (!isVectorList(stext)) {
		return 0;
	}

	handle = getListElement(stext, "handle");
	if (handle == R_NilValue) {
		return 0;
	}

	return ((TYPEOF(handle) == EXTPTRSXP)
		&& (R_ExternalPtrTag(handle) == TEXT_TAG));
}


struct text *as_text(SEXP stext, R_xlen_t *lenptr)
{
	SEXP handle;
	struct rtext *text;

	if (!is_text(stext)) {
		error("invalid 'text' object");
	}

	handle = getListElement(stext, "handle");
	text = R_ExternalPtrAddr(handle);

	if (lenptr) {
		*lenptr = text->length;
	}

	return &text->items[0];
}


SEXP names_text(SEXP stext)
{
	if (!is_text(stext)) {
		error("invalid 'text' object");
	}

	return getListElement(stext, "names");
}


// this is only for internal use; it modifies the argument, rather
// than making a copy
static SEXP setnames_text(SEXP stext, SEXP svalue)
{
	int i;

	if (!is_text(stext)) {
		error("invalid 'text' object");
	}
	i = findListElement(stext, "names");
	if (i < 0) {
		error("invalid 'text' object (no 'names' field)");
	}
	SET_VECTOR_ELT(stext, i, svalue);

	return stext;
}


SEXP coerce_text(SEXP sx)
{
	SEXP stext, str;
	struct text *text;
	const char *ptr;
	R_xlen_t i, n;
	uint64_t len;
	bool duped = false;

	if (is_text(sx)) {
		return sx;
	} else if (is_dataset(sx)) {
		return as_text_dataset(sx);
	}

	PROTECT(sx = coerceVector(sx, STRSXP));
	n = XLENGTH(sx);

	PROTECT(stext = alloc_text(n, sx));
	PROTECT(stext = setnames_text(stext, getAttrib(sx, R_NamesSymbol)));

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
			len = (uint64_t)XLENGTH(str);
			if (len > (uint64_t)TEXT_SIZE_MAX) {
				error("text size (%"PRIu64" bytes)"
				      " exceeds maximum (%"PRIu64" bytes)",
				      len, (uint64_t)TEXT_SIZE_MAX);
			}
			if (text_assign(&text[i], (uint8_t *)ptr, (size_t)len,
					TEXT_NOESCAPE) != 0) {
				warning("invalid UTF-8 in character object");
				text[i].ptr = NULL;
				text[i].attr = 0;
			}
		}
	}

	UNPROTECT(3);
	if (duped) {
		UNPROTECT(1);
	}
	return stext;
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
			error("invalid index (%g) at position %"PRIu64,
			      subset[s], (uint64_t)(s + 1));
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
