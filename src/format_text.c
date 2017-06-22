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

#include <ctype.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "corpus/src/unicode.h"
#include "rcorpus.h"


static int char_width(uint32_t code, int type)
{
	switch (type) {
	case CORPUS_CHARWIDTH_IGNORABLE:
	case CORPUS_CHARWIDTH_NONE:
		return 0;

	case CORPUS_CHARWIDTH_NARROW:
		return 1;

	case CORPUS_CHARWIDTH_WIDE:
	case CORPUS_CHARWIDTH_AMBIGUOUS:
		return 2;

	default:
		break;
	}

	// CORPUS_CHARWIDTH_OTHER; need to escape

	if (code < 0x80) {
		if (isprint((int)code)) {
			return 1;
		}
		switch (code) {
		case '\a':
		case '\b':
		case '\f':
		case '\n':
		case '\r':
		case '\t':
		case '\v':
			return 2;
		default:
			break;
		}
		return 6; // \uXXXX
	}

	if (CORPUS_UTF16_ENCODE_LEN(code) == 1) {
		return 6;
	} else {
		return 10; // \UXXXXYYYY
	}
}

static int text_width(const struct corpus_text *text)
{
	struct corpus_text_iter it;
	int32_t code;
	int type, width, w;

	corpus_text_iter_make(&it, text);
	width = 0;
	while (corpus_text_iter_advance(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);
		w = char_width(code, type);
		if (width > INT_MAX - w) {
			return INT_MAX;
		}
		width += w;
	}

	return width;
}


static SEXP format_left(const struct corpus_text *text, int trim,
			int truncate, int width_max, uint8_t *buf)
{
	struct corpus_text_iter it;
	uint8_t *dst;
	uint32_t code;
	int w, type, size, width;

	width = 0;
	dst = buf;

	corpus_text_iter_make(&it, text);
	while (corpus_text_iter_advance(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);

		if (type == CORPUS_CHARWIDTH_IGNORABLE) {
			continue;
		}

		w = char_width(code, type);
		if (width > truncate - w) {
			corpus_encode_utf8(0x2026, &dst);
			width += 1;
			break;
		} else {
			corpus_encode_utf8(code, &dst);
			width += w;
		}
	}

	if (!trim) {
		while (width < width_max) {
			*dst++ = ' ';
			width++;
		}
	}

	size = (int)(dst - buf);
	return mkCharLenCE((char *)buf, size, CE_UTF8);
}


static SEXP format_right(const struct corpus_text *text, int trim,
			 int truncate, int width_max, uint8_t *end)
{
	struct corpus_text_iter it;
	uint8_t *dst;
	uint32_t code;
	int w, type, size, width;

	width = 0;
	dst = end;

	corpus_text_iter_make(&it, text);
	corpus_text_iter_skip(&it);
	while (corpus_text_iter_retreat(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);

		if (type == CORPUS_CHARWIDTH_IGNORABLE) {
			continue;
		}

		w = char_width(code, type);
		if (width > truncate - w) {
			corpus_rencode_utf8(0x2026, &dst);
			width += 1;
			break;
		} else {
			corpus_rencode_utf8(code, &dst);
			width += w;
		}
	}

	if (!trim) {
		while (width < width_max) {
			*--dst = ' ';
			width++;
		}
	}

	size = (int)(end - dst);
	return mkCharLenCE((char *)dst, size, CE_UTF8);
}


enum justify_type {
	JUSTIFY_NONE = 0,
	JUSTIFY_LEFT,
	JUSTIFY_RIGHT
};


SEXP format_text(SEXP sx, SEXP strim, SEXP sjustify, SEXP struncate)
{
	SEXP ans, ans_i;
	enum justify_type justify;
	const char *justify_str;
	uint8_t *buf, *end;
	struct corpus_text *text;
	R_xlen_t i, n;
	int width, width_max, nbuf, trim;
	size_t size, size_max;
	int truncate;
	int nprot;

	nprot = 0;
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	PROTECT(strim = coerceVector(strim, LGLSXP)); nprot++;
	trim = (LOGICAL(strim)[0] == TRUE);

	justify_str = CHAR(STRING_ELT(sjustify, 0));
	if (strcmp(justify_str, "left") == 0) {
		justify = JUSTIFY_LEFT;
	} else if (strcmp(justify_str, "right") == 0) {
		justify = JUSTIFY_RIGHT;
	} else {
		justify = JUSTIFY_LEFT; // "none"; no "centre"
		trim = 0;
	}

	PROTECT(struncate = coerceVector(struncate, INTSXP)); nprot++;
	truncate = INTEGER(struncate)[0];
	if (truncate == NA_INTEGER) {
		truncate = INT_MAX;
	} else if (truncate < 0) {
		truncate = 0;
	}

	PROTECT(ans = allocVector(STRSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	width_max = 0;
	size_max = 0;
	for (i = 0; i < n; i++) {
		width = text_width(&text[i]);
		size = CORPUS_TEXT_SIZE(&text[i]);
		if (width > width_max) {
			width_max = width;
		}
		if (size > size_max) {
			size_max = size;
		}

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	if (width_max > truncate) {
		width_max = truncate + 1;
	}

	nbuf = size_max + width_max; // conservative
	buf = (void *)R_alloc(nbuf, sizeof(uint8_t));
	end = buf + nbuf;

	for (i = 0; i < n; i++) {
		if (justify != JUSTIFY_RIGHT) {
			ans_i = format_left(&text[i], trim, truncate,
					    width_max, buf);
		} else {
			ans_i = format_right(&text[i], trim, truncate,
					     width_max, end);
		}
		SET_STRING_ELT(ans, i, ans_i);

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	UNPROTECT(nprot);
	return ans;
}
