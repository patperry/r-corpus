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
#include <stdint.h>
#include <string.h>
#include "corpus/src/array.h"
#include "corpus/src/unicode.h"
#include "rcorpus.h"


#define ELLIPSIS 0x2026


static void encode_ascii(uint32_t code, uint8_t **bufptr)
{
	char *dst = (char *)*bufptr;

	if (code < 0x80) {
		*dst++ = (uint8_t)code;
	} else if (code <= 0xFFFF) {
		sprintf(dst, "<U+%04x", (unsigned)code);
		dst += 7;
		*dst++ = '>'; // overwrite trailing \0
	} else {
		sprintf(dst, "<U+%08x", (unsigned)code);
		dst += 11;
		*dst++ = '>';
	}

	*bufptr = (uint8_t *)dst;
}


static void rencode_ascii(uint32_t code, uint8_t **bufptr)
{
	char *dst = (char *)*bufptr;

	if (code < 0x80) {
		*--dst = (uint8_t)code;
	} else if (code <= 0xFFFF) {
		dst -= 8;
		sprintf(dst, "<U+%04x", (unsigned)code);
		dst[7] = '>';
	} else {
		dst -= 12;
		sprintf(dst, "<U+%08x", (unsigned)code);
		dst[11] = '>';
	}

	*bufptr = (uint8_t *)dst;
}


static void encode(int utf8, uint32_t code, uint8_t **bufptr)
{
	if (utf8) {
		corpus_encode_utf8(code, bufptr);
	} else {
		encode_ascii(code, bufptr);
	}
}


static void rencode(int utf8, uint32_t code, uint8_t **bufptr)
{
	if (utf8) {
		corpus_rencode_utf8(code, bufptr);
	} else {
		rencode_ascii(code, bufptr);
	}
}


static int char_width(uint32_t code, int type, int utf8)
{
	(void)utf8;

	if (type == CORPUS_CHARWIDTH_IGNORABLE) {
		return 0;
	}

	if (CORPUS_UTF16_ENCODE_LEN(code) > 1) {
		// R doesn't handle these values
		//   UTF-8 locale: \uXXXXYYYY   (10)
		//   C     locale: <U+XXXXYYYY> (12)
		return utf8 ? 10 : 12; 
	}

	if (code >= 0x80 && !utf8) {
		return 8; // <U+XXXX>
	}

	switch (type) {
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
			return 4; // \ooo octal escape
		}
	}

	if (CORPUS_UTF16_ENCODE_LEN(code) == 1) {
		return 6; // \uXXXX
	} else {
		return 10; // \UXXXXYYYY
	}
}


static int text_width(const struct corpus_text *text, int limit, int utf8)
{
	struct corpus_text_iter it;
	int32_t code;
	int type, width, w;
	int ellipsis = utf8 ? 1 : 3;

	corpus_text_iter_make(&it, text);
	width = 0;
	while (corpus_text_iter_advance(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);
		w = char_width(code, type, utf8);
		if (width > limit - w) {
			return width + ellipsis;
		}
		width += w;
	}

	return width;
}


static int text_rwidth(const struct corpus_text *text, int limit, int utf8)
{
	struct corpus_text_iter it;
	int32_t code;
	int type, width, w;
	int ellipsis = utf8 ? 1 : 3;

	corpus_text_iter_make(&it, text);
	corpus_text_iter_skip(&it);
	width = 0;
	while (corpus_text_iter_retreat(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);
		w = char_width(code, type, utf8);
		if (width > limit - w) {
			return width + ellipsis;
		}
		width += w;
	}

	return width;
}


static void grow_buffer(uint8_t **bufptr, int *nbufptr, int nadd)
{
	uint8_t *buf = *bufptr;
	int nbuf0 = *nbufptr;
	int nbuf = nbuf0;
	int err;

	if ((err = corpus_array_size_add(&nbuf, 1, nbuf0, nadd))) {
		error("buffer size (%d + %d bytes)"
		      " exceeds maximum (%d bytes)", nbuf0, nadd,
		      INT_MAX);
	}

	buf = (void *)S_realloc((char *)buf, nbuf, nbuf0, sizeof(uint8_t));

	*bufptr = buf;
	*nbufptr = nbuf;
}


static SEXP format_left(const struct corpus_text *text, int trim,
			int chars, int width_max, int utf8,
			uint8_t **bufptr, int *nbufptr)
{
	uint8_t *buf = *bufptr;
	int nbuf = *nbufptr;
	uint8_t *end = buf + nbuf;
	struct corpus_text_iter it;
	uint8_t *dst;
	uint32_t code;
	int w, trunc, type, nbyte, fill, len, off, width;

	dst = buf;
	width = 0;
	trunc = 0;
	corpus_text_iter_make(&it, text);

	while (!trunc && corpus_text_iter_advance(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);

		if (type == CORPUS_CHARWIDTH_IGNORABLE) {
			continue;
		}

		w = char_width(code, type, utf8);
		if (width > chars - w) {
			code = ELLIPSIS;
			w = utf8 ? 1 : 3;
			trunc = 1;
		}

		nbyte = CORPUS_UTF8_ENCODE_LEN(code);
		if (dst + nbyte > end) {
			off = (int)(dst - buf);

			grow_buffer(&buf, &nbuf, nbyte);
			dst = buf + off;
			end = buf + nbuf;
		}

		if (trunc) {
			if (utf8) {
				corpus_encode_utf8(ELLIPSIS, &dst);
			} else {
				// nbyte(ELLIPSIS) == 3 so no need to reserve
				*dst++ = '.';
				*dst++ = '.';
				*dst++ = '.';
			}
		} else {
			encode(utf8, code, &dst);
		}
		width += w;
	}

	if (!trim && ((fill = width_max - width)) > 0) {
		if (dst + fill > end) {
			off = (int)(dst - buf);

			grow_buffer(&buf, &nbuf, fill);
			dst = buf + off;
			end = buf + nbuf;
		}

		while (fill-- > 0) {
			*dst++ = ' ';
		}
	}


	*bufptr = buf;
	*nbufptr = nbuf;

	len = (int)(dst - buf);
	return mkCharLenCE((char *)buf, len, CE_UTF8);
}


static SEXP format_centre(const struct corpus_text *text, int chars,
			  int width_max, int utf8,
			  uint8_t **bufptr, int *nbufptr)
{
	uint8_t *buf = *bufptr;
	int nbuf = *nbufptr;
	uint8_t *end = buf + nbuf;
	struct corpus_text_iter it;
	uint8_t *dst;
	uint32_t code;
	int i, w, trunc, type, nbyte, bfill, fill, len, off,
	    fullwidth, width;

	dst = buf;

	fullwidth = text_width(text, chars, utf8);

	if ((fill = width_max - fullwidth) > 0) {
		bfill = fill / 2;

		if (dst + bfill > end) {
			off = (int)(dst - buf);
			grow_buffer(&buf, &nbuf, bfill);
			dst = buf + off;
			end = buf + nbuf;
		}

		for (i = 0; i < bfill; i++) {
			*dst++ = ' ';
		}
	}

	width = 0;
	trunc = 0;
	corpus_text_iter_make(&it, text);

	while (!trunc && corpus_text_iter_advance(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);

		if (type == CORPUS_CHARWIDTH_IGNORABLE) {
			continue;
		}

		w = char_width(code, type, utf8);
		if (width > chars - w) {
			code = ELLIPSIS;
			w = utf8 ? 1 : 3;
			trunc = 1;
		}

		nbyte = CORPUS_UTF8_ENCODE_LEN(code);
		if (dst + nbyte > end) {
			off = (int)(dst - buf);

			grow_buffer(&buf, &nbuf, nbyte);
			dst = buf + off;
			end = buf + nbuf;
		}

		if (trunc) {
			if (utf8) {
				corpus_encode_utf8(ELLIPSIS, &dst);
			} else {
				// nbyte(ELLIPSIS) == 3 so no need to reserve
				*dst++ = '.';
				*dst++ = '.';
				*dst++ = '.';
			}
		} else {
			encode(utf8, code, &dst);
		}
		width += w;
	}

	if (((fill = width_max - width - bfill)) > 0) {
		if (dst + fill > end) {
			off = (int)(dst - buf);

			grow_buffer(&buf, &nbuf, fill);
			dst = buf + off;
			end = buf + nbuf;
		}

		while (fill-- > 0) {
			*dst++ = ' ';
		}
	}


	*bufptr = buf;
	*nbufptr = nbuf;

	len = (int)(dst - buf);
	return mkCharLenCE((char *)buf, len, CE_UTF8);
}


static SEXP format_right(const struct corpus_text *text, int trim,
			 int chars, int width_max, int utf8,
			 uint8_t **bufptr, int *nbufptr)
{
	uint8_t *buf = *bufptr;
	int nbuf = *nbufptr;
	struct corpus_text_iter it;
	uint8_t *dst;
	uint32_t code;
	int w, fill, trunc, type, off, len, nbyte, width;

	dst = buf + nbuf;
	width = 0;
	trunc = 0;
	corpus_text_iter_make(&it, text);
	corpus_text_iter_skip(&it);

	while (!trunc && corpus_text_iter_retreat(&it)) {
		code = it.current;
		type = corpus_unicode_charwidth(code);

		if (type == CORPUS_CHARWIDTH_IGNORABLE) {
			continue;
		}

		w = char_width(code, type, utf8);
		if (width > chars - w) {
			code = ELLIPSIS;
			w = utf8 ? 1 : 3;
			trunc = 1;
		}

		nbyte = CORPUS_UTF8_ENCODE_LEN(code);

		if (dst < buf + nbyte) {
			off = dst - buf;
			len = nbuf - off;

			grow_buffer(&buf, &nbuf, nbyte);
			dst = buf + nbuf - len;
			memmove(dst, buf + off, len);
		}

		if (trunc) {
			if (utf8) {
				corpus_rencode_utf8(ELLIPSIS, &dst);
			} else {
				// nbyte(ELLIPSIS) == 3 so no need to reserve
				*--dst = '.';
				*--dst = '.';
				*--dst = '.';
			}
		} else {
			rencode(utf8, code, &dst);
		}
		width += w;
	}

	if (!trim && ((fill = width_max - width)) > 0) {
		if (dst < buf + fill) {
			off = dst - buf;
			len = nbuf - off;

			grow_buffer(&buf, &nbuf, fill);
			dst = buf + nbuf - len;
			memmove(dst, buf + off, len);
		}

		while (fill-- > 0) {
			*--dst = ' ';
		}
	}

	*bufptr = buf;
	*nbufptr = nbuf;

	off = (int)(dst - buf);
	len = nbuf - off;
	return mkCharLenCE((char *)dst, len, CE_UTF8);
}


enum justify_type {
	JUSTIFY_NONE = 0,
	JUSTIFY_LEFT,
	JUSTIFY_CENTRE,
	JUSTIFY_RIGHT
};


SEXP format_text(SEXP sx, SEXP strim, SEXP schars, SEXP sjustify,
		 SEXP swidth, SEXP sna_encode, SEXP sutf8)
{
	SEXP ans, ans_i;
	enum justify_type justify;
	const char *justify_str;
	uint8_t *buf;
	struct corpus_text *text, *text_i;
	struct corpus_text na_text;
	R_xlen_t i, n;
	int chars, chars_i, ellipsis, width, width_max, nbuf, trim, na_encode,
	    utf8, nprot;

	nprot = 0;
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	PROTECT(strim = coerceVector(strim, LGLSXP)); nprot++;
	trim = (LOGICAL(strim)[0] == TRUE);

	PROTECT(strim = coerceVector(sutf8, LGLSXP)); nprot++;
	utf8 = (LOGICAL(sutf8)[0] == TRUE);
	ellipsis = utf8 ? 1 : 3;

	PROTECT(schars = coerceVector(schars, INTSXP)); nprot++;
	chars = INTEGER(schars)[0];
	if (chars == NA_INTEGER) {
		chars = INT_MAX - ellipsis;
	} else if (chars < 0) {
		chars = 0;
	}

	justify_str = CHAR(STRING_ELT(sjustify, 0));
	if (strcmp(justify_str, "left") == 0) {
		justify = JUSTIFY_LEFT;
	} else if (strcmp(justify_str, "right") == 0) {
		justify = JUSTIFY_RIGHT;
	} else if (strcmp(justify_str, "centre") == 0) {
		justify = JUSTIFY_CENTRE;
	} else {
		justify = JUSTIFY_NONE;
		trim = 1;
	}

	if (swidth != R_NilValue) {
		PROTECT(swidth = coerceVector(swidth, INTSXP)); nprot++;
		width_max = INTEGER(swidth)[0];
		if (width_max == NA_INTEGER || width_max < 0) {
			width_max = 0;
		}
	} else {
		width_max = 0;
	}

	PROTECT(sna_encode = coerceVector(sna_encode, LGLSXP)); nprot++;
	na_encode = (LOGICAL(sna_encode)[0] == TRUE);

	PROTECT(ans = allocVector(STRSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	for (i = 0; i < n; i++) {
		if (text[i].ptr == NULL) {
			width = na_encode ? 2 : 0;
		} else if (justify == JUSTIFY_RIGHT) {
			width = text_rwidth(&text[i], chars, utf8);
		} else {
			width = text_width(&text[i], chars, utf8);
		}

		if (width > width_max) {
			width_max = width;
		}

		if (width_max >= chars + ellipsis) {
			width_max = chars + ellipsis;
			break;
		}
	}

	nbuf = width_max;
	buf = (void *)R_alloc(nbuf, sizeof(uint8_t));

	na_text.ptr = (uint8_t *)"NA";
	na_text.attr = 2;

	for (i = 0; i < n; i++) {
		text_i = &text[i];
		if (text_i->ptr == NULL) {
			if (!na_encode) {
				SET_STRING_ELT(ans, i, NA_STRING);
				continue;
			} else {
				text_i = &na_text;
				chars_i = 2;
			}
		} else {
			chars_i = chars;
		}

		switch (justify) {
		case JUSTIFY_LEFT:
		case JUSTIFY_NONE:
			ans_i = format_left(text_i, trim, chars_i,
					    width_max, utf8, &buf, &nbuf);
			break;

		case JUSTIFY_CENTRE:
			ans_i = format_centre(text_i, chars_i, width_max,
					      utf8, &buf, &nbuf);
			break;

		case JUSTIFY_RIGHT:
			ans_i = format_right(text_i, trim, chars_i,
					     width_max, utf8, &buf, &nbuf);
			break;
		}

		SET_STRING_ELT(ans, i, ans_i);

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	UNPROTECT(nprot);
	return ans;
}
