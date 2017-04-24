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

#include <stdlib.h>
#include "corpus/src/array.h"
#include "corpus/src/error.h"
#include "corpus/src/text.h"
#include "corpus/src/unicode.h"
#include "corpus/src/xalloc.h"
#include "rcorpus.h"


static int mkchar_ensure(struct mkchar *mk, int nmin);


void mkchar_init(struct mkchar *mk)
{
	mk->buf = NULL;
	mk->size = 0;
}


void mkchar_destroy(struct mkchar *mk)
{
	free(mk->buf);
}


/* Note: We have to destroy the mkchar object before calling error
 * so that we don't leak memory.
 */
SEXP mkchar_get(struct mkchar *mk, const struct text *text)
{
	SEXP ans;
	uint8_t *ptr;
	size_t len = TEXT_SIZE(text);
	struct text_iter it;
	int err;

	if (len >= INT_MAX) {
		mkchar_destroy(mk);
		error("character string length exceeds maximum (%d)",
			INT_MAX - 1);
	}

	if (text->ptr == NULL) {
		ans = NA_STRING;
	} else {
		if (TEXT_HAS_ESC(text)) {
			if ((err = mkchar_ensure(mk, (int)len))) {
				goto error;
			}
			text_iter_make(&it, text);
			ptr = mk->buf;
			while (text_iter_advance(&it)) {
				encode_utf8(it.current, &ptr);
			}
			len = ptr - mk->buf;
			ptr = mk->buf;
		} else {
			ptr = (uint8_t *)text->ptr;
		}

		ans = mkCharLenCE((char *)ptr, (int)len, CE_UTF8);
	}

	return ans;

error:
	mkchar_destroy(mk);
	error("memory allocation failure while creating character object");
	return NA_STRING;
}


static int mkchar_ensure(struct mkchar *mk, int nmin)
{
	void *base = mk->buf;
	int size = mk->size;
	int err;

	if (size >= nmin) {
		return 0;
	}

	err = array_grow(&base, &size, sizeof(uint8_t), 0, nmin);
	if (!err) {
		mk->buf = base;
		mk->size = size;
	}

	return err;
}
