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
#include <stddef.h>
#include <stdint.h>
#include "rcorpus.h"

#define TERMSET_TAG install("corpus::termset")


static struct termset *termset_new(void);
static void termset_free(struct termset *obj);
static void set_items_termset(SEXP termset);


struct termset *termset_new(void)
{
	struct termset *obj;
	int err;

	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));

	TRY(corpus_termset_init(&obj->set));
	obj->has_set = 1;

	err = 0;
out:
	if (err) {
		termset_free(obj);
		error("memory allocation failure");
	}

	return obj;
}


void termset_free(struct termset *obj)
{
	int n;

	if (!obj) {
		return;
	}

	if (obj->has_set) {
		corpus_termset_destroy(&obj->set);
		obj->has_set = 0;
	}

	n = obj->nitem;
	while (n-- > 0) {
		corpus_free(obj->items[n].ptr);
	}
	obj->nitem = 0;

	corpus_free(obj->items);
	obj->items = NULL;

	corpus_free(obj);
}


static void free_termset(SEXP obj)
{
        struct termset *set = R_ExternalPtrAddr(obj);
	termset_free(set);
	R_ClearExternalPtr(obj);
}


int is_termset(SEXP stermset)
{
	return ((TYPEOF(stermset) == EXTPTRSXP)
		&& (R_ExternalPtrTag(stermset) == TERMSET_TAG));
}


struct termset *as_termset(SEXP stermset)
{
	if (!is_termset(stermset)) {
		error("invalid 'termset' object");
	}
	return R_ExternalPtrAddr(stermset);
}


#define CLEANUP() \
	do { \
		corpus_free(buf); \
		buf = NULL; \
		if (has_render) { \
			utf8lite_render_destroy(&render); \
			has_render = 0; \
		} \
	} while (0)


SEXP alloc_termset(SEXP sterms, const char *name,
		   struct corpus_filter *filter, int allow_dup)
{
	SEXP ans;
	struct corpus_wordscan scan;
	struct utf8lite_render render;
	const struct utf8lite_text *terms;
	struct utf8lite_text type;
	struct termset *obj;
	const uint8_t *ptr;
	size_t attr, size;
	int *buf, *buf2;
	char *errstr;
	R_xlen_t i, n;
	int err,  has_render, id, j, length, max_length,
	    nbuf, nprot, rendered_error, type_id;

	has_render = 0;
	buf = NULL;
	nprot = 0;
	err = 0;
	max_length = 1;
	rendered_error = 0;

	obj = termset_new();
	PROTECT(ans = R_MakeExternalPtr(obj, TERMSET_TAG, R_NilValue));
	nprot++;
	R_RegisterCFinalizerEx(ans, free_termset, TRUE);

	if (sterms == R_NilValue) {
		goto out;
	}

	PROTECT(sterms = coerce_text(sterms)); nprot++;
	terms = as_text(sterms, &n);
	if (n == 0) {
		goto out;
	}

	TRY_ALLOC(obj->items = corpus_malloc(n * sizeof(*obj->items)));

	nbuf = 32;
	TRY_ALLOC(buf = corpus_malloc(nbuf * sizeof(*buf)));

	TRY(utf8lite_render_init(&render, UTF8LITE_ESCAPE_CONTROL));
	has_render = 1;

	for (i = 0; i < n; i++) {
		corpus_wordscan_make(&scan, &terms[i]);

		length = 0;
		while (corpus_wordscan_advance(&scan)) {
			// skip over leading spaces
			if (scan.type == CORPUS_WORD_NONE) {
				continue;
			}

			// found a non-space word
			ptr = scan.current.ptr;
			attr = UTF8LITE_TEXT_BITS(&scan.current);

			// skip until we find a space
			while (corpus_wordscan_advance(&scan)) {
				if (scan.type == CORPUS_WORD_NONE) {
					break;
				}
				attr |= UTF8LITE_TEXT_BITS(&scan.current);
			}

			size = (size_t)(scan.current.ptr - ptr);

			// found a type; get the id
			type.ptr = (uint8_t *)ptr;
			type.attr = attr | size;

			TRY(corpus_filter_add_type(filter, &type, &type_id));

			// expand the buffer if necessary
			if (length == nbuf) {
				nbuf = nbuf * 2;
				buf2 = corpus_realloc(buf,
						      nbuf * sizeof(*buf));
				if (!buf2) {
					err = CORPUS_ERROR_NOMEM;
					goto out;
				}
				buf = buf2;
			}

			// add the type to the buffer
			buf[length] = type_id;
			length++;
		}

		if (length > max_length) {
			max_length = length;
		}

		if (length == 0) {
			utf8lite_render_printf(&render,
				"%s term in position %"PRIu64" (\"",
				name, (uint64_t)(i+1));
			utf8lite_render_text(&render, &terms[i]);
			utf8lite_render_string(&render, "\") ");
			utf8lite_render_string(&render,
					       "has empty type (\"\")");
			rendered_error = 1;
			goto out;
		}

		for (j = 0; j < length; j++) {
			type_id = buf[j];
			if (!filter->props[type_id].drop) {
				continue;
			}

			utf8lite_render_printf(&render,
				"%s term in position %"PRIu64" (\"",
				name, (uint64_t)(i+1));
			utf8lite_render_text(&render, &terms[i]);
			utf8lite_render_string(&render, "\") ");
			utf8lite_render_string(&render,
				"contains a dropped type (\"");
			utf8lite_render_text(&render,
				&filter->symtab.types[type_id].text);
			utf8lite_render_string(&render, "\")");
			rendered_error = 1;
			goto out;
		}

		if (!allow_dup
			&& corpus_termset_has(&obj->set, buf, length, &id)) {
			utf8lite_render_printf(&render,
				"%s terms in positions %"PRIu64
				" and %"PRIu64" (\"", name,
				(uint64_t)(id + 1), (uint64_t)(i + 1));
			utf8lite_render_text(&render, &terms[id]);
			utf8lite_render_string(&render, "\" and \"");
			utf8lite_render_text(&render, &terms[i]);
			utf8lite_render_string(&render,
					       "\") have the same type");
			rendered_error = 1;
			goto out;
		}

		TRY(corpus_termset_add(&obj->set, buf, length, &id));
		if (id == obj->nitem) {
			TRY(utf8lite_text_init_copy(&obj->items[id],
						    &terms[i]));
			obj->nitem = id + 1;
		}
	}

	err = 0;

out:
	if (rendered_error && !((err = render.error))) {
		errstr = (void *)R_alloc(render.length + 1, 1);
		memcpy(errstr, render.string, render.length + 1);
		CLEANUP();
		error(errstr);
		return R_NilValue;
	}

	CLEANUP();
	if (err) {
		error("failed initializing %s term set", name);
	}

	obj->max_length = max_length;
	set_items_termset(ans);

	UNPROTECT(nprot);
	return ans;
}


static void set_items_termset(SEXP stermset)
{
	SEXP items, str;
	const struct utf8lite_text *item;
	int i, n;
	struct termset *obj;

	obj = as_termset(stermset);
	n = obj->nitem;
	PROTECT(items = allocVector(STRSXP, n));

	for (i = 0; i < n; i++) {
		item = &obj->items[i];
		str = mkCharLenCE((char *)item->ptr,
				  (int)UTF8LITE_TEXT_SIZE(item),
				  CE_UTF8);
		SET_STRING_ELT(items, i, str);
	}

	R_SetExternalPtrProtected(stermset, items);
	UNPROTECT(1);
}


SEXP items_termset(SEXP stermset)
{
	return R_ExternalPtrProtected(stermset);
}
