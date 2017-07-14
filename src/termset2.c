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

#include <stddef.h>
#include <inttypes.h>
#include "corpus/src/error.h"
#include "corpus/src/memory.h"
#include "corpus/src/render.h"
#include "corpus/src/table.h"
#include "corpus/src/tree.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "rcorpus.h"

#ifdef error
#  undef error
#endif

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
		Rf_error("memory allocation failure");
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
		Rf_error("invalid 'termset' object");
	}
	return R_ExternalPtrAddr(stermset);
}


#define CLEANUP() \
	do { \
		corpus_free(buf); \
		buf = NULL; \
		if (has_render) { \
			corpus_render_destroy(&render); \
			has_render = 0; \
		} \
		if (has_typemap) { \
			corpus_typemap_destroy(&typemap); \
			has_typemap = 0; \
		} \
	} while (0)


SEXP alloc_termset(SEXP sterms, const char *name,
		   struct corpus_filter *filter, int allow_dup)
{
	SEXP ans;
	struct corpus_render render;
	struct corpus_typemap typemap;
	const struct corpus_text *terms;
	struct termset *obj;
	int *buf, *buf2;
	char *errstr;
	R_xlen_t i, n;
	int err,  has_render, has_typemap, id, length, max_length,
	    nbuf, nprot, rendered_error, type_id, type_kind;

	has_render = 0;
	has_typemap = 0;
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

	TRY(corpus_render_init(&render, CORPUS_ESCAPE_CONTROL));
	has_render = 1;

	type_kind = filter->symtab.typemap.kind;
	TRY(corpus_typemap_init(&typemap, type_kind, NULL));
	has_typemap = 1;

	for (i = 0; i < n; i++) {
		TRY(corpus_typemap_set(&typemap, &terms[i]));
		TRY(corpus_filter_start(filter, &typemap.type,
					CORPUS_FILTER_SCAN_TYPES));

		length = 0;
		type_id = CORPUS_FILTER_IGNORED;

		while (corpus_filter_advance(filter)) {
			type_id = filter->type_id;

			// skip ignored types
			if (type_id == CORPUS_FILTER_IGNORED) {
				continue;
			}

			// error on dropped types
			if (type_id < 0) {
				break;
			}

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

		TRY(filter->error);

		if (length > max_length) {
			max_length = length;
		}

		if (length == 0 || type_id == CORPUS_FILTER_DROPPED) {
			corpus_render_printf(&render,
				"%s term in position %"PRIu64" ('",
				name, (uint64_t)(i+1));
			corpus_render_text(&render, &terms[i]);
			corpus_render_string(&render, "') ");
			if (length == 0) {
				corpus_render_string(&render,
						"does not contain a type");
			} else {
				corpus_render_string(&render,
						"contains a dropped type ('");
				corpus_render_text(&render, &filter->current);
				corpus_render_string(&render, "')");
			}
			rendered_error = 1;
			goto out;
		}

		if (!allow_dup
			&& corpus_termset_has(&obj->set, buf, length, &id)) {
			corpus_render_printf(&render,
				"%s terms in positions %"PRIu64
				" and %"PRIu64" ('", name,
				(uint64_t)(id + 1), (uint64_t)(i + 1));
			corpus_render_text(&render, &terms[id]);
			corpus_render_string(&render, "' and '");
			corpus_render_text(&render, &terms[i]);
			corpus_render_string(&render, "') have the same type");
			rendered_error = 1;
			goto out;
		}

		TRY(corpus_termset_add(&obj->set, buf, length, &id));
		if (id == obj->nitem) {
			TRY(corpus_text_init_copy(&obj->items[id],
						  &typemap.type));
			obj->nitem = id + 1;
		}
	}

	err = 0;

out:
	if (rendered_error && !((err = render.error))) {
		errstr = (void *)R_alloc(render.length + 1, 1);
		memcpy(errstr, render.string, render.length + 1);
		CLEANUP();
		Rf_error(errstr);
		return R_NilValue;
	}

	CLEANUP();
	if (err) {
		Rf_error("failed initializing %s term set", name);
	}

	obj->max_length = max_length;
	set_items_termset(ans);

	UNPROTECT(nprot);
	return ans;
}


static void set_items_termset(SEXP stermset)
{
	SEXP items, str;
	const struct corpus_text *item;
	int i, n;
	struct termset *obj;

	obj = as_termset(stermset);
	n = obj->nitem;
	PROTECT(items = allocVector(STRSXP, n));

	for (i = 0; i < n; i++) {
		item = &obj->items[i];
		str = mkCharLenCE((char *)item->ptr,
				  (int)CORPUS_TEXT_SIZE(item),
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
