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

#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include "rcorpus.h"


struct context {
	struct utf8lite_text *block;
	R_xlen_t *parent;
	R_xlen_t nblock;
	R_xlen_t nblock_max;
};


static void context_destroy(void *obj)
{
        struct context *ctx = obj;
	corpus_free(ctx->block);
	corpus_free(ctx->parent);
}


static void context_grow(struct context *ctx, size_t nadd)
{
	struct utf8lite_text *block;
	R_xlen_t *parent;
	size_t count, size, width;
	int err = 0;

	count = (size_t)ctx->nblock;
	size = (size_t)ctx->nblock_max;
	width = (sizeof(*block) < sizeof(*parent)
			? sizeof(*parent)
			: sizeof(*block));

	if (nadd <= size && count <= size - nadd) {
		return;
	}

	TRY(corpus_bigarray_size_add(&size, width, count, nadd));

	TRY_ALLOC(block = corpus_realloc(ctx->block, size * sizeof(*block)));
	ctx->block = block;

	TRY_ALLOC(parent = corpus_realloc(ctx->parent, size * sizeof(*parent)));
	ctx->parent = parent;

	ctx->nblock_max = (R_xlen_t)size;
out:
	CHECK_ERROR(err);
}


static void context_add(struct context *ctx, const struct utf8lite_text *block,
			R_xlen_t parent)
{
	R_xlen_t nblock = ctx->nblock;

	if (nblock == ctx->nblock_max) {
		context_grow(ctx, 1);
	}

	ctx->block[nblock] = *block;
	ctx->parent[nblock] = parent;
	ctx->nblock = nblock + 1;
}


static void context_trim(struct context *ctx)
{
	struct utf8lite_text *block;
	R_xlen_t *parent;
	size_t size = (size_t)ctx->nblock;

	if (size == 0) {
		corpus_free(ctx->block);
		ctx->block = NULL;

		corpus_free(ctx->parent);
		ctx->parent = NULL;
	} else {
		block = corpus_realloc(ctx->block, size * sizeof(*block));
		if (block) {
			ctx->block = block;
		}

		parent = corpus_realloc(ctx->parent, size * sizeof(*parent));
		if (parent) {
			ctx->parent = parent;
		}
	}

	ctx->nblock_max = size;
}


static SEXP context_make(struct context *ctx, SEXP sx)
{
	SEXP ans, handle, sources, psource, prow, pstart, ptable, source,
	     row, start, stop, index, sparent, stext, names, filter,
	     sclass, row_names;
	struct rcorpus_text *obj;
	R_xlen_t src, i, iblock, nblock;
	double r;
	int err = 0, j, off, len, nprot;

	context_trim(ctx);

	nprot = 0;
	nblock = ctx->nblock;

	filter = filter_text(sx);
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");

	PROTECT(source = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(row = allocVector(REALSXP, nblock)); nprot++;
	PROTECT(start = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(stop = allocVector(INTSXP, nblock)); nprot++;
	PROTECT(sparent = allocVector(REALSXP, nblock)); nprot++;
	PROTECT(index = allocVector(INTSXP, nblock)); nprot++;

	i = -1;
	j = 0;
	off = 0;
	r = NA_REAL;
	src = NA_INTEGER;

	for (iblock = 0; iblock < nblock; iblock++) {
		RCORPUS_CHECK_INTERRUPT(iblock);

		if (ctx->parent[iblock] != i) {
			i = ctx->parent[iblock];
			j = 0;
			src = INTEGER(psource)[i];
			r = REAL(prow)[i];
			off = INTEGER(pstart)[i];
		}
		len = (int)UTF8LITE_TEXT_SIZE(&ctx->block[iblock]);

		INTEGER(source)[iblock] = src;
		REAL(row)[iblock] = r;
		INTEGER(start)[iblock] = off;
		INTEGER(stop)[iblock] = off + (len - 1);
		INTEGER(index)[iblock] = j + 1;
		REAL(sparent)[iblock] = (double)i + 1;

		j++;
		off += len;
	}
	corpus_free(ctx->parent);
	ctx->parent = NULL;
	ctx->nblock = 0;
	ctx->nblock_max = 0;

	PROTECT(stext = alloc_text(sources, source, row, start, stop,
				   R_NilValue, filter));
	nprot++;

	handle = getListElement(stext, "handle");
	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));
	R_SetExternalPtrAddr(handle, obj);

	obj->text = ctx->block;
	obj->length = nblock;
	ctx->block = NULL;

	PROTECT(ans = allocVector(VECSXP, 3)); nprot++;
	SET_VECTOR_ELT(ans, 0, sparent);
	SET_VECTOR_ELT(ans, 1, index);
	SET_VECTOR_ELT(ans, 2, stext);

	PROTECT(names = allocVector(STRSXP, 3)); nprot++;
	SET_STRING_ELT(names, 0, mkChar("parent"));
	SET_STRING_ELT(names, 1, mkChar("index"));
	SET_STRING_ELT(names, 2, mkChar("text"));
	setAttrib(ans, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2)); nprot++;
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)nblock;
	setAttrib(ans, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 2)); nprot++;
        SET_STRING_ELT(sclass, 0, mkChar("corpus_frame"));
        SET_STRING_ELT(sclass, 1, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);

out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_split_sentences(SEXP sx, SEXP ssize)
{
	SEXP ans, sctx, snsent;
	struct context *ctx;
	struct corpus_sentfilter *filter;
	const struct utf8lite_text *text;
	struct utf8lite_text current;
	R_xlen_t i, n;
	size_t attr, size;
	double s, block_size, nsent, nbin, min_size, extra, target;
	int nprot, err = 0;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_sentfilter(sx);

	// size
        PROTECT(ssize = coerceVector(ssize, REALSXP)); nprot++;
	block_size = REAL(ssize)[0];
	if (!(block_size >= 1)) {
		block_size = 1;
	}

	if (block_size != 1) {
		PROTECT(snsent = text_nsentence(sx)); nprot++;
	} else {
		snsent = R_NilValue;
		extra = 0;
		target = 1;
	}

	PROTECT(sctx = alloc_context(sizeof(*ctx), context_destroy)); nprot++;
        ctx = as_context(sctx);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) { // missing value
			continue;
		}

		if (UTF8LITE_TEXT_SIZE(&text[i]) == 0) { // empty text
			context_add(ctx, &text[i], i);
			continue;
		}

		if (block_size != 1) {
			nsent = REAL(snsent)[i];
			nbin = ceil(nsent / block_size);
			min_size = floor(nsent / nbin);
			extra = nsent - nbin * min_size;
			target = min_size;
			if (extra > 0) {
				target += 1;
			}
		}

		s = 0;
		size = 0;
		attr = 0;

		TRY(corpus_sentfilter_start(filter, &text[i]));
		while (corpus_sentfilter_advance(filter)) {
			if (s == 0) {
				current.ptr = filter->current.ptr;
				attr = 0;
				size = 0;
			}

			size += UTF8LITE_TEXT_SIZE(&filter->current);
			attr |= UTF8LITE_TEXT_BITS(&filter->current);
			s++;

			if (s < target) {
				continue;
			}

			current.attr = attr | size;
			context_add(ctx, &current, i);

			s = 0;

			if (block_size != 1) {
				extra -= 1;
				if (extra <= 0) {
					target = min_size;
				}
			}
		}
		TRY(filter->error);

		if (s > 0) {
			current.attr = attr | size;
			context_add(ctx, &current, i);
		}
	}

	PROTECT(ans = context_make(ctx, sx)); nprot++;
out:
        free_context(sctx);
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_split_tokens(SEXP sx, SEXP ssize)
{
	SEXP ans, sctx, sntok;
	struct context *ctx;
	struct corpus_filter *filter;
	const struct utf8lite_text *text;
	struct utf8lite_text current;
	R_xlen_t i, n;
	size_t attr, size;
	double s, block_size, ntok, nbin, min_size, extra, target;
	int nprot, err = 0;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	// size
        PROTECT(ssize = coerceVector(ssize, REALSXP)); nprot++;
	block_size = REAL(ssize)[0];
	if (!(block_size >= 1)) {
		block_size = 1;
	}

	if (block_size != 1) {
		PROTECT(sntok = text_ntoken(sx)); nprot++;
	} else {
		sntok = R_NilValue;
		extra = 0;
		target = 1;
	}

	PROTECT(sctx = alloc_context(sizeof(*ctx), context_destroy)); nprot++;
        ctx = as_context(sctx);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) { // missing value
			continue;
		}

		if (UTF8LITE_TEXT_SIZE(&text[i]) == 0) { // empty text
			context_add(ctx, &text[i], i);
			continue;
		}

		if (block_size != 1) {
			ntok = REAL(sntok)[i];
			nbin = ceil(ntok / block_size);
			min_size = floor(ntok / nbin);
			extra = ntok - nbin * min_size;
			target = min_size;
			if (extra > 0) {
				target += 1;
			}
		}

		// start with an empty block
		s = 0;
		size = 0;
		attr = 0;

		TRY(corpus_filter_start(filter, &text[i]));

		while (corpus_filter_advance(filter)) {
			// if we encounter a non-dropped, non-ignored
			// token and the block is already full, add it
			if (filter->type_id >= 0 && s >= target) {
				current.attr = attr | size;
				context_add(ctx, &current, i);
				size = 0;
				s = 0;

				if (block_size != 1) {
					extra -= 1;
					if (extra <= 0) {
						target = min_size;
					}
				}
			}

			// if the block is empty, initialize it
			if (size == 0) {
				current.ptr = filter->current.ptr;
				size = 0;
				attr = 0;
			}

			// update the block size and attributes
			size += UTF8LITE_TEXT_SIZE(&filter->current);
			attr |= UTF8LITE_TEXT_BITS(&filter->current);

			// possibly update the non-dropped token count
			if (filter->type_id >= 0) {
				s++;
			}
		}
		TRY(filter->error);

		if (size > 0) {
			current.attr = attr | size;
			context_add(ctx, &current, i);
		}
	}

	PROTECT(ans = context_make(ctx, sx)); nprot++;
out:
	free_context(sctx);
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}
