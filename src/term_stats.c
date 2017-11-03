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

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "rcorpus.h"


struct context {
	int ngram_max;
	int *buffer;
	int *ngram_set;
	double *support;
	double *count;
	struct utf8lite_render render;
	struct corpus_ngram ngram;
	struct corpus_termset termset;
	int has_render;
	int has_ngram;
	int has_termset;
};


static void context_init(struct context *ctx, SEXP sngrams)
{
	const int *ngrams;
	int *ngram_set;
	R_xlen_t i, n;
	int ngram_max;
	int err = 0;

	if (sngrams != R_NilValue) {
		ngrams = INTEGER(sngrams);
		n = XLENGTH(sngrams);
		ngram_max = 1;

		for (i = 0; i < n; i++) {
			if (ngrams[i] > ngram_max) {
				ngram_max = ngrams[i];
			}
		}

		ngram_set = (void *)R_alloc(ngram_max + 1, sizeof(*ngram_set));
		memset(ngram_set, 0, (ngram_max + 1) * sizeof(*ngram_set));
		for (i = 0; i < n; i++) {
			ngram_set[ngrams[i]] = 1;
		}
	} else {
		ngram_max = 1;
		ngram_set = (void *)R_alloc(2, sizeof(*ngram_set));
		ngram_set[0] = 0;
		ngram_set[1] = 1;
	}

	ctx->ngram_max = ngram_max;
	ctx->ngram_set = ngram_set;
	ctx->buffer = (void *)R_alloc(ngram_max, sizeof(*ctx->buffer));

	TRY(utf8lite_render_init(&ctx->render, UTF8LITE_ESCAPE_NONE));
	ctx->has_render = 1;

	TRY(corpus_ngram_init(&ctx->ngram, ngram_max));
	ctx->has_ngram = 1;

	TRY(corpus_termset_init(&ctx->termset));
	ctx->has_termset = 1;
out:
	CHECK_ERROR(err);
}


static void context_destroy(void *obj)
{
	struct context *ctx = obj;

	corpus_free(ctx->count);
	corpus_free(ctx->support);

	if (ctx->has_termset) {
		corpus_termset_destroy(&ctx->termset);
	}
	if (ctx->has_ngram) {
		corpus_ngram_destroy(&ctx->ngram);
	}
	if (ctx->has_render) {
		utf8lite_render_destroy(&ctx->render);
	}
}


static void context_update(struct context *ctx, double weight)
{
	struct corpus_ngram_iter it;
	size_t size;
	double *count;
	double *support;
	int term_id = -1, nterm, nterm_max;
	int err = 0;

	corpus_ngram_iter_make(&it, &ctx->ngram, ctx->buffer);
	while (corpus_ngram_iter_advance(&it)) {
		if (!ctx->ngram_set[it.length]) {
			continue;
		}

		if (!corpus_termset_has(&ctx->termset, it.type_ids,
					it.length, &term_id)) {

			nterm = ctx->termset.nitem;
			nterm_max = ctx->termset.nitem_max;
			TRY(corpus_termset_add(&ctx->termset, it.type_ids,
					       it.length, &term_id));

			if (ctx->termset.nitem_max != nterm_max) {
				nterm_max = ctx->termset.nitem_max;

				size = nterm_max * sizeof(*count);
				TRY_ALLOC(count
					= corpus_realloc(ctx->count, size));
				ctx->count = count;

				size = nterm_max * sizeof(*support);
				TRY_ALLOC(support
					= corpus_realloc(ctx->support, size));
				ctx->support = support;
			}
			while (nterm < ctx->termset.nitem) {
				ctx->count[nterm] = 0;
				ctx->support[nterm] = 0;
				nterm++;
			}
		}
		ctx->count[term_id] += it.weight;
		ctx->support[term_id] += weight;
	}
	corpus_ngram_clear(&ctx->ngram);
out:
	CHECK_ERROR(err);
}


SEXP term_stats(SEXP sx, SEXP sngrams, SEXP smin_count, SEXP smax_count,
		SEXP smin_support, SEXP smax_support, SEXP soutput_types)
{
	SEXP ans, sctx, sterm, scount, ssupport, stext,
	     sclass, snames, srow_names, stype = NA_STRING;
	SEXP *stypes;
	struct context *ctx;
	const struct utf8lite_text *text, *type = NULL;
	const struct corpus_termset_term *term;
	struct mkchar mkchar;
	struct corpus_filter *filter;
	double count, supp, min_count, max_count, min_support, max_support;
	R_xlen_t i, n, iterm, nterm;
	int output_types;
	int off, len, j, type_id, err = 0, nprot = 0;

	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);
	filter = text_filter(stext);

	if (sngrams != R_NilValue) {
		PROTECT(sngrams = coerceVector(sngrams, INTSXP)); nprot++;
	}

	min_count = smin_count == R_NilValue ? -INFINITY : REAL(smin_count)[0];
	max_count = smax_count == R_NilValue ? INFINITY : REAL(smax_count)[0];

	min_support = (smin_support == R_NilValue ? -INFINITY
						  : REAL(smin_support)[0]);
	max_support = (smax_support == R_NilValue ? INFINITY
						  : REAL(smax_support)[0]);

	output_types = (LOGICAL(soutput_types)[0] == TRUE);

	PROTECT(sctx = alloc_context(sizeof(*ctx), context_destroy)); nprot++;
        ctx = as_context(sctx);
	context_init(ctx, sngrams);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		TRY(corpus_filter_start(filter, &text[i]));

		while (corpus_filter_advance(filter)) {
			type_id = filter->type_id;

			if (type_id == CORPUS_TYPE_NONE) {
				continue;
			} else if (type_id < 0) {
				TRY(corpus_ngram_break(&ctx->ngram));
				continue;
			}

			TRY(corpus_ngram_add(&ctx->ngram, type_id, 1));
		}
		TRY(filter->error);

		TRY(corpus_ngram_break(&ctx->ngram));
		context_update(ctx, 1);
	}

	nterm = 0;
	for (i = 0; i < ctx->termset.nitem; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		term = &ctx->termset.items[i];
		count = ctx->count[i];
		supp = ctx->support[i];

		if (!(min_count <= count && count <= max_count)) {
			continue;
		}

		if (!(min_support <= supp && supp <= max_support)) {
			continue;
		}

		if (nterm == R_XLEN_T_MAX) {
			err = CORPUS_ERROR_OVERFLOW;
			Rf_error("number of terms exceeds maximum (%"PRIu64")",
				 (uint64_t)R_XLEN_T_MAX);
			goto out;
		}
		nterm++;
	}

	PROTECT(sterm = allocVector(STRSXP, nterm)); nprot++;
	if (output_types) {
		stypes = (void *)R_alloc(ctx->ngram_max, sizeof(*stypes));
		for (j = 0; j < ctx->ngram_max; j++) {
			PROTECT(stypes[j] = allocVector(STRSXP, nterm));
			nprot++;
			for (i = 0; i < nterm; i++) {
				SET_STRING_ELT(stypes[j], i, NA_STRING);
			}
		}
	} else {
		stypes = NULL;
	}

	PROTECT(scount = allocVector(REALSXP, nterm)); nprot++;
	PROTECT(ssupport = allocVector(REALSXP, nterm)); nprot++;

	mkchar_init(&mkchar);
	iterm = 0;
	
	for (i = 0; i < ctx->termset.nitem; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		term = &ctx->termset.items[i];
		count = ctx->count[i];
		supp = ctx->support[i];

		if (!(min_count <= count && count <= max_count)) {
			continue;
		}

		if (!(min_support <= supp && supp <= max_support)) {
			continue;
		}

		assert(term->length <= ctx->ngram_max);

		for (j = 0; j < term->length; j++) {
			type_id = term->type_ids[j];
			type = &filter->symtab.types[type_id].text;

			if (output_types) {
				stype = mkchar_get(&mkchar, type);
				SET_STRING_ELT(stypes[j], iterm, stype);
			}

			if (j > 0) {
				utf8lite_render_char(&ctx->render, ' ');
			}

			if (term->length > 1) {
				utf8lite_render_text(&ctx->render, type);
			}
		}

		if (term->length == 1) {
			if (!output_types) {
				stype = mkchar_get(&mkchar, type);
			}
			SET_STRING_ELT(sterm, iterm, stype);
		} else {
			TRY(ctx->render.error);
			SET_STRING_ELT(sterm, iterm,
				       mkCharLenCE(ctx->render.string,
					           ctx->render.length,
						   CE_UTF8));
			utf8lite_render_clear(&ctx->render);
		}

		REAL(scount)[iterm] = count;
		REAL(ssupport)[iterm] = supp;
		iterm++;
	}

	len = 3 + (output_types ? ctx->ngram_max : 0);
	off = 0;

	PROTECT(ans = allocVector(VECSXP, len)); nprot++;
	PROTECT(snames = allocVector(STRSXP, len)); nprot++;

	SET_VECTOR_ELT(ans, off, sterm);
	SET_STRING_ELT(snames, off, mkChar("term"));
	off++;

	if (output_types) {
		for (j = 0; j < ctx->ngram_max; j++) {
			SET_VECTOR_ELT(ans, off, stypes[j]);

			utf8lite_render_printf(&ctx->render, "type%d", j + 1);
			TRY(ctx->render.error);
			SET_STRING_ELT(snames, off, mkChar(ctx->render.string));
			utf8lite_render_clear(&ctx->render);

			off++;
		}
	}

	SET_VECTOR_ELT(ans, off, scount);
	SET_STRING_ELT(snames, off, mkChar("count"));
	off++;

	SET_VECTOR_ELT(ans, off, ssupport);
	SET_STRING_ELT(snames, off, mkChar("support"));
	off++;

	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(srow_names = allocVector(REALSXP, 2)); nprot++;
	REAL(srow_names)[0] = NA_REAL;
	REAL(srow_names)[1] = -(double)nterm;
	setAttrib(ans, R_RowNamesSymbol, srow_names);

	PROTECT(sclass = allocVector(STRSXP, 2)); nprot++;
	SET_STRING_ELT(sclass, 0, mkChar("corpus_frame"));
	SET_STRING_ELT(sclass, 1, mkChar("data.frame"));
	setAttrib(ans, R_ClassSymbol, sclass);

out:
	CHECK_ERROR(err);
        free_context(sctx);
	UNPROTECT(nprot);
	return ans;
}
