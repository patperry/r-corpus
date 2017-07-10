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
#include <string.h>
#include "corpus/src/error.h"
#include "corpus/src/memory.h"
#include "corpus/src/render.h"
#include "corpus/src/table.h"
#include "corpus/src/tree.h"
#include "corpus/src/termset.h"
#include "corpus/src/ngram.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "rcorpus.h"


// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_filter
#ifdef error
#  undef error
#endif

struct context {
	int ngram_max;
	int *buffer;
	int *ngram_set;
	double *support;
	struct corpus_render render;
	struct corpus_ngram ngram_loc;
	struct corpus_ngram ngram_tot;
	struct corpus_termset termset;
	int has_render;
	int has_ngram_loc;
	int has_ngram_tot;
	int has_termset;
};

#define CHECK_ERROR(err) \
	do { \
		if (err) { \
			Rf_error("failed computing term counts"); \
		} \
	} while (0)

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
			if (ngrams[i] == NA_INTEGER) {
				continue;
			}
			if (ngrams[i] > ngram_max) {
				ngram_max = ngrams[i];
			}
		}

		ngram_set = (void *)R_alloc(ngram_max + 1, sizeof(*ngram_set));
		memset(ngram_set, 0, (ngram_max + 1) * sizeof(*ngram_set));
		for (i = 0; i < n; i++) {
			if (ngrams[i] == NA_INTEGER) {
				continue;
			}
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

	TRY(corpus_render_init(&ctx->render, CORPUS_ESCAPE_NONE));
	ctx->has_render = 1;

	TRY(corpus_ngram_init(&ctx->ngram_loc, ngram_max));
	ctx->has_ngram_loc = 1;

	TRY(corpus_ngram_init(&ctx->ngram_tot, ngram_max));
	ctx->has_ngram_tot = 1;

	TRY(corpus_termset_init(&ctx->termset));
	ctx->has_termset = 1;

out:
	CHECK_ERROR(err);
}


static void context_destroy(void *obj)
{
	struct context *ctx = obj;

	corpus_free(ctx->support);

	if (ctx->has_termset) {
		corpus_termset_destroy(&ctx->termset);
	}
	if (ctx->has_ngram_tot) {
		corpus_ngram_destroy(&ctx->ngram_tot);
	}
	if (ctx->has_ngram_loc) {
		corpus_ngram_destroy(&ctx->ngram_loc);
	}
	if (ctx->has_render) {
		corpus_render_destroy(&ctx->render);
	}
}


static void context_update(struct context *ctx, double weight)
{
	struct corpus_ngram_iter it;
	size_t size;
	double *support;
	int term_id = -1, nterm, nterm_max;
	int err = 0;

	corpus_ngram_iter_make(&it, &ctx->ngram_loc, ctx->buffer);
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
				size = nterm_max * sizeof(*support);
				TRY_ALLOC(support
					= corpus_realloc(ctx->support, size));
				ctx->support = support;
			}
			while (nterm < ctx->termset.nitem) {
				ctx->support[nterm] = 0;
				nterm++;
			}
		}
		ctx->support[term_id] += weight;
	}
	corpus_ngram_clear(&ctx->ngram_loc);
out:
	CHECK_ERROR(err);
}


static double context_support(const struct context *ctx,
			      const int *type_ids, int length)
{
	int term_id;

	if (!corpus_termset_has(&ctx->termset, type_ids, length, &term_id)) {
		return 0;
	}

	return ctx->support[term_id];
}


SEXP term_counts_text(SEXP sx, SEXP sprops, SEXP sweights, SEXP sngrams,
		      SEXP smin_count, SEXP smax_count, SEXP smin_support,
		      SEXP smax_support, SEXP soutput_types)
{
	SEXP ans, sctx, stype, sterm, scount, ssupport, stext, sfilter,
	     sclass, snames, srow_names;
	SEXP *stypes;
	struct context *ctx;
	const struct corpus_text *text, *type;
	struct mkchar mkchar;
	struct corpus_ngram_iter it;
	struct corpus_filter *filter;
	const double *weights;
	int w;
	double wt, supp, min_count, max_count, min_support, max_support;
	R_xlen_t i, n, nterm;
	int output_types;
	int type_id, err = 0, nprot = 0;
	int restrict_count, restrict_support;

	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);

	PROTECT(sfilter = alloc_filter(sprops)); nprot++;
	filter = as_filter(sfilter);

	if (sngrams != R_NilValue) {
		PROTECT(sngrams = coerceVector(sngrams, INTSXP)); nprot++;
	}

	weights = as_weights(sweights, n);

	min_count = REAL(smin_count)[0];
	max_count = REAL(smax_count)[0];
	restrict_count = min_count > 0 || max_count < INFINITY;

	min_support = REAL(smin_support)[0];
	max_support = REAL(smax_support)[0];
	restrict_support = min_support > 0 || max_support < INFINITY;

	output_types = (LOGICAL(soutput_types)[0] == TRUE);

	PROTECT(sctx = alloc_context(sizeof(*ctx), context_destroy)); nprot++;
        ctx = as_context(sctx);
	context_init(ctx, sngrams);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		wt = weights ? weights[i] : 1;
		if (wt == 0) {
			continue;
		}

		// add the text[i] n-grams
		TRY(corpus_filter_start(filter, &text[i],
					CORPUS_FILTER_SCAN_TOKENS));

		while (corpus_filter_advance(filter)) {
			type_id = filter->type_id;
			if (type_id == CORPUS_FILTER_IGNORED) {
				continue;
			} else if (type_id < 0) {
				TRY(corpus_ngram_break(&ctx->ngram_loc));
				TRY(corpus_ngram_break(&ctx->ngram_tot));
				continue;
			}

			TRY(corpus_ngram_add(&ctx->ngram_loc, type_id, wt));
			TRY(corpus_ngram_add(&ctx->ngram_tot, type_id, wt));
		}
		TRY(filter->error);

		TRY(corpus_ngram_break(&ctx->ngram_loc));
		TRY(corpus_ngram_break(&ctx->ngram_tot));

		context_update(ctx, wt);
	}

	nterm = 0;
	corpus_ngram_iter_make(&it, &ctx->ngram_tot, ctx->buffer);
	while (corpus_ngram_iter_advance(&it)) {
		RCORPUS_CHECK_INTERRUPT(nterm);

		if (!ctx->ngram_set[it.length]) {
			continue;
		}

		if (restrict_count) {
			wt = it.weight;
			if (!(min_count <= wt && wt <= max_count)) {
				continue;
			}
		}

		if (restrict_support) {
			supp = context_support(ctx, it.type_ids, it.length);
			if (!(min_support <= supp && supp <= max_support)) {
				continue;
			}
		}

		if (nterm == R_XLEN_T_MAX) {
			err = CORPUS_ERROR_OVERFLOW;
			corpus_log(err, "number of terms exceeds maximum"
				   " (%"PRIu64")",
				   (uint64_t)R_XLEN_T_MAX);
		}
		nterm++;
	}

	PROTECT(sterm = allocVector(STRSXP, nterm)); nprot++;
	if (output_types) {
		stypes = (void *)R_alloc(ctx->ngram_max, sizeof(*stypes));
		for (w = 0; w < ctx->ngram_max; w++) {
			PROTECT(stypes[w] = allocVector(STRSXP, nterm));
			nprot++;
			for (i = 0; i < nterm; i++) {
				SET_STRING_ELT(stypes[w], i, NA_STRING);
			}
		}
	} else {
		stypes = NULL;
	}

	PROTECT(scount = allocVector(REALSXP, nterm)); nprot++;
	PROTECT(ssupport = allocVector(REALSXP, nterm)); nprot++;

	mkchar_init(&mkchar);
	i = 0;
	
	corpus_ngram_iter_make(&it, &ctx->ngram_tot, ctx->buffer);
	while (corpus_ngram_iter_advance(&it)) {
		RCORPUS_CHECK_INTERRUPT(i);

		wt = it.weight;
		supp = context_support(ctx, it.type_ids, it.length);

		if (!ctx->ngram_set[it.length]) {
			continue;
		}

		if (restrict_count) {
			if (!(min_count <= wt && wt <= max_count)) {
				continue;
			}
		}

		if (restrict_support) {
			if (!(min_support <= supp && supp <= max_support)) {
				continue;
			}
		}

		if (it.length == 1) {
			type_id = it.type_ids[0];
			type = corpus_filter_type(filter, type_id);
			stype = mkchar_get(&mkchar, type);
			SET_STRING_ELT(sterm, i, stype);
			if (output_types) {
				SET_STRING_ELT(stypes[0], i, stype);
			}
		} else {
			for (w = 0; w < it.length; w++) {
				type_id = it.type_ids[w];
				type = corpus_filter_type(filter, type_id);
				if (output_types) {
					stype = mkchar_get(&mkchar, type);
					SET_STRING_ELT(stypes[w], i, stype);
				}

				if (w > 0) {
					corpus_render_char(&ctx->render, ' ');
				}
				corpus_render_text(&ctx->render, type);
			}

			TRY(ctx->render.error);

			SET_STRING_ELT(sterm, i,
				       mkCharLenCE(ctx->render.string,
					           ctx->render.length,
						   CE_UTF8));
			corpus_render_clear(&ctx->render);
		}

		REAL(scount)[i] = wt;
		REAL(ssupport)[i] = supp;
		i++;
	}

	if (output_types) {
		PROTECT(ans = allocVector(VECSXP, 3 + ctx->ngram_max)); nprot++;
		SET_VECTOR_ELT(ans, 0, sterm);
		for (w = 0; w < ctx->ngram_max; w++) {
			SET_VECTOR_ELT(ans, w + 1, stypes[w]);
		}
		SET_VECTOR_ELT(ans, ctx->ngram_max + 1, scount);
		SET_VECTOR_ELT(ans, ctx->ngram_max + 2, ssupport);

		PROTECT(snames = allocVector(STRSXP, 3 + ctx->ngram_max));
		nprot++;

		SET_STRING_ELT(snames, 0, mkChar("term"));
		for (w = 0; w < ctx->ngram_max; w++) {
			corpus_render_printf(&ctx->render, "type%d", w + 1);
			TRY(ctx->render.error);
			SET_STRING_ELT(snames, w + 1,
				       mkChar(ctx->render.string));
			corpus_render_clear(&ctx->render);
		}
		SET_STRING_ELT(snames, ctx->ngram_max + 1, mkChar("count"));
		SET_STRING_ELT(snames, ctx->ngram_max + 2, mkChar("support"));
	} else {
		PROTECT(ans = allocVector(VECSXP, 3)); nprot++;
		SET_VECTOR_ELT(ans, 0, sterm);
		SET_VECTOR_ELT(ans, 1, scount);
		SET_VECTOR_ELT(ans, 2, ssupport);

		PROTECT(snames = allocVector(STRSXP, 3)); nprot++;
		SET_STRING_ELT(snames, 0, mkChar("term"));
		SET_STRING_ELT(snames, 1, mkChar("count"));
		SET_STRING_ELT(snames, 2, mkChar("support"));
	}
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
