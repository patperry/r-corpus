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
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "corpus/src/error.h"
#include "corpus/src/memory.h"
#include "corpus/src/render.h"
#include "corpus/src/table.h"
#include "corpus/src/tree.h"
#include "corpus/src/termset.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/ngram.h"
#include "rcorpus.h"


// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_filter
#ifdef error
#  undef error
#endif


struct context {
	struct corpus_render render;
	struct corpus_termset termset;
	struct corpus_ngram *ngram;
	int *buffer;
	int *ngram_set;
	int has_render, has_termset;
	R_xlen_t has_ngram;
};

static void context_destroy(void *obj);


static void context_init(struct context *ctx, SEXP sngrams,
			 const struct termset *select, R_xlen_t ngroup)
{
	const int *ngrams;
	R_xlen_t i, n;
	int ngram_max;
	int err = 0;

	TRY(corpus_render_init(&ctx->render, CORPUS_ESCAPE_NONE));
	ctx->has_render = 1;

	if (sngrams != R_NilValue) {
		ngrams = INTEGER(sngrams);
		ngram_max = 1;
		n = XLENGTH(sngrams);

		for (i = 0; i < n; i++) {
			if (ngrams[i] == NA_INTEGER) {
				continue;
			}
			if (ngrams[i] > ngram_max) {
				ngram_max = ngrams[i];
			}
		}
	} else {
		ngrams = NULL;
		ngram_max = select ? select->max_length : 1;
	}

	ctx->buffer = (void *)R_alloc(ngram_max, sizeof(*ctx->buffer));
	ctx->ngram_set = (void *)R_alloc(ngram_max + 1,
					 sizeof(*ctx->ngram_set));
	memset(ctx->ngram_set, 0, (ngram_max + 1) * sizeof(*ctx->ngram_set));

	if (sngrams != R_NilValue) {
		for (i = 0; i < n; i++) {
			if (ngrams[i] == NA_INTEGER) {
				continue;
			}
			ctx->ngram_set[ngrams[i]] = 1;
		}
	} else {
		for (i = 0; i < ngram_max; i++) {
			ctx->ngram_set[i + 1] = 1;
		}
	}

	if (ngroup > 0) {
		TRY_ALLOC(ctx->ngram = corpus_malloc(ngroup
					             * sizeof(*ctx->ngram)));
	}

	while (ctx->has_ngram < ngroup) {
		TRY(corpus_ngram_init(&ctx->ngram[ctx->has_ngram], ngram_max));
		ctx->has_ngram++;
	}

	if (!select) {
		TRY(corpus_termset_init(&ctx->termset));
		ctx->has_termset = 1;
	}
out:
	if (err) {
		context_destroy(ctx);
	}
	CHECK_ERROR(err);
}

static void context_destroy(void *obj)
{
	struct context *ctx = obj;

	if (ctx->has_render) {
		corpus_render_destroy(&ctx->render);
	}

	if (ctx->has_termset) {
		corpus_termset_destroy(&ctx->termset);
	}

	while (ctx->has_ngram-- > 0) {
		corpus_ngram_destroy(&ctx->ngram[ctx->has_ngram]);
	}

	corpus_free(ctx->ngram);
}


SEXP term_matrix_text(SEXP sx, SEXP sprops, SEXP sweights, SEXP sngrams,
		      SEXP sselect, SEXP sgroup)
{
	SEXP ans = R_NilValue, sctx, snames, si, sj, scount, stext, sfilter,
	     scol_names, srow_names, sterm;
	struct context *ctx;
	const struct corpus_text *text, *type;
	struct corpus_filter *filter;
	const struct termset *select;
	const struct corpus_termset *terms;
	const double *weights;
	const int *type_ids;
	const int *group;
	double w;
	struct corpus_ngram_iter it;
	R_xlen_t i, n, g, ngroup, nz, off;
	int err = 0, j, m, term_id, type_id, nprot = 0;

	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);

	PROTECT(sfilter = alloc_filter(sprops)); nprot++;
	filter = as_filter(sfilter);

	if (sngrams != R_NilValue) {
		PROTECT(sngrams = coerceVector(sngrams, INTSXP)); nprot++;
	}

	select = NULL;
	if (sselect != R_NilValue) {
		PROTECT(sselect = alloc_termset(sselect, "select", filter, 0));
		nprot++;
		select = as_termset(sselect);
	}

	weights = as_weights(sweights, n);
	if (sgroup != R_NilValue) {
		srow_names = getAttrib(sgroup, R_LevelsSymbol);
		ngroup = XLENGTH(srow_names);
		group = INTEGER(sgroup);
	} else {
		srow_names = names_text(sx);
		ngroup = n;
		group = NULL;
	}

	PROTECT(sctx = alloc_context(sizeof(*ctx), context_destroy)); nprot++;
        ctx = as_context(sctx);
	context_init(ctx, sngrams, select, ngroup);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!group) {
			g = i;
		} else if (group[i] == NA_INTEGER) {
			continue;
		} else {
			assert(0 < group[i] && group[i] <= ngroup);
			g = (R_xlen_t)(group[i] - 1);
		}

		w = weights ? weights[i] : 1;

		TRY(corpus_filter_start(filter, &text[i],
					CORPUS_FILTER_SCAN_TOKENS));

		while (corpus_filter_advance(filter)) {
			type_id = filter->type_id;
			if (type_id == CORPUS_FILTER_IGNORED) {
				continue;
			} else if (type_id < 0) {
				TRY(corpus_ngram_break(&ctx->ngram[g]));
				continue;
			}

			TRY(corpus_ngram_add(&ctx->ngram[g], type_id, w));
		}
		TRY(filter->error);

		TRY(corpus_ngram_break(&ctx->ngram[g]));
	}

	nz = 0;

	for (g = 0; g < ngroup; g++) {
		RCORPUS_CHECK_INTERRUPT(g);

		corpus_ngram_iter_make(&it, &ctx->ngram[g], ctx->buffer);
		while (corpus_ngram_iter_advance(&it)) {
			if (!ctx->ngram_set[it.length]) {
				continue;
			}

			if (select) {
			       if (!corpus_termset_has(&select->set,
						       it.type_ids,
						       it.length, NULL)) {
				       continue;
			       }
			} else {
				TRY(corpus_termset_add(&ctx->termset,
						       it.type_ids,
						       it.length, NULL));
			}

			TRY(nz == R_XLEN_T_MAX ? CORPUS_ERROR_OVERFLOW : 0);
			nz++;
		}
	}

	PROTECT(si = allocVector(REALSXP, nz)); nprot++;
	PROTECT(sj = allocVector(INTSXP, nz)); nprot++;
	PROTECT(scount = allocVector(REALSXP, nz)); nprot++;

	off = 0;
	terms = select ? &select->set : &ctx->termset;
	for (g = 0; g < ngroup; g++) {
		RCORPUS_CHECK_INTERRUPT(g);

		corpus_ngram_iter_make(&it, &ctx->ngram[g], ctx->buffer);
		while (corpus_ngram_iter_advance(&it)) {
			if (!ctx->ngram_set[it.length]) {
				continue;
			}

			if (!corpus_termset_has(terms, it.type_ids,
						it.length, &term_id)) {
				continue;
			}

			REAL(si)[off] = (double)g;
			INTEGER(sj)[off] = term_id;
			REAL(scount)[off] = it.weight;
			off++;
		}
	}

	PROTECT(scol_names = allocVector(STRSXP, terms->nitem));
	nprot++;

	for (i = 0; i < terms->nitem; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		type_ids = terms->items[i].type_ids;
		m = terms->items[i].length;

		for (j = 0; j < m; j++) {
			type = corpus_filter_type(filter, type_ids[j]);
			if (j > 0) {
				corpus_render_char(&ctx->render, ' ');
			}
			corpus_render_text(&ctx->render, type);
		}
		TRY(ctx->render.error);

		sterm = mkCharLenCE(ctx->render.string, ctx->render.length,
				    CE_UTF8);
		corpus_render_clear(&ctx->render);

		SET_STRING_ELT(scol_names, i, sterm);
	}

	PROTECT(ans = allocVector(VECSXP, 5)); nprot++;
	SET_VECTOR_ELT(ans, 0, si);
	SET_VECTOR_ELT(ans, 1, sj);
	SET_VECTOR_ELT(ans, 2, scount);
	SET_VECTOR_ELT(ans, 3, srow_names);
	SET_VECTOR_ELT(ans, 4, scol_names);

	PROTECT(snames = allocVector(STRSXP, 5)); nprot++;
	SET_STRING_ELT(snames, 0, mkChar("i"));
	SET_STRING_ELT(snames, 1, mkChar("j"));
	SET_STRING_ELT(snames, 2, mkChar("count"));
	SET_STRING_ELT(snames, 3, mkChar("row_names"));
	SET_STRING_ELT(snames, 4, mkChar("col_names"));
	setAttrib(ans, R_NamesSymbol, snames);

out:
	CHECK_ERROR(err);
	free_context(sctx);
	UNPROTECT(nprot);
	return ans;
}
