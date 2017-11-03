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
#include <stdlib.h>
#include <string.h>
#include "corpus/src/table.h"
#include "corpus/src/intset.h"
#include "rcorpus.h"


struct types_context {
	SEXP names;
	struct corpus_filter *filter;
	struct corpus_intset *types;
	int *is_na;
	R_xlen_t ngroup;
	int collapse;
};


static void types_context_init(struct types_context *ctx, SEXP sx,
			       SEXP scollapse)
{
	const struct utf8lite_text *text;
	R_xlen_t i, n, g, ngroup;
	int err = 0;

	text = as_text(sx, &n);
	ctx->filter = text_filter(sx);

	ctx->collapse = LOGICAL(scollapse)[0] == TRUE;
	ngroup = ctx->collapse ? 1 : n;
	ctx->names = ctx->collapse ? R_NilValue : names_text(sx);

	ctx->is_na = (void *)R_alloc(ngroup, sizeof(*ctx->is_na));
	memset(ctx->is_na, 0, ngroup * sizeof(*ctx->is_na));

	ctx->types = (void *)R_alloc(ngroup, sizeof(*ctx->types));
	for (g = 0; g < ngroup; g++) {
		RCORPUS_CHECK_INTERRUPT(g);
		TRY(corpus_intset_init(&ctx->types[g]));
		ctx->ngroup = g + 1;
	}

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		g = ctx->collapse ? 0 : i;

		if (!text[i].ptr) { // missing text
			ctx->is_na[g] = 1;
			continue;
		}

		TRY(corpus_filter_start(ctx->filter, &text[i]));

		while (corpus_filter_advance(ctx->filter)) {
			if (ctx->filter->type_id < 0) {
				// skip ignored and dropped tokens
				continue;
			}

			TRY(corpus_intset_add(&ctx->types[g],
					      ctx->filter->type_id, NULL));
		}

		TRY(ctx->filter->error);
	}
out:
	if (err) {
		Rf_error("failed initializing text_types context");
	}
}


static void types_context_destroy(void *obj)
{
	struct types_context *ctx = obj;
	R_xlen_t g = ctx->ngroup;

	while (g-- > 0) {
		corpus_intset_destroy(&ctx->types[g]);
	}
}


SEXP text_ntype(SEXP sx, SEXP scollapse)
{
	SEXP ans, sctx;
	struct types_context *ctx;
	double *count;
	R_xlen_t g;
	int nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;

	PROTECT(sctx = alloc_context(sizeof(*ctx), types_context_destroy));
	nprot++;

	ctx = as_context(sctx);
	types_context_init(ctx, sx, scollapse);

	PROTECT(ans = allocVector(REALSXP, ctx->ngroup)); nprot++;
	setAttrib(ans, R_NamesSymbol, ctx->names);
	count = REAL(ans);

	for (g = 0; g < ctx->ngroup; g++) {
		RCORPUS_CHECK_INTERRUPT(g);

		if (ctx->is_na[g]) {
			count[g] = NA_REAL;
		} else {
			count[g] = (double)ctx->types[g].nitem;
		}
	}

	free_context(sctx);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_types(SEXP sx, SEXP scollapse)
{
	SEXP ans, sctx, set;
	const struct utf8lite_text *type;
	struct types_context *ctx;
	const struct corpus_intset *types;
	struct mkchar mkchar;
	R_xlen_t g;
	int i, n, type_id, nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;

	PROTECT(sctx = alloc_context(sizeof(*ctx), types_context_destroy));
	nprot++;
	ctx = as_context(sctx);
	types_context_init(ctx, sx, scollapse);

	mkchar_init(&mkchar);

	if (ctx->collapse) {
		ans = R_NilValue;
	} else {
		PROTECT(ans = allocVector(VECSXP, ctx->ngroup)); nprot++;
		setAttrib(ans, R_NamesSymbol, ctx->names);
	}

	for (g = 0; g < ctx->ngroup; g++) {
		RCORPUS_CHECK_INTERRUPT(g);

		types = &ctx->types[g];
		n = types->nitem;
		PROTECT(set = allocVector(STRSXP, n)); nprot++;

		for (i = 0; i < n; i++) {
			type_id = types->items[i];
			type = &ctx->filter->symtab.types[type_id].text;
			SET_STRING_ELT(set, i, mkchar_get(&mkchar, type));
		}

		if (ctx->collapse) {
			PROTECT(ans = set); nprot++;
		} else {
			SET_VECTOR_ELT(ans, g, set);
		}
		UNPROTECT(1); nprot--; // 'set' is protected by ans
	}

	free_context(sctx);
	UNPROTECT(nprot);
	return ans;
}
