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
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"

// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_sentfilter
#ifdef error
#  undef error
#endif


struct types_context {
	SEXP names;
	R_xlen_t ngroup;
	struct corpus_filter *filter;
	struct corpus_intset *types;
	int *is_na;
	int collapse;
	int nprot;
};


static void types_context_init(struct types_context *ctx, SEXP sx,
			       SEXP sfilter, SEXP sweights, SEXP sgroup,
			       SEXP scollapse);
static void types_context_destroy(struct types_context *ctx);


#define BAIL(msg) \
	do { \
		types_context_destroy(ctx); \
		Rf_error(msg); \
	} while (0)

void types_context_init(struct types_context *ctx, SEXP sx, SEXP sfilter,
			SEXP sweights, SEXP sgroup, SEXP scollapse)
{
	const struct corpus_text *text;
	const double *weights;
	double w;
	const int *group;
	R_xlen_t i, n, g;
	int err;

	ctx->nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); ctx->nprot++;
	text = as_text(sx, &n);

	// filter
	PROTECT(sfilter = alloc_filter(sfilter)); ctx->nprot++;
	ctx->filter = as_filter(sfilter);

	// weights
	weights = as_weights(sweights, n);

	// group
	if (sgroup != R_NilValue) {
		ctx->names = getAttrib(sgroup, R_LevelsSymbol);
		ctx->ngroup = XLENGTH(ctx->names);
		group = INTEGER(sgroup);
	} else {
		ctx->names = names_text(sx);
		ctx->ngroup = n;
		group = NULL;
	}

	ctx->collapse = LOGICAL(scollapse)[0] == TRUE;
	if (ctx->collapse) {
		ctx->names = R_NilValue;
		ctx->ngroup = 1;
		group = NULL;
	}

	ctx->is_na = (void *)R_alloc(ctx->ngroup, sizeof(*ctx->is_na));
	memset(ctx->is_na, 0, ctx->ngroup * sizeof(*ctx->is_na));

	ctx->types = (void *)R_alloc(ctx->ngroup, sizeof(*ctx->types));
	for (g = 0; g < ctx->ngroup; g++) {
		if ((err = corpus_intset_init(&ctx->types[g]))) {
			while (g-- > 0) {
				corpus_intset_destroy(&ctx->types[g]);
			}
			Rf_error("memory allocation failure");
		}
	}

	for (i = 0; i < n; i++) {
		w = weights ? weights[i] : 1;
		if (w == 0) { // no weight
			continue;
		}

		if (group && group[i] == NA_INTEGER) {
			continue;
		}
		g = ctx->collapse ? 0 : (group ? group[i] - 1: i);

		if (!text[i].ptr) { // missing text
			ctx->is_na[g] = 1;
			continue;
		}

		if ((err = corpus_filter_start(ctx->filter, &text[i],
					       CORPUS_FILTER_SCAN_TOKENS))) {
			BAIL("memory allocation failure");
		}

		while (corpus_filter_advance(ctx->filter)) {
			if (ctx->filter->type_id < 0) {
				// skip ignored and dropped tokens
				continue;
			}

			if ((err = corpus_intset_add(&ctx->types[g],
						     ctx->filter->type_id,
						     NULL))) {
				BAIL("memory allocation failure");
			}
		}

		if (ctx->filter->error) {
			BAIL("memory allocation failure");
		}
	}
}

#undef BAIL


void types_context_destroy(struct types_context *ctx)
{
	R_xlen_t g = ctx->ngroup;
	while (g-- > 0) {
		corpus_intset_destroy(&ctx->types[g]);
	}
	UNPROTECT(ctx->nprot);
}


SEXP text_ntype(SEXP sx, SEXP sfilter, SEXP sweights, SEXP sgroup,
		SEXP scollapse)
{
	SEXP ans;
	struct types_context ctx;
	double *count;
	R_xlen_t g;

	types_context_init(&ctx, sx, sfilter, sweights, sgroup, scollapse);
	PROTECT(ans = allocVector(REALSXP, ctx.ngroup));
	setAttrib(ans, R_NamesSymbol, ctx.names);
	count = REAL(ans);

	for (g = 0; g < ctx.ngroup; g++) {
		if (ctx.is_na[g]) {
			count[g] = NA_REAL;
		} else {
			count[g] = (double)ctx.types[g].nitem;
		}
	}

	types_context_destroy(&ctx);

	UNPROTECT(1);
	return ans;
}


SEXP text_types(SEXP sx, SEXP sfilter, SEXP sweights, SEXP sgroup,
		SEXP scollapse)
{
	SEXP ans, set;
	const struct corpus_text *type;
	struct types_context ctx;
	const struct corpus_intset *types;
	struct mkchar mkchar;
	R_xlen_t g;
	int i, n, type_id, nprot;

	nprot = 0;
	types_context_init(&ctx, sx, sfilter, sweights, sgroup, scollapse);
	mkchar_init(&mkchar);

	if (ctx.collapse) {
		ans = R_NilValue;
	} else {
		PROTECT(ans = allocVector(VECSXP, ctx.ngroup)); nprot++;
		setAttrib(ans, R_NamesSymbol, ctx.names);
	}

	for (g = 0; g < ctx.ngroup; g++) {
		types = &ctx.types[g];
		n = types->nitem;
		PROTECT(set = allocVector(STRSXP, n)); nprot++;

		for (i = 0; i < n; i++) {
			type_id = types->items[i];
			type = corpus_filter_type(ctx.filter, type_id);
			SET_STRING_ELT(set, i, mkchar_get(&mkchar, type));
		}

		if (ctx.collapse) {
			PROTECT(ans = set); nprot++;
		} else {
			SET_VECTOR_ELT(ans, g, set);
		}
		UNPROTECT(1); nprot--; // 'set' is protected by ans
	}

	types_context_destroy(&ctx);
	UNPROTECT(nprot);
	return ans;
}
