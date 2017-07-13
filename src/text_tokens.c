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
#include "corpus/src/array.h"
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
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


struct tokens {
	struct corpus_filter *filter;

	int *tokens;
	int ntoken;
	int ntoken_max;

	SEXP *types;
	int ntype;
	int ntype_max;
};


static void tokens_init(struct tokens *ctx, struct corpus_filter *filter);
static void tokens_clear_tokens(struct tokens *ctx);
static void tokens_add_token(struct tokens *ctx, int type_id);
static SEXP tokens_add_type(struct tokens *ctx, int type_id);
static SEXP tokens_scan(struct tokens *ctx, const struct corpus_text *text);


void tokens_init(struct tokens *ctx, struct corpus_filter *filter)
{
	ctx->filter = filter;

	ctx->ntoken_max = 0;
	ctx->ntoken = 0;
	ctx->tokens = NULL;

	ctx->ntype_max = 0;
	ctx->ntype = 0;
	ctx->types = NULL;
}


void tokens_clear_tokens(struct tokens *ctx)
{
	ctx->ntoken = 0;
}


void tokens_add_token(struct tokens *ctx, int type_id)
{
	int count = ctx->ntoken;
	int size = ctx->ntoken_max;
	int err = 0;

	if (count == size) {
		TRY(corpus_array_size_add(&size, sizeof(*ctx->tokens),
					  count, 1));
		ctx->tokens = (void *)S_realloc((void *)ctx->tokens,
					       size, count,
					       sizeof(*ctx->tokens));
		ctx->ntoken_max = size;
	}

	ctx->tokens[count] = type_id;
	ctx->ntoken = count + 1;
out:
	CHECK_ERROR(err);
}


SEXP tokens_add_type(struct tokens *ctx, int type_id)
{
	SEXP ans;
	const struct corpus_text *type;
	int count = ctx->ntype;
	int size = ctx->ntype_max;
	int err = 0;

	if (count == size) {
		TRY(corpus_array_size_add(&size, sizeof(*ctx->types),
					  count, 1));
		ctx->types = (void *)S_realloc((void *)ctx->types,
					       size, count,
					       sizeof(*ctx->types));
		ctx->ntype_max = size;
	}

	type = corpus_filter_type(ctx->filter, type_id);
	ans = mkCharLenCE((char *)type->ptr, CORPUS_TEXT_SIZE(type), CE_UTF8);
	ctx->types[count] = ans;
	ctx->ntype = count + 1;

out:
	CHECK_ERROR(err);
	return ans;
}


SEXP tokens_scan(struct tokens *ctx, const struct corpus_text *text)
{
	SEXP ans;
	int nprot, type_id, ntype;
	int err = 0, i;

	nprot = 0;

	if (!text->ptr) {
		return ScalarString(NA_STRING);
	}

	ntype = ctx->filter->ntype;

	TRY(corpus_filter_start(ctx->filter, text,
				CORPUS_FILTER_SCAN_TOKENS));
	while (corpus_filter_advance(ctx->filter)) {
		type_id = ctx->filter->type_id;
		if (type_id == CORPUS_FILTER_IGNORED) {
			continue;
		}

		// add the new types
		while (ntype < ctx->filter->ntype) {
			PROTECT(tokens_add_type(ctx, ntype)); nprot++;
			ntype++;
		}

		tokens_add_token(ctx, type_id);
	}
	TRY(ctx->filter->error);

	PROTECT(ans = allocVector(STRSXP, ctx->ntoken)); nprot++;
	for (i = 0; i < ctx->ntoken; i++) {
		type_id =  ctx->tokens[i];
		if (type_id >= 0) {
			SET_STRING_ELT(ans, i, ctx->types[type_id]);
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}
	tokens_clear_tokens(ctx);

	// no need to protect the new types any more; ans protects them
out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}


SEXP text_tokens(SEXP sx, SEXP sprops)
{
	SEXP ans, names, stext, sfilter;
	const struct corpus_text *text;
	struct corpus_filter *filter;
	struct tokens ctx;
	R_xlen_t i, n;
	int nprot, type_id, ntype;

	nprot = 0;

	PROTECT(stext = coerce_text(sx)); nprot++;
	PROTECT(sfilter = alloc_filter(sprops)); nprot++;

	text = as_text(stext, &n);
	filter = as_filter(sfilter);

	PROTECT(ans = allocVector(VECSXP, n)); nprot++;
	names = names_text(stext);
	setAttrib(ans, R_NamesSymbol, names);

	tokens_init(&ctx, filter);

	// add the existing types in the filter
	ntype = ctx.filter->ntype;
	for (type_id = 0; type_id < ntype; type_id++) {
		PROTECT(tokens_add_type(&ctx, type_id)); nprot++;
	}

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		SET_VECTOR_ELT(ans, i, tokens_scan(&ctx, &text[i]));
	}

	UNPROTECT(nprot);
	return ans;
}
