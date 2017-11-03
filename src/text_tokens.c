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
#include "rcorpus.h"


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
static SEXP tokens_scan(struct tokens *ctx, const struct utf8lite_text *text);


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
		if (count > 0) {
			ctx->tokens = (void *)S_realloc((void *)ctx->tokens,
							size, count,
							sizeof(*ctx->tokens));
		} else {
			ctx->tokens = (void *)R_alloc(size,
						      sizeof(*ctx->tokens));
		}
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
	const struct utf8lite_text *type;
	int count = ctx->ntype;
	int size = ctx->ntype_max;
	int err = 0;

	if (count == size) {
		TRY(corpus_array_size_add(&size, sizeof(*ctx->types),
					  count, 1));
		if (count > 0) {
			ctx->types = (void *)S_realloc((void *)ctx->types,
						       size, count,
						       sizeof(*ctx->types));
		} else {
			ctx->types = (void *)R_alloc(size,
						     sizeof(*ctx->types));
		}
		ctx->ntype_max = size;
	}

	type = &ctx->filter->symtab.types[type_id].text;
	ans = mkCharLenCE((char *)type->ptr, UTF8LITE_TEXT_SIZE(type), CE_UTF8);
	ctx->types[count] = ans;
	ctx->ntype = count + 1;

out:
	CHECK_ERROR(err);
	return ans;
}


SEXP tokens_scan(struct tokens *ctx, const struct utf8lite_text *text)
{
	SEXP ans;
	int nprot, type_id, ntype;
	int err = 0, i;

	nprot = 0;

	if (!text->ptr) {
		return ScalarString(NA_STRING);
	}

	ntype = ctx->filter->symtab.ntype;

	TRY(corpus_filter_start(ctx->filter, text));
	while (corpus_filter_advance(ctx->filter)) {
		// add the new types
		while (ntype < ctx->filter->symtab.ntype) {
			PROTECT(tokens_add_type(ctx, ntype)); nprot++;
			ntype++;
		}

		type_id = ctx->filter->type_id;
		if (type_id >= 0) {
			tokens_add_token(ctx, type_id);
		}
	}
	TRY(ctx->filter->error);

	PROTECT(ans = allocVector(STRSXP, ctx->ntoken)); nprot++;
	for (i = 0; i < ctx->ntoken; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		type_id =  ctx->tokens[i];
		SET_STRING_ELT(ans, i, ctx->types[type_id]);
	}
	tokens_clear_tokens(ctx);

	// no need to protect the new types any more; ans protects them
out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}


SEXP text_tokens(SEXP sx)
{
	SEXP ans, names;
	const struct utf8lite_text *text;
	struct corpus_filter *filter;
	struct tokens ctx;
	R_xlen_t i, n;
	int nprot, type_id, ntype;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	PROTECT(ans = allocVector(VECSXP, n)); nprot++;
	names = names_text(sx);
	setAttrib(ans, R_NamesSymbol, names);

	tokens_init(&ctx, filter);

	// add the existing types in the filter
	ntype = ctx.filter->symtab.ntype;
	for (type_id = 0; type_id < ntype; type_id++) {
		RCORPUS_CHECK_INTERRUPT(type_id);
		PROTECT(tokens_add_type(&ctx, type_id)); nprot++;
	}

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		SET_VECTOR_ELT(ans, i, tokens_scan(&ctx, &text[i]));
	}

	UNPROTECT(nprot);
	return ans;
}
