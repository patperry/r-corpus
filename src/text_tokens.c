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

	int *buf;
	int nbuf, nbuf_max;

	SEXP *types;
	int ntype, ntype_max;
};


static void tokens_init(struct tokens *ctx, struct corpus_filter *filter);
static SEXP tokens_add_type(struct tokens *ctx, int type_id);
static SEXP tokens_scan(struct tokens *ctx, const struct corpus_text *text);


void tokens_init(struct tokens *ctx, struct corpus_filter *filter)
{
	ctx->filter = filter;

	ctx->nbuf_max = 256;
	ctx->buf = (void *)R_alloc(ctx->nbuf_max, sizeof(*ctx->buf));

	ctx->ntype_max = 0;
	ctx->ntype = 0;
	ctx->types = NULL;
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
	int err = 0, i, ntok;

	if (!text->ptr) {
		return ScalarString(NA_STRING);
	}

	ntype = ctx->filter->ntype;
	nprot = 0;
	ntok = 0;

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

		if (ntok == ctx->nbuf_max) {
			ctx->nbuf_max = 2 * ctx->nbuf_max;
			ctx->buf = (void *)S_realloc((void *)ctx->buf,
						     ctx->nbuf_max,
						     ntok, sizeof(*ctx->buf));
		}
		ctx->buf[ntok] = type_id;
		ntok++;
	}
	TRY(ctx->filter->error);

	PROTECT(ans = allocVector(STRSXP, ntok)); nprot++;
	for (i = 0; i < ntok; i++) {
		type_id =  ctx->buf[i];
		if (type_id >= 0) {
			SET_STRING_ELT(ans, i, ctx->types[type_id]);
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}

	// no need to protect the new terms any more; ans protects them
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
	int nprot, ntype;

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
	for (i = 0; i < ntype; i++) {
		PROTECT(tokens_add_type(&ctx, i)); nprot++;
	}

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		SET_VECTOR_ELT(ans, i, tokens_scan(&ctx, &text[i]));
	}

	UNPROTECT(nprot);
	return ans;
}
