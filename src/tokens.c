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
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "rcorpus.h"


struct tokens {
	struct corpus_symtab symtab;
	struct corpus_wordscan scan;
	int ignore_empty;
	struct text_filter_drop drop;

	int *buf;
	int nbuf, nbuf_max;

	SEXP *types;
	int ntype, ntype_max;
};


static void tokens_init(struct tokens *ctx, SEXP sfilter)
{
	struct corpus_text empty;
	const char *stemmer;
	int kind, type_id;

	kind = text_filter_type_kind(sfilter);
	stemmer = text_filter_stemmer(sfilter);

	if (corpus_symtab_init(&ctx->symtab, kind, stemmer) != 0) {
		error("failed initializing tokens symbol table");
	}

	ctx->ignore_empty = text_filter_ignore_empty(sfilter);
	text_filter_get_drop(sfilter, &ctx->drop);

	ctx->nbuf_max = 256;
	ctx->buf = (void *)R_alloc(ctx->nbuf_max, sizeof(*ctx->buf));

	ctx->ntype_max = 256;
	ctx->ntype = 0;
	ctx->types = (void *)R_alloc(ctx->ntype_max, sizeof(*ctx->types));

	// add the empty type, and protect it
	empty.ptr = NULL;
	empty.attr = 0;
	if (corpus_symtab_add_type(&ctx->symtab, &empty, &type_id) != 0) {
		corpus_symtab_destroy(&ctx->symtab);
		error("memory allocation failure");
	}
	PROTECT(ctx->types[0] = mkCharLenCE(NULL, 0, CE_UTF8));
	ctx->ntype++;
}


static void tokens_destroy(struct tokens *ctx)
{
	corpus_symtab_destroy(&ctx->symtab);
	UNPROTECT(1);
}


static int tokens_add(struct tokens *ctx, const struct corpus_text *token,
		      int *naddptr)
{
	const struct corpus_text *type;
	int token_id, type_id;

	if (corpus_symtab_add_token(&ctx->symtab, token, &token_id) != 0) {
		tokens_destroy(ctx);
		error("memory allocation failure");
	}

	type_id = ctx->symtab.tokens[token_id].type_id;

	if (type_id == ctx->ntype) {
		if (ctx->ntype == ctx->ntype_max) {
			ctx->ntype_max = 2 * ctx->ntype_max;
			ctx->types = (void *)S_realloc((void *)ctx->types,
							ctx->ntype_max,
							ctx->ntype,
							sizeof(*ctx->types));
		}

		type = &ctx->symtab.types[type_id].text;
		ctx->types[ctx->ntype] = mkCharLenCE((char *)type->ptr,
						     CORPUS_TEXT_SIZE(type),
						     CE_UTF8);
		PROTECT(ctx->types[ctx->ntype]);
		ctx->ntype++;
		*naddptr = *naddptr + 1;
	}

	return type_id;
}


static int tokens_drop(struct tokens *ctx, enum corpus_word_type type)
{
	switch (type) {
	case CORPUS_WORD_NONE:
		return ctx->drop.symbol;

	case CORPUS_WORD_NUMBER:
		return ctx->drop.number;

	case CORPUS_WORD_LETTER:
		return ctx->drop.letter;

	case CORPUS_WORD_KANA:
		return ctx->drop.kana;

	case CORPUS_WORD_IDEO:
		return ctx->drop.ideo;

	default:
		tokens_destroy(ctx);
		error("internal error: unknown word type");
		return 0;
	}
}


static SEXP tokens_scan(struct tokens *ctx,
			const struct corpus_text *text)
{
	SEXP ans;
	const struct corpus_text *type;
	int nadd, type_id;
	int i, ntok;

	if (!text->ptr) {
		return ScalarString(NA_STRING);
	}

	ntok = 0;
	nadd = 0;

	corpus_wordscan_make(&ctx->scan, text);

	while (corpus_wordscan_advance(&ctx->scan)) {
		type_id = tokens_add(ctx, &ctx->scan.current, &nadd);
		type = &ctx->symtab.types[type_id].text;

		if (CORPUS_TEXT_SIZE(type) == 0 && ctx->ignore_empty) {
			continue;
		}

		if (tokens_drop(ctx, ctx->scan.type)) {
			type_id = -1;
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

	PROTECT(ans = allocVector(STRSXP, ntok));
	for (i = 0; i < ntok; i++) {
		type_id =  ctx->buf[i];
		if (type_id >= 0) {
			SET_STRING_ELT(ans, i, ctx->types[type_id]);
		} else {
			SET_STRING_ELT(ans, i, NA_STRING);
		}
	}

	// no need to protect the new words any more, since they
	// are protected by ans
	UNPROTECT(nadd + 1);

	return ans;
}


SEXP tokens_text(SEXP sx, SEXP sfilter)
{
	SEXP ans, names, stext;
	const struct corpus_text *text;
	struct tokens ctx;
	R_xlen_t i, n;

	PROTECT(stext = coerce_text(sx));
	text = as_text(stext, &n);

	PROTECT(ans = allocVector(VECSXP, n));
	names = names_text(stext);
	setAttrib(ans, R_NamesSymbol, names);

	tokens_init(&ctx, sfilter);

	for (i = 0; i < n; i++) {
		SET_VECTOR_ELT(ans, i, tokens_scan(&ctx, &text[i]));
	}

	tokens_destroy(&ctx);
	UNPROTECT(2);
	return ans;
}
