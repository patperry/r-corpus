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
#include "corpus/src/array.h"
#include "corpus/src/memory.h"
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/intset.h"
#include "rcorpus.h"

#ifdef error
#  undef error
#endif

#define TRY(x) \
	do { \
		if ((err = (x))) { \
			goto out; \
		} \
	} while (0)

struct locate_item {
	int text_id;
	int term_id;
	struct corpus_text instance;
};

struct locate {
	struct corpus_filter *filter;
	struct corpus_intset set;
	SEXP *terms;
	struct corpus_typemap map;
	struct locate_item *items;
	struct mkchar mkchar;
	int nitem;
	int nitem_max;
	int has_map;
	int has_set;
	int has_mkchar;
	int nprot;
};


static void locate_init(struct locate *loc, SEXP terms, SEXP filter);
static void locate_destroy(struct locate *loc);
static void locate_add(struct locate *loc, int text_id, int term_id,
		       const struct corpus_text *instance);
static void locate_grow(struct locate *loc, int nadd);
static SEXP make_instances(struct locate *loc, SEXP sx,
			   const struct corpus_text *text);


void locate_init(struct locate *loc, SEXP sterms, SEXP sfilter)
{
	const struct corpus_text *terms;
	R_xlen_t i, n;
	int err, symbol_id, term_id, type_id, type_kind;

	loc->has_map = 0;
	loc->has_set = 0;
	loc->has_mkchar = 0;
	loc->items = NULL;
	loc->nitem = 0;
	loc->nitem_max = 0;
	loc->nprot = 0;

	type_kind = token_filter_type_kind(sfilter);
	PROTECT(sfilter = alloc_filter(sfilter)); loc->nprot++;
	loc->filter = as_filter(sfilter);

	PROTECT(sterms = coerce_text(sterms)); loc->nprot++;
	terms = as_text(sterms, &n);
	loc->terms = (void *)R_alloc(n,  sizeof(*loc->terms));

	mkchar_init(&loc->mkchar);
	loc->has_mkchar = 1;
	
	TRY(corpus_typemap_init(&loc->map, type_kind, NULL));
	loc->has_map = 1;

	TRY(corpus_intset_init(&loc->set));
	loc->has_set = 1;

	for (i = 0; i < n; i++) {
		if (!terms[i].ptr) {
			continue;
		}

		TRY(corpus_typemap_set(&loc->map, &terms[i]));
		TRY(corpus_filter_combine(loc->filter, &loc->map.type));
		corpus_symtab_has_type(&loc->filter->symtab, &loc->map.type,
				       &symbol_id);
		type_id = loc->filter->type_ids[symbol_id];
		TRY(corpus_intset_add(&loc->set, type_id, &term_id));
		PROTECT(loc->terms[term_id]
				= mkchar_get(&loc->mkchar, &terms[i]));
		loc->nprot++;
	}
	err = 0;
out:
	if (err) {
		locate_destroy(loc);
		Rf_error("memory allocation failure");
	}
}


void locate_destroy(struct locate *loc)
{
	if (loc->has_set) {
		corpus_intset_destroy(&loc->set);
		loc->has_set = 0;
	}

	if (loc->has_map) {
		corpus_typemap_destroy(&loc->map);
		loc->has_map = 0;
	}

	if (loc->has_mkchar) {
		mkchar_destroy(&loc->mkchar);
		loc->has_mkchar = 0;
	}

	corpus_free(loc->items);
	loc->items = NULL;
	loc->nitem = 0;
	loc->nitem_max = 0;

	UNPROTECT(loc->nprot);
	loc->nprot = 0;
}


void locate_add(struct locate *loc, int text_id, int term_id,
		const struct corpus_text *instance)
{
	int id;

	if (loc->nitem == loc->nitem_max) {
		locate_grow(loc, 1);
	}

	id = loc->nitem;
	loc->items[id].text_id = text_id;
	loc->items[id].term_id = term_id;
	loc->items[id].instance = *instance;
	loc->nitem++;
}


void locate_grow(struct locate *loc, int nadd)
{
	void *base = loc->items;
        int size = loc->nitem_max;
        int err;

        if ((err = corpus_array_grow(&base, &size, sizeof(*loc->items),
                                     loc->nitem, nadd))) {
		locate_destroy(loc);
                Rf_error("memory allocation failure");
        }

        loc->items = base;
        loc->nitem_max = size;
}


SEXP text_locate(SEXP sx, SEXP sterms, SEXP sfilter)
{
	SEXP ans;
	const struct corpus_text *text;
	struct locate loc;
	R_xlen_t i, n;
	int err, nprot, type_id, term_id;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	locate_init(&loc, sterms, sfilter);
	
	for (i = 0; i < n; i++) {
		TRY(corpus_filter_start(loc.filter, &text[i],
					CORPUS_FILTER_SCAN_TOKENS));
		while (corpus_filter_advance(loc.filter)) {
			type_id = loc.filter->type_id;
			if (type_id < 0) {
				continue;
			}
			if (!corpus_intset_has(&loc.set, type_id, &term_id)) {
				continue;
			}
			locate_add(&loc, i, term_id, &loc.filter->current);
		}

		TRY(loc.filter->error);
	}

	PROTECT(ans = make_instances(&loc, sx, text)); nprot++;
	err = 0;

out:
	locate_destroy(&loc);
	UNPROTECT(nprot);
	if (err) {
		Rf_error("memory allocation failure");
	}
	return ans;
}


SEXP make_instances(struct locate *loc, SEXP sx,
		    const struct corpus_text *text)
{
	SEXP ans, names, row_names, sclass, sources,
	     ptable, psource, prow, pstart, pstop,
	     before, bsource, brow, bstart, bstop,
	     after, asource, arow, astart, astop,
	     stext, sterm, instance;
	R_xlen_t i, n, term_id, text_id;
	int nprot, off, len, start, stop;
	
	n = loc->nitem;
	nprot = 0;
	
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");
	pstop = getListElement(ptable, "stop");

	PROTECT(stext = allocVector(REALSXP, n)); nprot++;
	PROTECT(sterm = allocVector(STRSXP, n)); nprot++;
	PROTECT(instance = allocVector(STRSXP, n)); nprot++;

	PROTECT(bsource = allocVector(INTSXP, n)); nprot++;
	PROTECT(brow = allocVector(REALSXP, n)); nprot++;
	PROTECT(bstart = allocVector(INTSXP, n)); nprot++;
	PROTECT(bstop = allocVector(INTSXP, n)); nprot++;

	PROTECT(asource = allocVector(INTSXP, n)); nprot++;
	PROTECT(arow = allocVector(REALSXP, n)); nprot++;
	PROTECT(astart = allocVector(INTSXP, n)); nprot++;
	PROTECT(astop = allocVector(INTSXP, n)); nprot++;

	for (i = 0; i < n; i++) {
		text_id = loc->items[i].text_id;
		REAL(stext)[i] = (double)(text_id + 1);

		term_id = loc->items[i].term_id;
		SET_STRING_ELT(sterm, i, loc->terms[term_id]);

		SET_STRING_ELT(instance, i,
			       mkchar_get(&loc->mkchar,
				          &loc->items[i].instance));

		start = INTEGER(pstart)[text_id];
		stop = INTEGER(pstop)[text_id];
		off = (int)(loc->items[i].instance.ptr - text[text_id].ptr);
		len = (int)CORPUS_TEXT_SIZE(&loc->items[i].instance);

		INTEGER(bsource)[i] = INTEGER(psource)[text_id];
		REAL(brow)[i] = REAL(prow)[text_id];
		INTEGER(bstart)[i] = start;
		INTEGER(bstop)[i] = start + off - 1;

		INTEGER(asource)[i] = INTEGER(psource)[text_id];
		REAL(arow)[i] = REAL(prow)[text_id];
		INTEGER(astart)[i] = start + off + len;
		INTEGER(astop)[i] = stop;
	}

	PROTECT(before = alloc_text(sources, bsource, brow, bstart, bstop));
	nprot++;

	PROTECT(after = alloc_text(sources, asource, arow, astart, astop));
	nprot++;

	PROTECT(ans = allocVector(VECSXP, 5)); nprot++;
	SET_VECTOR_ELT(ans, 0, stext);
	SET_VECTOR_ELT(ans, 1, sterm);
	SET_VECTOR_ELT(ans, 2, before);
	SET_VECTOR_ELT(ans, 3, instance);
	SET_VECTOR_ELT(ans, 4, after);

	PROTECT(names = allocVector(STRSXP, 5)); nprot++;
	SET_STRING_ELT(names, 0, mkChar("text"));
	SET_STRING_ELT(names, 1, mkChar("term"));
	SET_STRING_ELT(names, 2, mkChar("before"));
	SET_STRING_ELT(names, 3, mkChar("instance"));
	SET_STRING_ELT(names, 4, mkChar("after"));
	setAttrib(ans, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2)); nprot++;
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)n;
	setAttrib(ans, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 1)); nprot++;
        SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);
	
	UNPROTECT(nprot);
	return ans;
}
