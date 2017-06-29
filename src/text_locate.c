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
#include "corpus/src/tree.h"
#include "corpus/src/text.h"
#include "corpus/src/termset.h"
#include "corpus/src/search.h"
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
	struct locate_item *items;
	int nitem;
	int nitem_max;
};


static void locate_init(struct locate *loc);
static void locate_add(struct locate *loc, int text_id, int term_id,
		       const struct corpus_text *instance);
static void locate_grow(struct locate *loc, int nadd);
SEXP make_instances(struct locate *loc, SEXP sx, SEXP terms,
		    const struct corpus_text *text);


void locate_init(struct locate *loc)
{
	loc->items = NULL;
	loc->nitem = 0;
	loc->nitem_max = 0;
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
	char *base = (char *)loc->items;
	size_t width = sizeof(*loc->items);
        int size = loc->nitem_max;
        int err;

	if (nadd <= size - loc->nitem) {
		return;
	}

        if ((err = corpus_array_size_add(&size, width, loc->nitem, nadd))) {
                Rf_error("overflow error");
        }

        loc->items = (void *)S_realloc(base, size, loc->nitem_max, width);
        loc->nitem_max = size;
}


SEXP text_count(SEXP sx, SEXP sterms, SEXP sfilter)
{
	SEXP ans, ssearch;
	const struct corpus_text *text;
	struct corpus_filter *filter;
	struct corpus_search *search;
	R_xlen_t i, n;
	int count;
	int err, nprot;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

	PROTECT(ssearch = alloc_search(sterms, "count", filter)); nprot++;
	search = as_search(ssearch);

	PROTECT(ans = allocVector(REALSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	for (i = 0; i < n; i++) {
		if (text[i].ptr == NULL) {
			REAL(ans)[i] = NA_REAL;
			continue;
		}

		TRY(corpus_search_start(search, &text[i], filter));

		count = 0;
		while (corpus_search_advance(search)) {
			count++;
		}
		REAL(ans)[i] = (double)count;

		TRY(search->error);

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	err = 0;

out:
	UNPROTECT(nprot);
	if (err) {
		Rf_error("memory allocation failure");
	}
	return ans;
}


SEXP text_detect(SEXP sx, SEXP sterms, SEXP sfilter)
{
	SEXP ans, ssearch;
	const struct corpus_text *text;
	struct corpus_filter *filter;
	struct corpus_search *search;
	R_xlen_t i, n;
	int err, nprot;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

	PROTECT(ssearch = alloc_search(sterms, "detect", filter)); nprot++;
	search = as_search(ssearch);

	PROTECT(ans = allocVector(LGLSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	for (i = 0; i < n; i++) {
		if (text[i].ptr == NULL) {
			LOGICAL(ans)[i] = NA_LOGICAL;
			continue;
		}

		TRY(corpus_search_start(search, &text[i], filter));

		if (corpus_search_advance(search)) {
			LOGICAL(ans)[i] = TRUE;
		} else {
			LOGICAL(ans)[i] = FALSE;
		}

		TRY(search->error);

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	err = 0;

out:
	UNPROTECT(nprot);
	if (err) {
		Rf_error("memory allocation failure");
	}
	return ans;
}


SEXP text_locate(SEXP sx, SEXP sterms, SEXP sfilter)
{
	SEXP ans, sitems, ssearch;
	const struct corpus_text *text, *token;
	struct corpus_filter *filter;
	struct corpus_search *search;
	struct locate loc;
	R_xlen_t i, n;
	int err, nprot, term_id;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	
	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

	PROTECT(ssearch = alloc_search(sterms, "locate", filter)); nprot++;
	sitems = items_search(ssearch);
	search = as_search(ssearch);

	locate_init(&loc);

	for (i = 0; i < n; i++) {
		if (text[i].ptr == NULL) {
			continue;
		}

		TRY(corpus_search_start(search, &text[i], filter));

		while (corpus_search_advance(search)) {
			term_id = search->term_id;
			token = &search->current;
			locate_add(&loc, i, term_id, token);
		}

		TRY(search->error);

		if ((i + 1) % RCORPUS_CHECK_INTERRUPT == 0) {
			R_CheckUserInterrupt();
		}
	}

	PROTECT(ans = make_instances(&loc, sx, sitems, text)); nprot++;
	err = 0;

out:
	UNPROTECT(nprot);
	if (err) {
		Rf_error("memory allocation failure");
	}
	return ans;
}


SEXP make_instances(struct locate *loc, SEXP sx, SEXP terms,
		    const struct corpus_text *text)
{
	SEXP ans, names, row_names, sclass, sources,
	     ptable, psource, prow, pstart, pstop,
	     before, bsource, brow, bstart, bstop,
	     after, asource, arow, astart, astop,
	     stext, sterm, instance;
	struct mkchar mkchar;
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

	mkchar_init(&mkchar);

	for (i = 0; i < n; i++) {
		text_id = loc->items[i].text_id;
		REAL(stext)[i] = (double)(text_id + 1);

		term_id = loc->items[i].term_id;
		SET_STRING_ELT(sterm, i, STRING_ELT(terms, term_id));

		SET_STRING_ELT(instance, i,
			       mkchar_get(&mkchar, &loc->items[i].instance));

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

	PROTECT(sclass = allocVector(STRSXP, 3)); nprot++;
        SET_STRING_ELT(sclass, 0, mkChar("corpus_text_locate"));
        SET_STRING_ELT(sclass, 1, mkChar("corpus_frame"));
        SET_STRING_ELT(sclass, 2, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);
	
	UNPROTECT(nprot);
	return ans;
}
