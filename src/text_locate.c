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
#include "rcorpus.h"


struct locate_item {
	int text_id;
	int term_id;
	struct utf8lite_text instance;
};


struct locate {
	struct locate_item *items;
	int nitem;
	int nitem_max;
};


static void locate_init(struct locate *loc);
static void locate_add(struct locate *loc, int text_id, int term_id,
		       const struct utf8lite_text *instance);
static void locate_grow(struct locate *loc, int nadd);
SEXP make_matches(struct locate *loc, SEXP terms);
SEXP make_instances(struct locate *loc, SEXP sx,
		    const struct utf8lite_text *text);


void locate_init(struct locate *loc)
{
	loc->items = NULL;
	loc->nitem = 0;
	loc->nitem_max = 0;
}


void locate_add(struct locate *loc, int text_id, int term_id,
		const struct utf8lite_text *instance)
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

        TRY(corpus_array_size_add(&size, width, loc->nitem, nadd));

	if (loc->nitem_max > 0) {
		loc->items = (void *)S_realloc(base, size, loc->nitem_max,
					       width);
	} else {
		loc->items = (void *)R_alloc(size, width);
	}
        loc->nitem_max = size;
	err = 0;
out:
	CHECK_ERROR(err);
}


SEXP text_count(SEXP sx, SEXP sterms)
{
	SEXP ans, ssearch;
	const struct utf8lite_text *text;
	struct corpus_filter *filter;
	struct corpus_search *search;
	R_xlen_t i, n;
	int count;
	int err, nprot;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	PROTECT(ssearch = alloc_search(sterms, "count", filter)); nprot++;
	search = as_search(ssearch);

	PROTECT(ans = allocVector(REALSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

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
	}

	err = 0;
out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_detect(SEXP sx, SEXP sterms)
{
	SEXP ans, ssearch;
	const struct utf8lite_text *text;
	struct corpus_filter *filter;
	struct corpus_search *search;
	R_xlen_t i, n;
	int err, nprot;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	PROTECT(ssearch = alloc_search(sterms, "detect", filter)); nprot++;
	search = as_search(ssearch);

	PROTECT(ans = allocVector(LGLSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names_text(sx));

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

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
	}

	err = 0;
out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_match(SEXP sx, SEXP sterms)
{
	SEXP ans, sitems, ssearch;
	const struct utf8lite_text *text, *token;
	struct corpus_filter *filter;
	struct corpus_search *search;
	struct locate loc;
	R_xlen_t i, n;
	int err, nprot, term_id;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	PROTECT(ssearch = alloc_search(sterms, "locate", filter)); nprot++;
	sitems = items_search(ssearch);
	search = as_search(ssearch);

	locate_init(&loc);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

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
	}

	PROTECT(ans = make_matches(&loc, sitems)); nprot++;
	err = 0;
out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_locate(SEXP sx, SEXP sterms)
{
	SEXP ans, ssearch;
	const struct utf8lite_text *text, *token;
	struct corpus_filter *filter;
	struct corpus_search *search;
	struct locate loc;
	R_xlen_t i, n;
	int err, nprot, term_id;

	nprot = 0;

	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	filter = text_filter(sx);

	PROTECT(ssearch = alloc_search(sterms, "locate", filter)); nprot++;
	search = as_search(ssearch);

	locate_init(&loc);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

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
	}

	PROTECT(ans = make_instances(&loc, sx, text)); nprot++;
	err = 0;
out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}


SEXP make_matches(struct locate *loc, SEXP levels)
{
	SEXP ans, names, row_names, sclass, stext, sterm;
	R_xlen_t i, n, term_id, text_id;
	int nprot;

	n = loc->nitem;
	nprot = 0;

	PROTECT(stext = allocVector(REALSXP, n)); nprot++;
	PROTECT(sterm = allocVector(INTSXP, n)); nprot++;

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		text_id = loc->items[i].text_id;
		REAL(stext)[i] = (double)(text_id + 1);

		term_id = loc->items[i].term_id;
		INTEGER(sterm)[i] = term_id + 1;
	}
	setAttrib(sterm, R_LevelsSymbol, levels);
	setAttrib(sterm, R_ClassSymbol, mkString("factor"));

	PROTECT(ans = allocVector(VECSXP, 2)); nprot++;
	SET_VECTOR_ELT(ans, 0, stext);
	SET_VECTOR_ELT(ans, 1, sterm);

	PROTECT(names = allocVector(STRSXP, 2)); nprot++;
	SET_STRING_ELT(names, 0, mkChar("text"));
	SET_STRING_ELT(names, 1, mkChar("term"));
	setAttrib(ans, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2)); nprot++;
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)n;
	setAttrib(ans, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 2)); nprot++;
        SET_STRING_ELT(sclass, 0, mkChar("corpus_frame"));
        SET_STRING_ELT(sclass, 1, mkChar("data.frame"));
        setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(nprot);
	return ans;
}


SEXP make_instances(struct locate *loc, SEXP sx,
		    const struct utf8lite_text *text)
{
	SEXP ans, names, filter, row_names, sclass, sources,
	     ptable, psource, prow, pstart, pstop,
	     before, bsource, brow, bstart, bstop,
	     instance, isource, irow, istart, istop,
	     after, asource, arow, astart, astop,
	     stext;
	struct mkchar mkchar;
	R_xlen_t i, n, text_id;
	double row;
	int nprot, off, len, source, start, stop;
	
	n = loc->nitem;
	nprot = 0;
	
	filter = filter_text(sx);
	sources = getListElement(sx, "sources");
	ptable = getListElement(sx, "table");
	psource = getListElement(ptable, "source");
	prow = getListElement(ptable, "row");
	pstart = getListElement(ptable, "start");
	pstop = getListElement(ptable, "stop");

	PROTECT(stext = allocVector(REALSXP, n)); nprot++;

	PROTECT(bsource = allocVector(INTSXP, n)); nprot++;
	PROTECT(brow = allocVector(REALSXP, n)); nprot++;
	PROTECT(bstart = allocVector(INTSXP, n)); nprot++;
	PROTECT(bstop = allocVector(INTSXP, n)); nprot++;

	PROTECT(isource = allocVector(INTSXP, n)); nprot++;
	PROTECT(irow = allocVector(REALSXP, n)); nprot++;
	PROTECT(istart = allocVector(INTSXP, n)); nprot++;
	PROTECT(istop = allocVector(INTSXP, n)); nprot++;

	PROTECT(asource = allocVector(INTSXP, n)); nprot++;
	PROTECT(arow = allocVector(REALSXP, n)); nprot++;
	PROTECT(astart = allocVector(INTSXP, n)); nprot++;
	PROTECT(astop = allocVector(INTSXP, n)); nprot++;

	mkchar_init(&mkchar);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		text_id = loc->items[i].text_id;
		REAL(stext)[i] = (double)(text_id + 1);

		source = INTEGER(psource)[text_id];
		row = REAL(prow)[text_id];
		start = INTEGER(pstart)[text_id];
		stop = INTEGER(pstop)[text_id];

		off = (int)(loc->items[i].instance.ptr - text[text_id].ptr);
		len = (int)UTF8LITE_TEXT_SIZE(&loc->items[i].instance);

		INTEGER(bsource)[i] = source;
		REAL(brow)[i] = row;
		INTEGER(bstart)[i] = start;
		INTEGER(bstop)[i] = start + off - 1;

		INTEGER(isource)[i] = source;
		REAL(irow)[i] = row;
		INTEGER(istart)[i] = start + off;
		INTEGER(istop)[i] = start + off + len - 1;

		INTEGER(asource)[i] = source;
		REAL(arow)[i] = row;
		INTEGER(astart)[i] = start + off + len;
		INTEGER(astop)[i] = stop;
	}

	PROTECT(before = alloc_text(sources, bsource, brow, bstart, bstop,
				    R_NilValue, filter));
	nprot++;

	PROTECT(instance = alloc_text(sources, isource, irow, istart, istop,
				      R_NilValue, filter));
	nprot++;

	PROTECT(after = alloc_text(sources, asource, arow, astart, astop,
				   R_NilValue, filter));
	nprot++;

	PROTECT(ans = allocVector(VECSXP, 4)); nprot++;
	SET_VECTOR_ELT(ans, 0, stext);
	SET_VECTOR_ELT(ans, 1, before);
	SET_VECTOR_ELT(ans, 2, instance);
	SET_VECTOR_ELT(ans, 3, after);

	PROTECT(names = allocVector(STRSXP, 4)); nprot++;
	SET_STRING_ELT(names, 0, mkChar("text"));
	SET_STRING_ELT(names, 1, mkChar("before"));
	SET_STRING_ELT(names, 2, mkChar("instance"));
	SET_STRING_ELT(names, 3, mkChar("after"));
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
