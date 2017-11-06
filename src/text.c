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

#include <float.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include "rcorpus.h"

#define TEXT_TAG install("corpus::text")

enum source_type {
	SOURCE_NONE = 0,
	SOURCE_CHAR,
	SOURCE_JSON
};


struct source {
	int type;
	union {
		const struct json *set;
		SEXP chars;
	} data;
	R_xlen_t nrow;
};


static int is_source(SEXP x)
{
	return (x == R_NilValue || TYPEOF(x) == STRSXP || is_json(x));
}


static void source_assign(struct source *source, SEXP value)
{
	if (value == R_NilValue) {
		source->type = SOURCE_NONE;
		source->nrow = 0;
	} else if (TYPEOF(value) == STRSXP) {
		source->type = SOURCE_CHAR;
		source->data.chars = value;
		source->nrow = XLENGTH(value);
	} else if (is_json(value)) {
		source->type = SOURCE_JSON;
		source->data.set = as_json(value);
		source->nrow = source->data.set->nrow;
	} else {
		error("invalid text source;"
		      " should be 'character', 'json', or NULL");
	}
}


static void free_text(SEXP stext)
{
        struct rcorpus_text *obj = R_ExternalPtrAddr(stext);
	R_SetExternalPtrAddr(stext, NULL);

	if (obj) {
		if (obj->has_sentfilter) {
			corpus_sentfilter_destroy(&obj->sentfilter);
		}

		if (obj->has_filter) {
			corpus_filter_destroy(&obj->filter);
		}

		if (obj->has_stemmer) {
			stemmer_destroy(&obj->stemmer);
		}

		corpus_free(obj->text);
		corpus_free(obj);
	}
}


static void load_text(SEXP x);


SEXP alloc_text_handle(void)
{
	SEXP ans;

	PROTECT(ans = R_MakeExternalPtr(NULL, TEXT_TAG, R_NilValue));
	R_RegisterCFinalizerEx(ans, free_text, TRUE);
	UNPROTECT(1);
	return ans;
}


SEXP alloc_text(SEXP sources, SEXP source, SEXP row, SEXP start, SEXP stop,
		SEXP eltnames, SEXP filter)
{
	SEXP ans, handle, names, sclass, src, row_names, table;
	R_xlen_t n;
	int s, nsrc;

	PROTECT(handle = alloc_text_handle());

	n = XLENGTH(source);

	if (TYPEOF(sources) != VECSXP) {
		error("invalid 'sources' argument");
	} else if (XLENGTH(sources) > INT_MAX) {
		error("'sources' length exceeds maximum (%d)", INT_MAX);
	} else if (TYPEOF(source) != INTSXP) {
		error("invalid 'source' argument");
	} else if (XLENGTH(row) != n || TYPEOF(row) != REALSXP) {
		error("invalid 'row' argument");
	} else if (XLENGTH(start) != n || TYPEOF(start) != INTSXP) {
		error("invalid 'start' argument");
	} else if (XLENGTH(stop) != n || TYPEOF(stop) != INTSXP) {
		error("invalid 'stop' argument");
	}
	if (eltnames != R_NilValue) {
		if (XLENGTH(eltnames) != n || TYPEOF(eltnames) != STRSXP) {
			error("invalid 'names' argument");
		}
	}

	nsrc = (int)XLENGTH(sources);
	for (s = 0; s < nsrc; s++) {
		src = VECTOR_ELT(sources, s);
		if (!is_source(src)) {
			error("'sources' element at index %d is invalid;"
			      "should be a 'character' or 'json'", s + 1);
		}
	}

	PROTECT(table = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(table, 0, source);
	SET_VECTOR_ELT(table, 1, row);
	SET_VECTOR_ELT(table, 2, start);
	SET_VECTOR_ELT(table, 3, stop);

	PROTECT(names = allocVector(STRSXP, 4));
        SET_STRING_ELT(names, 0, mkChar("source"));
        SET_STRING_ELT(names, 1, mkChar("row"));
        SET_STRING_ELT(names, 2, mkChar("start"));
        SET_STRING_ELT(names, 3, mkChar("stop"));
        setAttrib(table, R_NamesSymbol, names);

	PROTECT(row_names = allocVector(REALSXP, 2));
	REAL(row_names)[0] = NA_REAL;
	REAL(row_names)[1] = -(double)n;
	setAttrib(table, R_RowNamesSymbol, row_names);

	PROTECT(sclass = allocVector(STRSXP, 1));
        SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
        setAttrib(table, R_ClassSymbol, sclass);

	PROTECT(ans = allocVector(VECSXP, 5));
	SET_VECTOR_ELT(ans, 0, handle);
	SET_VECTOR_ELT(ans, 1, sources);
	SET_VECTOR_ELT(ans, 2, table);
	SET_VECTOR_ELT(ans, 3, eltnames);
	SET_VECTOR_ELT(ans, 4, filter);

	PROTECT(names = allocVector(STRSXP, 5));
        SET_STRING_ELT(names, 0, mkChar("handle"));
        SET_STRING_ELT(names, 1, mkChar("sources"));
        SET_STRING_ELT(names, 2, mkChar("table"));
        SET_STRING_ELT(names, 3, mkChar("names"));
        SET_STRING_ELT(names, 4, mkChar("filter"));
        setAttrib(ans, R_NamesSymbol, names);

	PROTECT(sclass = allocVector(STRSXP, 1));
        SET_STRING_ELT(sclass, 0, mkChar("corpus_text"));
        setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(8);
	return ans;
}


int is_text(SEXP x)
{
	SEXP handle;

	if (!isVectorList(x)) {
		return 0;
	}

	handle = getListElement(x, "handle");
	if (handle == R_NilValue) {
		return 0;
	}

	return ((TYPEOF(handle) == EXTPTRSXP)
		&& (R_ExternalPtrTag(handle) == TEXT_TAG));
}


SEXP text_valid(SEXP x)
{
	return ScalarLogical(is_text(x));
}


struct utf8lite_text *as_text(SEXP stext, R_xlen_t *lenptr)
{
	SEXP handle;
	struct rcorpus_text *obj;

	if (!is_text(stext)) {
		error("invalid 'text' object");
	}

	handle = getListElement(stext, "handle");
	obj = R_ExternalPtrAddr(handle);
	if (!obj) {
		load_text(stext);
		handle = getListElement(stext, "handle");
		obj = R_ExternalPtrAddr(handle);
	}

	if (lenptr) {
		*lenptr = obj->length;
	}

	return obj->text;
}


SEXP as_text_json(SEXP sdata, SEXP filter)
{
	SEXP ans, handle, sources, source, row, start, stop, names;
	const struct json *d = as_json(sdata);
	struct rcorpus_text *obj;
	R_xlen_t i, nrow = d->nrow;
	int err = 0, nprot = 0;

	PROTECT(sources = allocVector(VECSXP, 1)); nprot++;
	SET_VECTOR_ELT(sources, 0, sdata);

	PROTECT(source = allocVector(INTSXP, nrow)); nprot++;
	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		INTEGER(source)[i] = 1;
	}

	PROTECT(row = allocVector(REALSXP, nrow)); nprot++;
	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		REAL(row)[i] = (double)(i + 1);
	}

	PROTECT(start = allocVector(INTSXP, nrow)); nprot++;
	PROTECT(stop = allocVector(INTSXP, nrow)); nprot++;

	names = R_NilValue;

	PROTECT(ans = alloc_text(sources, source, row, start, stop, names,
				 filter));
	nprot++;

	handle = getListElement(ans, "handle");

	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));
	R_SetExternalPtrAddr(handle, obj);

	if (nrow > 0) {
		TRY_ALLOC(obj->text = corpus_calloc(nrow, sizeof(*obj->text)));
		obj->length = nrow;
	}

	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		if ((err = corpus_data_text(&d->rows[i], &obj->text[i]))) {
			obj->text[i].ptr = NULL;
			obj->text[i].attr = 0;
			INTEGER(start)[i] = NA_INTEGER;
			INTEGER(stop)[i] = NA_INTEGER;
		} else {
			if (UTF8LITE_TEXT_SIZE(&obj->text[i]) > INT_MAX) {
				error("text size (%"PRIu64 "bytes)"
				      "exceeds maximum (%d bytes)",
				      (uint64_t)
				      	UTF8LITE_TEXT_SIZE(&obj->text[i]),
				      INT_MAX);
			}
			INTEGER(start)[i] = 1;
			INTEGER(stop)[i] = (int)UTF8LITE_TEXT_SIZE(
							&obj->text[i]);
		}
	}

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}


SEXP as_text_character(SEXP x, SEXP filter)
{
	SEXP ans, handle, sources, source, row, start, stop, names, str;
	struct rcorpus_text *obj;
	const char *ptr;
	R_xlen_t i, nrow, len;
	int err = 0, nprot = 0;

	if (x == R_NilValue || TYPEOF(x) != STRSXP) {
	       error("invalid 'character' object");
	}

	nrow = XLENGTH(x);
	if ((uint64_t)nrow > (((uint64_t)1) << DBL_MANT_DIG)) {
		error("text vector length (%"PRIu64")"
		      " exceeds maximum (%"PRIu64")",
		      (uint64_t)nrow, ((uint64_t)1) << DBL_MANT_DIG);
	}

	PROTECT(sources = allocVector(VECSXP, 1)); nprot++;
	SET_VECTOR_ELT(sources, 0, x);

	PROTECT(source = allocVector(INTSXP, nrow)); nprot++;
	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		INTEGER(source)[i] = 1;
	}

	PROTECT(row = allocVector(REALSXP, nrow)); nprot++;
	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		REAL(row)[i] = (double)(i + 1);
	}

	PROTECT(start = allocVector(INTSXP, nrow)); nprot++;
	PROTECT(stop = allocVector(INTSXP, nrow)); nprot++;
	names = R_NilValue;

	PROTECT(ans = alloc_text(sources, source, row, start, stop, names,
				 filter));
	nprot++;

	handle = getListElement(ans, "handle");

	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));
	R_SetExternalPtrAddr(handle, obj);

	if (nrow > 0) {
		TRY_ALLOC(obj->text = corpus_calloc(nrow, sizeof(*obj->text)));
		obj->length = nrow;
	}

	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		str = STRING_ELT(x, i);

		// handle NA
		if (str == NA_STRING) {
			obj->text[i].ptr = NULL;
			obj->text[i].attr = 0;
			INTEGER(start)[i] = NA_INTEGER;
			INTEGER(stop)[i] = NA_INTEGER;
			continue;
		}

		// assume input is already UTF-8
		ptr = CHAR(str);

		// convert to char to text
		len = XLENGTH(str);
		if ((uint64_t)len > (uint64_t)UTF8LITE_TEXT_SIZE_MAX) {
			error("size of character object at index %"PRIu64
			      " (%"PRIu64" bytes)"
			      " exceeds maximum (%"PRIu64" bytes)",
			      (uint64_t)(i + 1), (uint64_t)len,
			      (uint64_t)UTF8LITE_TEXT_SIZE_MAX);
		}

		TRY(utf8lite_text_assign(&obj->text[i], (uint8_t *)ptr,
					 (size_t)len, 0, NULL));

		INTEGER(start)[i] = 1;
		INTEGER(stop)[i] = (int)UTF8LITE_TEXT_SIZE(&obj->text[i]);
	}

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}


SEXP coerce_text(SEXP sx)
{
	SEXP ans;

	if (is_text(sx)) {
		return sx;
	} else if (is_json(sx)) {
		return as_text_json(sx, R_NilValue);
	}

	PROTECT(sx = coerceVector(sx, STRSXP));
	ans = as_text_character(sx, R_NilValue);
	UNPROTECT(1);
	return ans;
}


static void load_text(SEXP x)
{
	SEXP shandle, srow, ssource, sstart, sstop, ssources, src, str, stable;
	const double *row;
	const int *source, *start, *stop;
	struct rcorpus_text *obj;
	struct utf8lite_text txt;
	struct utf8lite_message msg;
	struct source *sources;
	const uint8_t *ptr;
	double r;
	R_xlen_t i, j, len, nrow;
	int err = 0, s, nsrc, begin, end, flags = 0;

	shandle = getListElement(x, "handle");

	obj = R_ExternalPtrAddr(shandle);
	if (obj) {
		return;
	}

	ssources = getListElement(x, "sources");
	if (TYPEOF(ssources) != VECSXP) {
		error("invalid 'sources' argument");
	} else if (XLENGTH(ssources) > INT_MAX) {
		error("'sources' length exceeds maximum (%d)", INT_MAX);
	}

	nsrc = (int)XLENGTH(ssources);
	sources = (struct source *)R_alloc(nsrc, sizeof(*sources));

	for (s = 0; s < nsrc; s++) {
		src = VECTOR_ELT(ssources, s);
		source_assign(&sources[s], src);
	}

	stable = getListElement(x, "table");
	ssource = getListElement(stable, "source");
	srow = getListElement(stable, "row");
	sstart = getListElement(stable, "start");
	sstop = getListElement(stable, "stop");

	nrow = XLENGTH(ssource);

	if (TYPEOF(ssource) != INTSXP) {
		error("invalid 'source' argument");
	} else if (XLENGTH(srow) != nrow || TYPEOF(srow) != REALSXP) {
		error("invalid 'row' argument");
	} else if (XLENGTH(sstart) != nrow || TYPEOF(sstart) != INTSXP) {
		error("invalid 'start' argument");
	} else if (XLENGTH(sstop) != nrow || TYPEOF(sstop) != INTSXP) {
		error("invalid 'stop' argument");
	}

	source = INTEGER(ssource);
	row = REAL(srow);
	start = INTEGER(sstart);
	stop = INTEGER(sstop);

	R_RegisterCFinalizerEx(shandle, free_text, TRUE);
	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));
	R_SetExternalPtrAddr(shandle, obj);

	if (nrow > 0) {
		TRY_ALLOC(obj->text = corpus_calloc(nrow, sizeof(*obj->text)));
		obj->length = nrow;
	}

	for (i = 0; i < nrow; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		s = source[i];
		if (!(1 <= s && s <= nsrc)) {
			error("source[[%"PRIu64"]] (%d) is out of range",
				(uint64_t)i + 1, s);
		}
		s--; // switch to 0-based index

		r = row[i];
		if (!(1 <= r && r <= sources[s].nrow)) {
			error("row[[%"PRIu64"]] (%g) is out of range",
				(uint64_t)i + 1, r);
		}
		j = (R_xlen_t)(r - 1);

		// handle NA range
		if (start[i] == NA_INTEGER || stop[i] == NA_INTEGER) {
			obj->text[i].ptr = NULL;
			obj->text[i].attr = 0;
			continue;
		}

		switch (sources[s].type) {
		case SOURCE_CHAR:
			str = STRING_ELT(sources[s].data.chars, j);
			if (str == NA_STRING) {
				txt.ptr = NULL;
				txt.attr = 0;
			} else {
				ptr = (const uint8_t *)CHAR(str);
				len = XLENGTH(str);
				flags = 0;
				err = utf8lite_text_assign(&txt, ptr, len,
							   flags, &msg);
				if (err) {
					error("character object in source %d"
					      " at index %"PRIu64
					      " contains malformed UTF-8: %s",
					      s + 1, (uint64_t)(j + 1),
					      msg.string);
				}
			}
			break;

		case SOURCE_JSON:
			// no need to validate input (handled by json)
			corpus_data_text(&sources[s].data.set->rows[j], &txt);
			flags = UTF8LITE_TEXT_UNESCAPE;
			break;

		default:
			txt.ptr = NULL;
			txt.attr = 0;
			flags = 0;
			break;
		}

		begin = (start[i] < 1) ? 0 : (start[i] - 1);
		end = stop[i] < start[i] ? begin : stop[i];
		if ((size_t)end > UTF8LITE_TEXT_SIZE(&txt)) {
			end = (int)UTF8LITE_TEXT_SIZE(&txt);
		}

		// this could be made more efficient; add a
		// 'can_break?' function to corpus/text.h
		err = utf8lite_text_assign(&obj->text[i], txt.ptr + begin,
					   end - begin, flags, NULL);
		if (err) {
			error("text span in row[[%"PRIu64"]]"
			      " starts or ends in the middle"
			      " of a multi-byte character", i + 1);
		}
	}
out:
	CHECK_ERROR(err);
}


SEXP as_character_text(SEXP x)
{
	SEXP ans, str, sources, table, source, row, start, stop, src;
	struct utf8lite_text *text;
	struct mkchar mk;
	R_xlen_t i, n, r;
	int *is_char;
	int s, ns, len, alloc;

	text = as_text(x, &n);
	sources = getListElement(x, "sources");
	table = getListElement(x, "table");
	source = getListElement(table, "source");
	row = getListElement(table, "row");
	start = getListElement(table, "start");
	stop = getListElement(table, "stop");

	// compute whether each source is character
	ns = LENGTH(sources);
	is_char = (void *)R_alloc(ns, sizeof(*is_char));
	for (s = 0; s < ns; s++) {
		src = VECTOR_ELT(sources, s);
		is_char[s] = (TYPEOF(src) == STRSXP);
	}

	// allocate temporary buffer for decoding
	mkchar_init(&mk);

	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		s = INTEGER(source)[i] - 1;

		// if the source is character, we might be able to use that
		// instead of allocating a new object
		alloc = 1;
		if (is_char[s]) {
			r = (R_xlen_t)(REAL(row)[i] - 1);
			src = VECTOR_ELT(sources, s);
			str = STRING_ELT(src, r);

			if (str == NA_STRING) {
				alloc = 0;
			} else if (INTEGER(start)[i] == 1) {
				len = LENGTH(str);
				if (INTEGER(stop)[i] == len) {
					alloc = 0;
				}
			}
		}

		if (alloc) {
			str = mkchar_get(&mk, &text[i]);
		}
		SET_STRING_ELT(ans, i, str);
	}

	UNPROTECT(1);
	return ans;
}
