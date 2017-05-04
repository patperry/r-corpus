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
#include "corpus/src/xalloc.h"
#include "rcorpus.h"

#define TEXT_TAG install("corpus::text")


enum source_type {
	SOURCE_NONE = 0,
	SOURCE_CHAR,
	SOURCE_DATASET
};


struct source {
	int type;
	union {
		const struct jsondata *set;
		SEXP chars;
	} data;
	R_xlen_t nrow;
};


static void load_text(SEXP x);


static int is_source(SEXP x)
{
	return (x == R_NilValue || TYPEOF(x) == STRSXP || is_jsondata(x));
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
	} else if (is_jsondata(value)) {
		source->type = SOURCE_DATASET;
		source->data.set = as_jsondata(value);
		source->nrow = source->data.set->nrow;
	} else {
		error("invalid text source;"
		      " should be 'character', 'jsondata', or NULL");
	}
}


static void free_text(SEXP stext)
{
        struct text *text = R_ExternalPtrAddr(stext);
        free(text);
}


SEXP alloc_text(SEXP sources, SEXP source, SEXP row, SEXP start, SEXP stop)
{
	SEXP ans, handle, names, sclass, src, row_names, table;
	R_xlen_t n;
	int s, nsrc;

	PROTECT(handle = R_MakeExternalPtr(NULL, TEXT_TAG, R_NilValue));
	R_RegisterCFinalizerEx(handle, free_text, TRUE);

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

	nsrc = (int)XLENGTH(sources);
	for (s = 0; s < nsrc; s++) {
		src = VECTOR_ELT(sources, s);
		if (!is_source(src)) {
			error("'sources' element at index %d is invalid;"
			      "should be a 'character' or 'jsondata'", s + 1);
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

	PROTECT(ans = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(ans, 0, handle);
	SET_VECTOR_ELT(ans, 1, sources);
	SET_VECTOR_ELT(ans, 2, table);
	SET_VECTOR_ELT(ans, 3, R_NilValue);

	PROTECT(names = allocVector(STRSXP, 4));
        SET_STRING_ELT(names, 0, mkChar("handle"));
        SET_STRING_ELT(names, 1, mkChar("sources"));
        SET_STRING_ELT(names, 2, mkChar("table"));
        SET_STRING_ELT(names, 3, mkChar("names"));
        setAttrib(ans, R_NamesSymbol, names);

	PROTECT(sclass = allocVector(STRSXP, 1));
        SET_STRING_ELT(sclass, 0, mkChar("text"));
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


struct text *as_text(SEXP stext, R_xlen_t *lenptr)
{
	SEXP handle, source, table;
	struct text *text;

	if (!is_text(stext)) {
		error("invalid 'text' object");
	}

	handle = getListElement(stext, "handle");
	text = R_ExternalPtrAddr(handle);
	if (!text) {
		load_text(stext);
		handle = getListElement(stext, "handle");
		text = R_ExternalPtrAddr(handle);
	}

	if (lenptr) {
		table = getListElement(stext, "table");
		if (table == R_NilValue) {
			*lenptr = 0;
		} else {
			source = getListElement(table, "source");
			if (source == R_NilValue) {
				*lenptr = 0;
			} else {
				*lenptr = XLENGTH(source);
			}
		}
	}

	return text;
}


SEXP as_text_jsondata(SEXP sdata)
{
	SEXP ans, handle, sources, source, row, start, stop;
	const struct jsondata *d = as_jsondata(sdata);
	struct text *text;
	R_xlen_t i, nrow = d->nrow;
	int err;

	PROTECT(sources = allocVector(VECSXP, 1));
	SET_VECTOR_ELT(sources, 0, sdata);

	PROTECT(source = allocVector(INTSXP, nrow));
	for (i = 0; i < nrow; i++) {
		INTEGER(source)[i] = 1;
	}

	PROTECT(row = allocVector(REALSXP, nrow));
	for (i = 0; i < nrow; i++) {
		REAL(row)[i] = (double)(i + 1);
	}

	PROTECT(start = allocVector(INTSXP, nrow));
	PROTECT(stop = allocVector(INTSXP, nrow));
	PROTECT(ans = alloc_text(sources, source, row, start, stop));
	handle = getListElement(ans, "handle");

	text = calloc(nrow, sizeof(*text));
	if (nrow > 0 && !text) {
		error("failed allocating memory (%"PRIu64" objects"
		      " of size %u bytes)", (uint64_t)nrow, sizeof(*text));
	}
	R_SetExternalPtrAddr(handle, text);

	for (i = 0; i < nrow; i++) {
		if ((err = data_text(&d->rows[i], &text[i]))) {
			text[i].ptr = NULL;
			text[i].attr = 0;
			INTEGER(start)[i] = NA_INTEGER;
			INTEGER(stop)[i] = NA_INTEGER;
		} else {
			if (TEXT_SIZE(&text[i]) > INT_MAX) {
				error("text size (%"PRIu64 "bytes)"
				      "exceeds maximum (%d bytes)",
				      (uint64_t)TEXT_SIZE(&text[i]),
				      INT_MAX);
			}
			INTEGER(start)[i] = 1;
			INTEGER(stop)[i] = (int)TEXT_SIZE(&text[i]);
		}
	}

	UNPROTECT(6);
	return ans;
}


SEXP as_text_character(SEXP x)
{
	SEXP ans, handle, sources, source, row, start, stop, str;
	struct text *text;
	const char *ptr;
	R_xlen_t i, nrow, len;
	int err, iname, duped = 0;

	if (TYPEOF(x) != STRSXP) {
	       error("invalid 'character' object");
	}

	nrow = XLENGTH(x);
	if ((uint64_t)nrow > (((uint64_t)1) << DBL_MANT_DIG)) {
		error("text vector length (%"PRIu64")"
		      " exceeds maximum (%"PRIu64")",
		      (uint64_t)nrow, ((uint64_t)1) << DBL_MANT_DIG);
	}

	PROTECT(sources = allocVector(VECSXP, 1));
	SET_VECTOR_ELT(sources, 0, x);

	PROTECT(source = allocVector(INTSXP, nrow));
	for (i = 0; i < nrow; i++) {
		INTEGER(source)[i] = 1;
	}

	PROTECT(row = allocVector(REALSXP, nrow));
	for (i = 0; i < nrow; i++) {
		REAL(row)[i] = (double)(i + 1);
	}

	PROTECT(start = allocVector(INTSXP, nrow));
	PROTECT(stop = allocVector(INTSXP, nrow));
	PROTECT(ans = alloc_text(sources, source, row, start, stop));
	handle = getListElement(ans, "handle");

	text = calloc(nrow, sizeof(*text));
	if (nrow > 0 && !text) {
		error("failed allocating memory (%"PRIu64" objects"
		      " of size %u bytes)", (uint64_t)nrow, sizeof(*text));
	}
	R_SetExternalPtrAddr(handle, text);

	iname = findListElement(ans, "names");
	SET_VECTOR_ELT(ans, iname, getAttrib(x, R_NamesSymbol));

	for (i = 0; i < nrow; i++) {
		str = STRING_ELT(x, i);

		// handle NA
		if (str == NA_STRING) {
			text[i].ptr = NULL;
			text[i].attr = 0;
			INTEGER(start)[i] = NA_INTEGER;
			INTEGER(stop)[i] = NA_INTEGER;
			continue;
		}

		// convert to UTF-8
		ptr = translateCharUTF8(str);
		if (ptr != CHAR(str)) {
			if (!duped) {
				SET_VECTOR_ELT(sources, 0, (x = duplicate(x)));
				duped = 1;
			}
			str = mkCharCE(ptr, CE_UTF8);
			SET_STRING_ELT(x, i, str);
			ptr = CHAR(str);
		}

		// convert to char to text
		len = XLENGTH(str);
		if ((uint64_t)len > (uint64_t)TEXT_SIZE_MAX) {
			error("size of character object at index %"PRIu64
			      " (%"PRIu64" bytes)"
			      " exceeds maximum (%"PRIu64" bytes)",
			      (uint64_t)(i + 1), (uint64_t)len,
			      (uint64_t)TEXT_SIZE_MAX);
		}
		if (len > INT_MAX) {
			error("size of character object at index %"PRIu64
			      " (%"PRIu64" bytes)"
			      " exceeds maximum (%d bytes)",
			      (uint64_t)(i + 1), (uint64_t)len, INT_MAX);
		}
		if ((err = text_assign(&text[i], (uint8_t *)ptr, (size_t)len,
					TEXT_NOESCAPE))) {
			error("character object at index %"PRIu64
			      " contains invalid UTF-8", (uint64_t)(i + 1));
		}

		INTEGER(start)[i] = 1;
		INTEGER(stop)[i] = (int)TEXT_SIZE(&text[i]);
	}

	UNPROTECT(6);
	return ans;
}


SEXP alloc_na_text(void)
{
	return as_text_character(ScalarString(NA_STRING));
}


SEXP coerce_text(SEXP sx)
{
	SEXP ans;

	if (is_text(sx)) {
		return sx;
	} else if (is_jsondata(sx)) {
		return as_text_jsondata(sx);
	}

	PROTECT(sx = coerceVector(sx, STRSXP));
	ans = as_text_character(sx);
	UNPROTECT(1);

	return ans;
}


static void load_text(SEXP x)
{
	SEXP shandle, srow, ssource, sstart, sstop, ssources, src, str, stable;
	const double *row;
	const int *source, *start, *stop;
	struct text *text;
	struct text txt;
	struct source *sources;
	const uint8_t *ptr;
	double r;
	R_xlen_t i, j, len, nrow;
	int s, nsrc, err, begin, end, flags;

	shandle = getListElement(x, "handle");

	text = R_ExternalPtrAddr(shandle);
	if (text) {
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

	text = calloc(nrow, sizeof(*text));
	if (nrow > 0 && !text) {
		error("failed allocating memory (%"PRIu64" objects"
		      " of size %u bytes)", (uint64_t)nrow, sizeof(*text));
	}
	R_SetExternalPtrAddr(shandle, text);

	for (i = 0; i < nrow; i++) {
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

		switch (sources[s].type) {
		case SOURCE_CHAR:
			str = STRING_ELT(sources[s].data.chars, j);
			ptr = (const uint8_t *)CHAR(str);
			len = XLENGTH(str);
			flags = TEXT_NOESCAPE;
			err = text_assign(&txt, ptr, len, flags);
			if (err) {
				error("character object in source %d"
				      " at index %"PRIu64
				      " contains invalid UTF-8",
				      s + 1, (uint64_t)(j + 1));
			}
			break;

		case SOURCE_DATASET:
			// no need to validate input (handled by jsondata)
			data_text(&sources[s].data.set->rows[j], &txt);
			flags = 0;
			break;

		default:
			txt.ptr = NULL;
			txt.attr = 0;
			flags = 0;
			break;
		}

		begin = (start[i] < 1) ? 0 : (start[i] - 1);
		end = stop[i] < start[i] ? start[i] : stop[i];
		if ((size_t)end > TEXT_SIZE(&txt)) {
			end = (int)TEXT_SIZE(&txt);
		}


		// this could be made more efficient; add a
		// 'can_break?' function to corpus/text.h
		err = text_assign(&text[i], txt.ptr + begin, end - begin,
				  flags);

		if (err) {
			error("text span in row[[%"PRIu64"]]"
			      " starts or ends in the middle"
			      " of a multi-byte character", i + 1);
		}
	}
}


SEXP subset_text_handle(SEXP handle, SEXP si)
{
	SEXP ans;
	const struct text *text = R_ExternalPtrAddr(handle);
	struct text *sub;
	R_xlen_t i, n;
	double ix;

	PROTECT(ans = R_MakeExternalPtr(NULL, TEXT_TAG, R_NilValue));
	R_RegisterCFinalizerEx(ans, free_text, TRUE);

	if (text) {
		n = XLENGTH(si);
		sub = xcalloc(n, sizeof(*sub));
		if (n && !sub) {
			error("failed allocating %"PRIu64" bytes",
			      (uint64_t)n * sizeof(*sub));
		}
		R_SetExternalPtrAddr(ans, sub);

		for (i = 0; i < n; i++) {
			ix = REAL(si)[i] - 1;
			sub[i] = text[(R_xlen_t)ix];
		}
	}

	UNPROTECT(1);
	return ans;
}
