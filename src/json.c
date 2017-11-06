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

#include <assert.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "rcorpus.h"

#define JSON_TAG install("corpus::json")


static SEXP subrows_json(SEXP sdata, SEXP si);
static SEXP subfield_json(SEXP sdata, SEXP sname);


static void free_json(SEXP sjson)
{
        struct json *obj = R_ExternalPtrAddr(sjson);
        R_SetExternalPtrAddr(sjson, NULL);
	if (obj) {
		corpus_schema_destroy(&obj->schema);
		corpus_free(obj->rows);
		corpus_free(obj);
	}
}


static void *realloc_nonnull(void *ptr, size_t size)
{
	void *ans;
	int err = 0;

       	TRY_ALLOC(ans = corpus_realloc(ptr, size ? size : 1));
out:
	CHECK_ERROR(err);
	return ans;
}


static void *calloc_nonnull(size_t count, size_t size)
{
	void *ans;
	int err = 0;

	TRY_ALLOC(ans = corpus_calloc(count ? count : 1, size ? size : 1));
out:
	CHECK_ERROR(err);
	return ans;
}


static void *malloc_nonnull(size_t size)
{
	return realloc_nonnull(NULL, size);
}


SEXP alloc_json(SEXP sbuffer, SEXP sfield, SEXP srows, SEXP stext)
{
	SEXP ans = R_NilValue, sclass, shandle, snames;
	struct json *obj = NULL;
	int err = 0, nprot = 0;

	PROTECT(shandle = R_MakeExternalPtr(NULL, JSON_TAG, R_NilValue));
	nprot++;
	R_RegisterCFinalizerEx(shandle, free_json, TRUE);

	TRY_ALLOC(obj = corpus_malloc(sizeof(*obj)));
	TRY(corpus_schema_init(&obj->schema));

	obj->rows = NULL;
	obj->nrow = 0;
	obj->type_id = CORPUS_DATATYPE_NULL;
	obj->kind = CORPUS_DATATYPE_NULL;

	R_SetExternalPtrAddr(shandle, obj);
	obj = NULL;

	PROTECT(ans = allocVector(VECSXP, 5)); nprot++;
	SET_VECTOR_ELT(ans, 0, shandle);
	SET_VECTOR_ELT(ans, 1, sbuffer);
	SET_VECTOR_ELT(ans, 2, sfield);
	SET_VECTOR_ELT(ans, 3, srows);
	SET_VECTOR_ELT(ans, 4, stext);

	PROTECT(snames = allocVector(STRSXP, 5)); nprot++;
	SET_STRING_ELT(snames, 0, mkChar("handle"));
	SET_STRING_ELT(snames, 1, mkChar("buffer"));
	SET_STRING_ELT(snames, 2, mkChar("field"));
	SET_STRING_ELT(snames, 3, mkChar("rows"));
	SET_STRING_ELT(snames, 4, mkChar("text"));
	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(sclass = allocVector(STRSXP, 1)); nprot++;
	SET_STRING_ELT(sclass, 0, mkChar("corpus_json"));
	setAttrib(ans, R_ClassSymbol, sclass);

out:
	corpus_free(obj);
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


static void grow_datarows(struct corpus_data **rowsptr, R_xlen_t *nrow_maxptr)
{
	struct corpus_data *rows = *rowsptr;
	void *base = rows;
	size_t size = (size_t)*nrow_maxptr;
	int err = 0;

	TRY(size == R_XLEN_T_MAX ? CORPUS_ERROR_OVERFLOW : 0);
	TRY(corpus_bigarray_grow(&base, &size, sizeof(*rows), size, 1));

	*rowsptr = base;
	*nrow_maxptr = size;
out:
	CHECK_ERROR(err);
}


static void json_load(SEXP sdata)
{
	SEXP shandle, sparent_handle, sbuffer, sfield, stext, sfield_path,
	     srows, sparent, sparent2;
	struct json *obj, *parent;
	struct corpus_filebuf *buf;
	struct corpus_filebuf_iter it;
	const uint8_t *ptr, *begin, *line_end, *end;
	uint_fast8_t ch;
	size_t size;
	R_xlen_t nrow, nrow_max, j, m;
	int err = 0, type_id;

	shandle = getListElement(sdata, "handle");
	obj = R_ExternalPtrAddr(shandle);
	if (obj && obj->rows) {
		return;
	}

	// set up the finalizer
	R_RegisterCFinalizerEx(shandle, free_json, TRUE);

	sbuffer = getListElement(sdata, "buffer");
	stext = getListElement(sdata, "text");
	PROTECT(sparent = alloc_json(sbuffer, R_NilValue, R_NilValue, stext));
	sparent_handle = getListElement(sparent, "handle");
	parent = R_ExternalPtrAddr(sparent_handle);

	type_id = CORPUS_DATATYPE_NULL;
	nrow = 0;
	nrow_max = 0;

	if (is_filebuf(sbuffer)) {
		buf = as_filebuf(sbuffer);

		corpus_filebuf_iter_make(&it, buf);
		while (corpus_filebuf_iter_advance(&it)) {
			RCORPUS_CHECK_INTERRUPT(nrow);

			if (nrow == nrow_max) {
				grow_datarows(&parent->rows, &nrow_max);
			}

			ptr = it.current.ptr;
			size = it.current.size;

			TRY(corpus_data_assign(&parent->rows[nrow],
					       &parent->schema, ptr, size));

			TRY(corpus_schema_union(&parent->schema, type_id,
						parent->rows[nrow].type_id,
						&type_id));
			nrow++;
		}
	} else {
		// parse data from buffer
		begin = (const uint8_t *)RAW(sbuffer);
		end = begin + XLENGTH(sbuffer);
		ptr = begin;

		while (ptr != end) {
			RCORPUS_CHECK_INTERRUPT(nrow);

			if (nrow == nrow_max) {
				grow_datarows(&parent->rows, &nrow_max);
			}

			line_end = ptr;
			do {
				ch = *line_end++;
			} while (ch != '\n' && line_end != end);

			size = (size_t)(line_end - ptr);

			TRY(corpus_data_assign(&parent->rows[nrow],
					       &parent->schema, ptr, size));

			TRY(corpus_schema_union(&parent->schema, type_id,
						parent->rows[nrow].type_id,
						&type_id));
			nrow++;
			ptr = line_end;
		}
	}

	// free excess memory, ensuring result is non-NULL
	parent->rows = realloc_nonnull(parent->rows,
				       nrow * sizeof(*parent->rows));

	// set the attribute fields
	parent->nrow = nrow;
	parent->type_id = type_id;
	parent->kind = (type_id < 0 ? CORPUS_DATATYPE_ANY
				    : parent->schema.types[type_id].kind);

	// first extract the rows from the parent...
	srows = getListElement(sdata, "rows");
	if (srows != R_NilValue) {
		PROTECT(sparent2 = subrows_json(sparent, srows));
		free_json(sparent_handle);
		UNPROTECT(2);
		PROTECT(sparent = sparent2);
		sparent_handle = getListElement(sparent, "handle");
	}

	// ...then extract the field
	sfield_path = getListElement(sdata, "field");
	if (sfield_path != R_NilValue) {
		m = XLENGTH(sfield_path);
		for (j = 0; j < m; j++) {
			sfield = STRING_ELT(sfield_path, j);
			PROTECT(sparent2 = subfield_json(sparent, sfield));
			free_json(sparent_handle);
			UNPROTECT(2);
			PROTECT(sparent = sparent2);
			sparent_handle = getListElement(sparent, "handle");
		}
	}

	// steal the handle from the parent
	obj = R_ExternalPtrAddr(sparent_handle);
	R_SetExternalPtrAddr(sparent_handle, NULL);
	free_json(shandle);
	R_SetExternalPtrAddr(shandle, obj);

out:
	CHECK_ERROR_FORMAT(err, "failed parsing row %"PRIu64" of JSON data",
			   (uint64_t)(nrow + 1));
	UNPROTECT(1);
}


int is_json(SEXP sdata)
{
	SEXP handle, buffer;

	if (!isVectorList(sdata)) {
		return 0;
	}

	handle = getListElement(sdata, "handle");
	if (handle == R_NilValue) {
		return 0;
	}

	buffer = getListElement(sdata, "buffer");
	if (!(TYPEOF(buffer) == RAWSXP || is_filebuf(buffer))) {
		return 0;
	}

	return ((TYPEOF(handle) == EXTPTRSXP)
		&& (R_ExternalPtrTag(handle) == JSON_TAG));
}


struct json *as_json(SEXP sdata)
{
	SEXP shandle;
	struct json *obj;

	if (!is_json(sdata)) {
		error("invalid JSON object");
	}

	json_load(sdata);

	shandle = getListElement(sdata, "handle");
	obj = R_ExternalPtrAddr(shandle);

	return obj;
}


SEXP dim_json(SEXP sdata)
{
	SEXP dims;
	const struct json *d = as_json(sdata);
	const struct corpus_datatype *t;
	const struct corpus_datatype_record *r;

	if (d->kind != CORPUS_DATATYPE_RECORD) {
		return R_NilValue;
	}

	t = &d->schema.types[d->type_id];
	r = &t->meta.record;

	if (d->nrow > INT_MAX) {
		PROTECT(dims = allocVector(REALSXP, 2));
		REAL(dims)[0] = (double)d->nrow;
		REAL(dims)[1] = (double)r->nfield;
	} else {
		PROTECT(dims = allocVector(INTSXP, 2));
		INTEGER(dims)[0] = d->nrow;
		INTEGER(dims)[1] = (int)r->nfield;
	}
	UNPROTECT(1);

	return dims;
}


SEXP length_json(SEXP sdata)
{
	const struct json *d = as_json(sdata);
	const struct corpus_datatype *t;
	const struct corpus_datatype_record *r;

	if (d->kind == CORPUS_DATATYPE_RECORD) {
		t = &d->schema.types[d->type_id];
		r = &t->meta.record;
		return ScalarInteger(r->nfield);
	}

	if (d->nrow > INT_MAX) {
		return ScalarReal((double)d->nrow);
	} else {
		return ScalarInteger((int)d->nrow);
	}
}


SEXP names_json(SEXP sdata)
{
	SEXP names, str;
	const struct json *d = as_json(sdata);
	const struct corpus_datatype *t;
	const struct corpus_datatype_record *r;
	const struct utf8lite_text *name;
	int i;

	if (d->kind != CORPUS_DATATYPE_RECORD) {
		return R_NilValue;
	}

	t = &d->schema.types[d->type_id];
	r = &t->meta.record;

	PROTECT(names = allocVector(STRSXP, r->nfield));
	for (i = 0; i < r->nfield; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		name = &d->schema.names.types[r->name_ids[i]].text;
		str = mkCharLenCE((char *)name->ptr,
				  UTF8LITE_TEXT_SIZE(name), CE_UTF8);
		SET_STRING_ELT(names, i, str);
	}

	UNPROTECT(1);
	return names;
}


SEXP print_json(SEXP sdata)
{
	const struct json *d = as_json(sdata);
	struct utf8lite_render r;
	int err = 0, has_render = 0;

	TRY(utf8lite_render_init(&r, UTF8LITE_ESCAPE_CONTROL));
	has_render = 1;

	corpus_render_datatype(&r, &d->schema, d->type_id);
	TRY(r.error);

	if (d->kind == CORPUS_DATATYPE_RECORD) {
		Rprintf("JSON data set with %"PRIu64" rows"
			" of the following type:\n%s\n",
			(uint64_t)d->nrow, r.string);
	} else {
		Rprintf("JSON data set with %"PRIu64" rows"
			" of type %s\n", (uint64_t)d->nrow, r.string);
	}
out:
	if (has_render) {
		utf8lite_render_destroy(&r);
	}
	CHECK_ERROR(err);

	return sdata;
}


SEXP subscript_json(SEXP sdata, SEXP si)
{
	SEXP ans, sname;
	const struct json *d = as_json(sdata);
	const struct corpus_schema *s = &d->schema;
	const struct corpus_datatype *t;
	const struct corpus_datatype_record *r;
	const struct utf8lite_text *name;
	double i;
	int name_id;

	if (!(isReal(si) && LENGTH(si) == 1)) {
		error("invalid 'i' argument");
	}
	i = REAL(si)[0];

	if (d->kind != CORPUS_DATATYPE_RECORD) {
		ans = subrows_json(sdata, si);
	} else {
		t = &d->schema.types[d->type_id];
		r = &t->meta.record;

		if (!(1 <= i && i <= r->nfield)) {
			error("invalid column subscript: \"%g\"", i);
		}
		name_id = r->name_ids[(int)(i - 1)];
		name = &s->names.types[name_id].text;

		PROTECT(sname = mkCharLenCE((const char *)name->ptr,
					    (int)UTF8LITE_TEXT_SIZE(name),
					    CE_UTF8));
		PROTECT(ans = subfield_json(sdata, sname));
		UNPROTECT(2);
	}

	return ans;
}


SEXP subrows_json(SEXP sdata, SEXP si)
{
	SEXP ans, shandle, sbuffer, sfield, srows, stext, srows2;
	const struct json *obj = as_json(sdata);
	struct json *obj2;
	const struct corpus_data *src;
	const double *index;
	double *irows;
	R_xlen_t i, n, ind;
	int type_id;
	int err = 0;

	if (si == R_NilValue) {
		return sdata;
	}

	index = REAL(si);
	n = XLENGTH(si);

	sbuffer = getListElement(sdata, "buffer");
	sfield = getListElement(sdata, "field");
	srows = getListElement(sdata, "rows");
	stext = getListElement(sdata, "text");

	PROTECT(srows2 = allocVector(REALSXP, n));
	irows = REAL(srows2);

	PROTECT(ans = alloc_json(sbuffer, sfield, srows2, stext));
	shandle = getListElement(ans, "handle");
	obj2 = R_ExternalPtrAddr(shandle);

	obj2->rows = malloc_nonnull(n * sizeof(*obj2->rows));
	type_id = CORPUS_DATATYPE_NULL;

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!(1 <= index[i] && index[i] <= (double)obj->nrow)) {
			error("invalid index: %g", index[i]);
		}

		ind = (R_xlen_t)(index[i] - 1);
		if (srows == R_NilValue) {
			irows[i] = index[i];
		} else {
			irows[i] = REAL(srows)[ind];
		}
		src = &obj->rows[ind];

		TRY(corpus_data_assign(&obj2->rows[i], &obj2->schema,
				       src->ptr, src->size));

		TRY(corpus_schema_union(&obj2->schema, type_id,
					obj2->rows[i].type_id, &type_id));
	}

	// set the fields
	obj2->nrow = n;
	obj2->type_id = type_id;
	obj2->kind = (type_id < 0 ? CORPUS_DATATYPE_ANY
				  : obj2->schema.types[type_id].kind);
out:
	CHECK_ERROR_FORMAT(err, "failed parsing row %"PRIu64" of JSON file",
			   (uint64_t)(irows[i] + 1));
	UNPROTECT(2);
	return ans;
}


SEXP subfield_json(SEXP sdata, SEXP sname)
{
	SEXP ans, sbuffer, sfield, sfield2, shandle, srows, stext;
	const struct json *obj = as_json(sdata);
	struct utf8lite_text name;
	struct corpus_data field;
	const char *name_ptr;
	size_t name_len;
	struct json *obj2;
	R_xlen_t i, n;
	int err = 0, nprot = 0, j, m, name_id, type_id;

	if (sname == R_NilValue) {
		return sdata;
	}
	TRY(TYPEOF(sname) != CHARSXP ? CORPUS_ERROR_INTERNAL : 0);

	// name must be in utf8 encoding (or 'native' on non-Windows)
	name_ptr = CHAR(sname);
	name_len = strlen(name_ptr);
	PROTECT(sname = mkCharLenCE(name_ptr, name_len, CE_UTF8)); nprot++;
	TRY(utf8lite_text_assign(&name, (uint8_t *)name_ptr, name_len, 0,
				 NULL));

	if (!corpus_symtab_has_type(&obj->schema.names, &name, &name_id)) {
		UNPROTECT(nprot);
		return R_NilValue;
	}

	sbuffer = getListElement(sdata, "buffer");
	sfield = getListElement(sdata, "field");
	srows = getListElement(sdata, "rows");
	stext = getListElement(sdata, "text");

	if (sfield == R_NilValue) {
		m = 0;
	} else {
		m = LENGTH(sfield);
	}

	PROTECT(sfield2 = allocVector(STRSXP, m + 1)); nprot++;
	for (j = 0; j < m; j++) {
		RCORPUS_CHECK_INTERRUPT(j);
		SET_STRING_ELT(sfield2, j, STRING_ELT(sfield, j));
	}
	SET_STRING_ELT(sfield2, m, sname);

	PROTECT(ans = alloc_json(sbuffer, sfield2, srows, stext)); nprot++;
	shandle = getListElement(ans, "handle");
	obj2 = R_ExternalPtrAddr(shandle);

	n = obj->nrow;
	obj2->rows = malloc_nonnull(n * sizeof(*obj2->rows));

	type_id = CORPUS_DATATYPE_NULL;
	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		// fails if the field is null
		corpus_data_field(&obj->rows[i], &obj->schema, name_id,
				  &field);

		TRY(corpus_data_assign(&obj2->rows[i], &obj2->schema,
				       field.ptr, field.size));

		TRY(corpus_schema_union(&obj2->schema, type_id,
					obj2->rows[i].type_id, &type_id));
	}

	obj2->nrow = n;
	obj2->type_id = type_id;
	obj2->kind = (type_id < 0 ? CORPUS_DATATYPE_ANY
				  : obj2->schema.types[type_id].kind);
	err = 0;
out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP subset_json(SEXP sdata, SEXP si, SEXP sj)
{
	SEXP ans;

	if (si == R_NilValue) {
		if (sj == R_NilValue) {
			return sdata;
		} else {
			// i is NULL, j is non-NULL
			return subscript_json(sdata, sj);
		}
	} else if (sj == R_NilValue) {
		return subrows_json(sdata, si);
	} else {
		// both i and j are non-NULL

		// get the column (do this first to preserve the column type)
		PROTECT(sdata = subscript_json(sdata, sj));

		// get the rows
		ans = subrows_json(sdata, si);
		UNPROTECT(1);
		return ans;
	}
}


SEXP as_double_json(SEXP sdata)
{
	SEXP ans;
	const struct json *d = as_json(sdata);
	double *val;
	R_xlen_t i, n = d->nrow;
	int err, overflow, underflow;

	PROTECT(ans = allocVector(REALSXP, n));
	val = REAL(ans);
	overflow = 0;
	underflow = 0;

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		err = corpus_data_double(&d->rows[i], &val[i]);
		if (err == CORPUS_ERROR_INVAL) {
			val[i] = NA_REAL;
		} else if (err == CORPUS_ERROR_RANGE) {
			if (val[i] == 0) {
				underflow = 1;
			} else {
				overflow = 1;
			}
		}
	}

	if (overflow) {
		warning("Inf introduced by coercion to double-precision range");
	}

	if (underflow) {
		warning("0 introduced by coercion to double-precision range");
	}

	UNPROTECT(1);
	return ans;
}


static SEXP as_integer_json_check(SEXP sdata, int *overflowptr)
{
	SEXP ans;
	const struct json *d = as_json(sdata);
	int *val;
	R_xlen_t i, n = d->nrow;
	int err, overflow;

	PROTECT(ans = allocVector(INTSXP, n));
	val = INTEGER(ans);
	overflow = 0;

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		err = corpus_data_int(&d->rows[i], &val[i]);
		if (err == CORPUS_ERROR_INVAL) {
			val[i] = NA_INTEGER;
		} else {
			if (err == CORPUS_ERROR_RANGE
					|| val[i] == NA_INTEGER) {
				overflow = 1;
				val[i] = NA_INTEGER;
			}
		}
	}

	if (overflowptr) {
		*overflowptr = overflow;
	}

	UNPROTECT(1);
	return ans;
}


SEXP as_integer_json(SEXP sdata)
{
	SEXP ans;
	int overflow;

	PROTECT(ans = as_integer_json_check(sdata, &overflow));
	if (overflow) {
		warning("NAs introduced by coercion to integer range");
	}

	UNPROTECT(1);
	return ans;
}


SEXP as_logical_json(SEXP sdata)
{
	SEXP ans;
	const struct json *d = as_json(sdata);
	R_xlen_t i, n = d->nrow;
	int *val;
	int b, err;

	PROTECT(ans = allocVector(LGLSXP, n));
	val = LOGICAL(ans);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		err = corpus_data_bool(&d->rows[i], &b);
		if (err == CORPUS_ERROR_INVAL) {
			val[i] = NA_LOGICAL;
		} else {
			val[i] = b ? TRUE : FALSE;
		}
	}

	UNPROTECT(1);
	return ans;
}


SEXP as_character_json(SEXP sdata)
{
	SEXP ans;
	const struct json *d = as_json(sdata);
	struct mkchar mkchar;
	struct utf8lite_text text;
	R_xlen_t i, n = d->nrow;
	int err;

	PROTECT(ans = allocVector(STRSXP, n));

	mkchar_init(&mkchar);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		err = corpus_data_text(&d->rows[i], &text);
		if (err == CORPUS_ERROR_INVAL) {
			SET_STRING_ELT(ans, i, NA_STRING);
		} else {
			SET_STRING_ELT(ans, i, mkchar_get(&mkchar, &text));
		}
	}

	UNPROTECT(1);
	return ans;
}


static int in_string_set(SEXP strs, SEXP item)
{
	R_xlen_t i, n;
	const char *s1, *s2;

	if (strs == R_NilValue || item == NA_STRING) {
		return 0;
	}

	n = XLENGTH(strs);
	if (n == 0) {
		return 0;
	}

	// assume items, strs in same encoding (UTF-8)
	s2 = CHAR(item);

	for (i = 0; i < n; i++) {
		if (STRING_ELT(strs, i) == NA_STRING) {
			continue;
		}

		s1 = CHAR(STRING_ELT(strs, i));
		if (strcmp(s1, s2) == 0) {
			return 1;
		}
	}

	return 0;
}


static SEXP as_list_json_record(SEXP sdata)
{
	SEXP ans, ans_j, names, sbuffer, sfield, sfield2, srows, stext,
	     shandle, sname;
	const struct json *d = as_json(sdata);
	struct json *d_j;
	struct corpus_schema **schema;
	const struct corpus_datatype_record *r;
	struct corpus_data_fields it;
	R_xlen_t i, n = d->nrow, k, m;
	int err = 0, j, nfield;
	int *type_id;
	struct corpus_data **rows;
	int *cols;

	assert(d->kind == CORPUS_DATATYPE_RECORD);

	r = &d->schema.types[d->type_id].meta.record;
	nfield = r->nfield;

	sbuffer = getListElement(sdata, "buffer");
	sfield = getListElement(sdata, "field");
	srows = getListElement(sdata, "rows");
	stext = getListElement(sdata, "text");
	PROTECT(names = names_json(sdata));

	PROTECT(ans = allocVector(VECSXP, r->nfield));
	setAttrib(ans, R_NamesSymbol, names);
	rows = (struct corpus_data **)R_alloc(nfield, sizeof(*rows));
	cols = (int *)R_alloc(d->schema.names.ntype, sizeof(*cols));
	schema = (struct corpus_schema **)R_alloc(nfield, sizeof(*schema));
	type_id = (int *)R_alloc(nfield, sizeof(type_id));

	for (j = 0; j < nfield; j++) {
		RCORPUS_CHECK_INTERRUPT(j);

		cols[r->name_ids[j]] = j;

		sname = STRING_ELT(names, j);
		m = (sfield == R_NilValue) ? 0 : XLENGTH(sfield);

		PROTECT(sfield2 = allocVector(STRSXP, m + 1));
		for (k = 0; k < m; k++) {
			SET_STRING_ELT(sfield2, k, STRING_ELT(sfield, k));
		}
		SET_STRING_ELT(sfield2, m, sname);
		ans_j = alloc_json(sbuffer, sfield2, srows, stext);
		SET_VECTOR_ELT(ans, j, ans_j);
		UNPROTECT(1); // sfield2 protected by ans_j, protected by ans

		shandle = getListElement(ans_j, "handle");
		d_j = R_ExternalPtrAddr(shandle);

		// use calloc so that all items are initialized to null
		rows[j] = calloc_nonnull(n, sizeof(*rows[j]));
		d_j->rows = rows[j];
		d_j->nrow = n;
		schema[j] = &d_j->schema;
		type_id[j] = CORPUS_DATATYPE_NULL;
	}

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if ((err = corpus_data_fields(&d->rows[i], &d->schema, &it))) {
			// record is null
			continue;
		}

		while (corpus_data_fields_advance(&it)) {
			j = cols[it.name_id];
			TRY(corpus_data_assign(&rows[j][i], schema[j],
					       it.current.ptr,
					       it.current.size));

			TRY(corpus_schema_union(schema[j], rows[j][i].type_id,
						type_id[j], &type_id[j]));
		}
	}

	for (j = 0; j < nfield; j++) {
		ans_j = VECTOR_ELT(ans, j);
		shandle = getListElement(ans_j, "handle");
		d_j = R_ExternalPtrAddr(shandle);
		d_j->type_id = type_id[j];
		d_j->kind = ((type_id[j] < 0)
				? CORPUS_DATATYPE_ANY
				: schema[j]->types[type_id[j]].kind);

		ans_j = simplify_json(ans_j);
		SET_VECTOR_ELT(ans, j, ans_j);
	}

	err = 0;
out:
	CHECK_ERROR_FORMAT(err, "failed parsing row %"PRIu64
			   ", field %d of JSON data", (uint64_t)(i + 1),
			   j + 1);
	UNPROTECT(2);
	return ans;
}


SEXP as_list_json(SEXP sdata)
{
	SEXP ans, val;
	const struct json *d = as_json(sdata);
	struct decode decode;
	struct corpus_data data;
	R_xlen_t i, n = d->nrow;

	if (d->kind == CORPUS_DATATYPE_RECORD) {
		return as_list_json_record(sdata);
	}

	PROTECT(ans = allocVector(VECSXP, n));

	decode_init(&decode);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		data = d->rows[i];
		if (d->type_id != CORPUS_DATATYPE_ANY) {
			data.type_id = d->type_id; // promote the type
		}
		val = decode_sexp(&decode, &data, &d->schema);
		SET_VECTOR_ELT(ans, i, val);
	}

	if (decode.overflow) {
		warning("Inf introduced by coercion to double-precision range");
	}

	if (decode.underflow) {
		warning("0 introduced by coercion to double-precision range");
	}

	UNPROTECT(1);
	return ans;
}


SEXP simplify_json(SEXP sdata)
{
	SEXP ans, spath, sfield, stext;
	const struct json *d = as_json(sdata);
	int overflow;

	switch (d->kind) {
	case CORPUS_DATATYPE_NULL:
		if (d->nrow == 0) {
			ans = R_NilValue;
			break;
		}
		// else fall through

	case CORPUS_DATATYPE_BOOLEAN:
		ans = as_logical_json(sdata);
		break;

	case CORPUS_DATATYPE_INTEGER:
		ans = as_integer_json_check(sdata, &overflow);
		if (overflow) {
			ans = as_double_json(sdata);
		}
		break;

	case CORPUS_DATATYPE_REAL:
		ans = as_double_json(sdata);
		break;

	case CORPUS_DATATYPE_TEXT:
		spath = getListElement(sdata, "field");
		if (spath != R_NilValue && XLENGTH(spath) > 0) {
			sfield = STRING_ELT(spath, XLENGTH(spath) - 1);
			stext = getListElement(sdata, "text");
			if (in_string_set(stext, sfield)) {
				ans = as_text_json(sdata, R_NilValue);
				goto out;
			}
		}
		ans = as_character_json(sdata);
		break;

	case CORPUS_DATATYPE_RECORD:
		ans = sdata;
		break;

	default:
		ans = as_list_json(sdata);
		break;
	}

out:
	return ans;
}
