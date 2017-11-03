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
#include <stdlib.h>
#include "rcorpus.h"

static int logical_data(const struct corpus_data *d);
static int integer_data(const struct corpus_data *d, int *overflowptr);
static double real_data(const struct corpus_data *d, int *overflowptr,
			int *underflowptr);
static SEXP charsxp_data(const struct corpus_data *d, struct mkchar *mk);
static SEXP decode_array(struct decode *d, const struct corpus_data *val,
			 const struct corpus_schema *s);
static SEXP decode_record(struct decode *d, const struct corpus_data *val,
			  const struct corpus_schema *s);


void decode_init(struct decode *d)
{
	mkchar_init(&d->mkchar);
	decode_set_overflow(d, 0);
	decode_set_underflow(d, 0);
}


int decode_set_overflow(struct decode *d, int overflow)
{
	int old = d->overflow;
	d->overflow = overflow;
	return old;
}


int decode_set_underflow(struct decode *d, int underflow)
{
	int old = d->underflow;
	d->underflow = underflow;
	return old;
}


int decode_logical(struct decode *d, const struct corpus_data *val)
{
	(void)d;
	return logical_data(val);
}


int decode_integer(struct decode *d, const struct corpus_data *val)
{
	int overflow = 0;
	int i = integer_data(val, &overflow);
	if (overflow) {
		d->overflow = overflow;
	}
	return i;
}


double decode_real(struct decode *d, const struct corpus_data *val)
{
	int overflow = 0;
	int underflow = 0;
	double x = real_data(val, &overflow, &underflow);
	if (overflow) {
		d->overflow = overflow;
	}
	if (underflow) {
		d->underflow = underflow;
	}
	return x;
}


SEXP decode_charsxp(struct decode *d, const struct corpus_data *val)
{
	return charsxp_data(val, &d->mkchar);
}


SEXP decode_sexp(struct decode *d, const struct corpus_data *val,
		 const struct corpus_schema *s)
{
	SEXP ans;
	int kind, i, overflow;

	assert(val->type_id >= 0); // type cannot be ANY

	kind = s->types[val->type_id].kind;

	switch (kind) {
	case CORPUS_DATATYPE_BOOLEAN:
		ans = ScalarLogical(decode_logical(d, val));
		break;

	case CORPUS_DATATYPE_INTEGER:
		overflow = decode_set_overflow(d, 0);
		i = decode_integer(d, val);
		if (!d->overflow) {
			d->overflow = overflow;
			ans = ScalarInteger(i);
			break;
		}
		d->overflow = overflow;
		// else fall through to CORPUS_DATATYPE_REAL

	case CORPUS_DATATYPE_REAL:
		ans = ScalarReal(decode_real(d, val));
		break;

	case CORPUS_DATATYPE_TEXT:
		ans = ScalarString(decode_charsxp(d, val));
		break;

	case CORPUS_DATATYPE_ARRAY:
		ans = decode_array(d, val, s);
		break;

	case CORPUS_DATATYPE_RECORD:
		ans = decode_record(d, val, s);
		break;

	case CORPUS_DATATYPE_NULL:
	default:
		ans = R_NilValue;
		break;
	}

	return ans;
}


int logical_data(const struct corpus_data *d)
{
	int ans, err, b;

	err = corpus_data_bool(d, &b);

	if (err == CORPUS_ERROR_INVAL) {
		ans = NA_LOGICAL;
	} else {
		ans = b ? TRUE : FALSE;
	}

	return ans;
}


int integer_data(const struct corpus_data *d, int *overflowptr)
{
	int ans, err, i;
	int overflow;

	overflow = 0;

	err = corpus_data_int(d, &i);
	if (err == CORPUS_ERROR_INVAL) {
		ans = NA_INTEGER;
	} else if (err == CORPUS_ERROR_RANGE || i == NA_INTEGER) {
		ans = NA_INTEGER;
		overflow = 1;
	} else {
		ans = i;
	}

	if (overflowptr) {
		*overflowptr = overflow;
	}

	return ans;
}


double real_data(const struct corpus_data *d, int *overflowptr,
		 int *underflowptr)
{
	double ans, r;
	int err;
	int overflow;
	int underflow;

	overflow = 0;
	underflow = 0;
	err = corpus_data_double(d, &r);
	if (err == CORPUS_ERROR_INVAL) {
		ans = NA_REAL;
	} else {
		if (err == CORPUS_ERROR_RANGE) {
			if (r == 0) {
				underflow = 1;
			} else {
				overflow = 1;
			}
		}
		ans = r;
	}

	if (overflowptr) {
		*overflowptr = overflow;
	}
	if (underflowptr) {
		*underflowptr = underflow;
	}

	return ans;
}


SEXP charsxp_data(const struct corpus_data *d, struct mkchar *mk)
{
	struct utf8lite_text text;
	int err;

	err = corpus_data_text(d, &text);
	if (err == CORPUS_ERROR_INVAL) {
		return NA_STRING;
	}

	return mkchar_get(mk, &text);
}


SEXP decode_array(struct decode *d, const struct corpus_data *val,
		  const struct corpus_schema *s)
{
	SEXP ans;
	struct corpus_data_items it;
	int err, overflow, i, n;
	int arr_id, type_id;

	if ((err = corpus_data_nitem(val, s, &n))) {
		ans = R_NilValue; // null
		goto out;
	}

	corpus_data_items(val, s, &it); // won't fail if data_nitem succeeds
	i = 0;

	arr_id = val->type_id;
	type_id = s->types[arr_id].meta.array.type_id;

	switch (type_id) {
	case CORPUS_DATATYPE_BOOLEAN:
		PROTECT(ans = allocVector(LGLSXP, n));
		while (corpus_data_items_advance(&it)) {
			LOGICAL(ans)[i] = decode_logical(d, &it.current);
			i++;
		}
		break;

	case CORPUS_DATATYPE_INTEGER:
		PROTECT(ans = allocVector(INTSXP, n));
		overflow = decode_set_overflow(d, 0);

		while (corpus_data_items_advance(&it)) {
			RCORPUS_CHECK_INTERRUPT(i);

			INTEGER(ans)[i] = decode_integer(d, &it.current);
			if (d->overflow) {
				break;
			}
			i++;
		}
		if (!d->overflow) {
			d->overflow = overflow;
			break;
		}

		d->overflow = overflow;
		UNPROTECT(1);
		corpus_data_items_reset(&it);
		// fall through, decode as CORPUS_DATATYPE_REAL

	case CORPUS_DATATYPE_REAL:
		PROTECT(ans = allocVector(REALSXP, n));
		while (corpus_data_items_advance(&it)) {
			RCORPUS_CHECK_INTERRUPT(i);

			REAL(ans)[i] = decode_real(d, &it.current);
			i++;
		}
		break;

	case CORPUS_DATATYPE_TEXT:
		PROTECT(ans = allocVector(STRSXP, n));
		while (corpus_data_items_advance(&it)) {
			RCORPUS_CHECK_INTERRUPT(i);

			SET_STRING_ELT(ans, i, decode_charsxp(d, &it.current));
			i++;
		}
		break;

	default:
		PROTECT(ans = allocVector(VECSXP, n));
		while (corpus_data_items_advance(&it)) {
			RCORPUS_CHECK_INTERRUPT(i);

			SET_VECTOR_ELT(ans, i, decode_sexp(d, &it.current, s));
			i++;
		}
		break;
	}

	UNPROTECT(1);
out:
	return ans;
}


SEXP decode_record(struct decode *d, const struct corpus_data *val,
		   const struct corpus_schema *s)
{
	SEXP ans, names;
	const struct utf8lite_text *name;
	struct corpus_data_fields it;
	int err, i, n;

	assert(val->type_id >= 0);
	assert(s->types[val->type_id].kind == CORPUS_DATATYPE_RECORD);

	if ((err = corpus_data_nfield(val, s, &n))) {
		return R_NilValue; // null
	}

	PROTECT(ans = allocVector(VECSXP, n));
	PROTECT(names = allocVector(STRSXP, n));
	if (n > 0) {
		setAttrib(ans, R_NamesSymbol, names);
	}

	i = 0;
	corpus_data_fields(val, s, &it);
	while (corpus_data_fields_advance(&it)) {
		RCORPUS_CHECK_INTERRUPT(i);

		SET_VECTOR_ELT(ans, i, decode_sexp(d, &it.current, s));

		name = &s->names.types[it.name_id].text;
		SET_STRING_ELT(names, i, mkchar_get(&d->mkchar, name));
		i++;
	}
	UNPROTECT(2);
	return ans;
}
