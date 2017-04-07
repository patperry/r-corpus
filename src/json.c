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

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <Rdefines.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "corpus/src/symtab.h"
#include "corpus/src/data.h"
#include "corpus/src/datatype.h"
#include "rcorpus.h"


static void grow_rows(struct data **rowsptr, R_xlen_t *nrow_maxptr)
{
	void *base1, *base = *rowsptr;
	size_t size1, size = (size_t)*nrow_maxptr;
	size_t width = sizeof(**rowsptr);

	if (size == 0) {
		size1 = 1;
	} else {
		size1 = 1.618 * size + 1; // (golden ratio)
	}

	if (size1 < size) { // overflow
		size1 = SIZE_MAX;
	}

	if (size1 > SIZE_MAX / width) {
		free(base);
		error("number of rows (%zu) exceeds maximum (%zu)",
			size1, (size_t)SIZE_MAX / width);
	}
	if (size1 > R_XLEN_T_MAX) {
		free(base);
		error("number of rows (%zu) exceeds maximum (%"PRIu64")",
			size1, (uint64_t)R_XLEN_T_MAX);
	}

	base1 = realloc(base, size1 * width);
	if (size1 > 0 && base1 == NULL) {
		free(base);
		error("failed allocating %zu bytes", size1 * width);
	}

	*rowsptr = base1;
	*nrow_maxptr = size1;
}


SEXP read_json(SEXP sfile)
{
	SEXP sbuf, sschema, prot, res;
	const struct mmap *buf;
	struct schema *schema;
	const uint8_t *begin, *ptr, *end;
	size_t size;
	struct data *rows;
	R_xlen_t nrow, nrow_max;
	int err, type_id;

	PROTECT(sbuf = alloc_mmap(sfile));
	buf = as_mmap(sbuf);

	PROTECT(sschema = alloc_schema());
	schema = as_schema(sschema);

	begin = (uint8_t *)buf->addr;
	ptr = begin;
	end = begin + buf->size;

	type_id = DATATYPE_NULL;
	nrow = 0;
	nrow_max = 0;
	rows = NULL;

	while (begin != end) {
		if (ptr < end && *ptr++ != '\n') {
			// only break at newlines and EOF
			continue;
		}

		if (nrow == nrow_max) {
			grow_rows(&rows, &nrow_max);
		}

		size = ptr - begin;

		if ((err = data_assign(&rows[nrow], schema, begin, size))) {
			free(rows);
			error("error parsing row %"PRIu64" of JSON file",
				(uint64_t)(nrow + 1));
		}

		if ((err = schema_union(schema, type_id, rows[nrow].type_id,
					&type_id))) {
			free(rows);
			error("memory allocation failure after parsing"
			      " row %"PRIu64" of JSON file",
			      (uint64_t)(nrow + 1));
		}
		nrow++;
		begin = ptr;
	}

	// free excess memory
	rows = realloc(rows, nrow * sizeof(*rows));

	PROTECT(prot  = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(prot, 0, sbuf);
	SET_VECTOR_ELT(prot, 1, sschema);

	PROTECT(res = alloc_dataset(schema, type_id, rows, nrow, prot));

	UNPROTECT(4);
	return res;
}
