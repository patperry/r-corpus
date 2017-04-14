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
#include <stdint.h>
#include <stdlib.h>
#include <Rdefines.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/token.h"
#include "corpus/src/symtab.h"
#include "corpus/src/datatype.h"
#include "rcorpus.h"


#define SCHEMA_TAG install("corpus::schema")


static struct schema *schema_new(void)
{
	struct schema *obj = NULL;
	struct schema s;

	if (schema_init(&s) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			schema_destroy(&s);
			error("failed allocating memory (%u bytes)",
			      (unsigned)sizeof(*obj));
		}
		*obj = s;
	} else {
		error("failed allocating schema object");
	}

	return obj;
}


static void schema_free(struct schema *s)
{
	if (s) {
		schema_destroy(s);
		free(s);
	}
}


static void free_schema(SEXP sschema)
{
        struct schema *s = R_ExternalPtrAddr(sschema);
	schema_free(s);
}


SEXP alloc_schema(void)
{
	SEXP sschema;
	struct schema *s;

	s = schema_new();
	PROTECT(sschema = R_MakeExternalPtr(s, SCHEMA_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sschema, free_schema, TRUE);

	UNPROTECT(1);
	return sschema;
}


int is_schema(SEXP sschema)
{
	return ((TYPEOF(sschema) == EXTPTRSXP)
		&& (R_ExternalPtrTag(sschema) == SCHEMA_TAG));
}


struct schema *as_schema(SEXP sschema)
{
	if (!is_schema(sschema))
		error("invalid 'schema' object");
	return R_ExternalPtrAddr(sschema);
}
