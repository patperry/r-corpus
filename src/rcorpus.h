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

#ifndef RCORPUS_H
#define RCORPUS_H

#include <stddef.h>
#include <Rdefines.h>

struct data;
struct schema;

struct dataset {
	struct schema *schema;
	struct data *rows;
	R_xlen_t nrow;
	int type_id;
	int kind;
};

struct mmap {
	int fd;
	void *addr;
	size_t size;
};

// data set
SEXP alloc_dataset(struct schema *schema, int type_id, struct data *rows,
		   R_xlen_t nrow, SEXP prot);
int is_dataset(SEXP data);
struct dataset *as_dataset(SEXP data);

SEXP dim_dataset(SEXP data);
SEXP length_dataset(SEXP data);
SEXP names_dataset(SEXP data);

// memory map
SEXP alloc_mmap(SEXP file);
int is_mmap(SEXP sbuf);
struct mmap *as_mmap(SEXP sbuf);

// data schema
SEXP alloc_schema(void);
int is_schema(SEXP schema);
struct schema *as_schema(SEXP schema);

// json values
SEXP read_json(SEXP file);

#endif /* RCORPUS_H */
