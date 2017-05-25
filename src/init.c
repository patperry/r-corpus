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
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include "rcorpus.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}


static const R_CallMethodDef CallEntries[] = {
	CALLDEF(anyNA_text, 1),
	CALLDEF(as_character_json, 1),
	CALLDEF(as_character_text, 1),
	CALLDEF(as_integer_json, 1),
	CALLDEF(as_double_json, 1),
	CALLDEF(as_list_json, 3),
	CALLDEF(as_logical_json, 1),
	CALLDEF(as_text_character, 1),
	CALLDEF(as_text_json, 1),
	CALLDEF(datatype_json, 1),
	CALLDEF(datatypes_json, 1),
	CALLDEF(dim_json, 1),
	CALLDEF(is_na_text, 1),
	CALLDEF(length_json, 1),
	CALLDEF(length_text, 1),
	CALLDEF(logging_off, 0),
	CALLDEF(logging_on, 0),
	CALLDEF(mmap_ndjson, 1),
	CALLDEF(names_json, 1),
	CALLDEF(print_json, 1),
	CALLDEF(read_ndjson, 1),
	CALLDEF(sentences_text, 1),
	CALLDEF(simplify_json, 3),
	CALLDEF(stopwords, 1),
	CALLDEF(subscript_json, 2),
	CALLDEF(subset_json, 3),
	CALLDEF(subset_text_handle, 2),
	CALLDEF(tokens_text, 2),
	CALLDEF(term_counts_text, 3),
	CALLDEF(term_matrix_text, 4),
        {NULL, NULL, 0}
};


void R_init_corpus(DllInfo *dll)
{
	R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
	R_forceSymbols(dll, TRUE);
}


void R_unload_corpus(DllInfo *dll)
{
	(void)dll;
}
