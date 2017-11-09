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
	CALLDEF(abbreviations, 1),
	CALLDEF(alloc_text_handle, 0),
	CALLDEF(anyNA_text, 1),
	CALLDEF(as_character_json, 1),
	CALLDEF(as_character_text, 1),
	CALLDEF(as_integer_json, 1),
	CALLDEF(as_double_json, 1),
	CALLDEF(as_list_json, 1),
	CALLDEF(as_logical_json, 1),
	CALLDEF(as_text_character, 2),
	CALLDEF(as_text_filter_connector, 1),
	CALLDEF(as_text_json, 2),
	CALLDEF(dim_json, 1),
	CALLDEF(is_na_text, 1),
	CALLDEF(length_json, 1),
	CALLDEF(length_text, 1),
	CALLDEF(logging_off, 0),
	CALLDEF(logging_on, 0),
	CALLDEF(mmap_ndjson, 2),
	CALLDEF(names_json, 1),
	CALLDEF(names_text, 1),
	CALLDEF(print_json, 1),
	CALLDEF(read_ndjson, 2),
	CALLDEF(simplify_json, 1),
	CALLDEF(stem_snowball, 2),
	CALLDEF(stopwords, 1),
	CALLDEF(subscript_json, 2),
	CALLDEF(subset_json, 3),
	CALLDEF(term_stats, 7),
	CALLDEF(term_matrix, 4),
	CALLDEF(text_c, 3),
	CALLDEF(text_count, 2),
	CALLDEF(text_detect, 2),
	CALLDEF(text_locate, 2),
	CALLDEF(text_match, 2),
	CALLDEF(text_nsentence, 1),
	CALLDEF(text_ntoken, 1),
	CALLDEF(text_ntype, 2),
	CALLDEF(text_split_sentences, 2),
	CALLDEF(text_split_tokens, 2),
	CALLDEF(text_sub, 3),
	CALLDEF(text_trunc, 3),
	CALLDEF(text_tokens, 1),
	CALLDEF(text_types, 2),
	CALLDEF(text_valid, 1),
        {NULL, NULL, 0}
};


void R_init_corpus(DllInfo *dll)
{
	R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
	R_forceSymbols(dll, TRUE);
}
