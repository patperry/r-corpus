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
#include <stdint.h>

#include <Rdefines.h>

#include "corpus/lib/utf8lite/src/utf8lite.h"
#include "corpus/src/array.h"
#include "corpus/src/error.h"
#include "corpus/src/filebuf.h"
#include "corpus/src/memory.h"
#include "corpus/src/stopword.h"
#include "corpus/src/table.h"
#include "corpus/src/tree.h"
#include "corpus/src/termset.h"
#include "corpus/src/textset.h"
#include "corpus/src/stem.h"
#include "corpus/src/symtab.h"
#include "corpus/src/datatype.h"
#include "corpus/src/data.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/sentfilter.h"
#include "corpus/src/search.h"
#include "corpus/src/ngram.h"


#define RCORPUS_CHECK_EVERY 1000
#define RCORPUS_CHECK_INTERRUPT(i) \
	do { \
		if (((i) + 1) % RCORPUS_CHECK_EVERY == 0) { \
			R_CheckUserInterrupt(); \
		} \
	} while (0)

#define TRY(x) \
	do { \
		if ((err = (x))) { \
			goto out; \
		} \
	} while (0)

#define TRY_ALLOC(x) \
	do { \
		if ((err = (x) ? 0 : CORPUS_ERROR_NOMEM)) { \
			goto out; \
		} \
	} while (0)

#define CHECK_ERROR_FORMAT_SEP(err, sep, fmt, ...) \
	do { \
		switch (err) { \
		case 0: \
			break; \
		case CORPUS_ERROR_INVAL: \
			Rf_error(fmt sep "invalid input", __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_NOMEM: \
			Rf_error(fmt sep "memory allocation failure", \
				 __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_OS: \
			Rf_error(fmt sep "operating system error", \
				 __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_OVERFLOW: \
			Rf_error(fmt sep "overflow error", __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_DOMAIN: \
			Rf_error(fmt sep "domain error", __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_RANGE: \
			Rf_error(fmt sep "range error", __VA_ARGS__); \
			break; \
		case CORPUS_ERROR_INTERNAL: \
			Rf_error(fmt sep "internal error", __VA_ARGS__); \
			break; \
		default: \
			Rf_error(fmt sep "unknown error", __VA_ARGS__); \
			break; \
		} \
	} while (0)

#define CHECK_ERROR_FORMAT(err, fmt, ...) \
	CHECK_ERROR_FORMAT_SEP(err, fmt, ": ", __VA_ARGS__)

#define CHECK_ERROR_MESSAGE(err, msg) \
	CHECK_ERROR_FORMAT(err, "%s", msg)

#define CHECK_ERROR(err) \
	CHECK_ERROR_FORMAT_SEP(err, "", "%s", "")


struct corpus_data;
struct corpus_filebuf;
struct corpus_filter;
struct corpus_search;
struct corpus_sentfilter;

struct mkchar {
	uint8_t *buf;
	int size;
};

struct decode {
	struct mkchar mkchar;
	int overflow;
	int underflow;
};

struct json {
	struct corpus_schema schema;
	struct corpus_data *rows;
	R_xlen_t nrow;
	int type_id;
	int kind;
};

enum stemmer_type {
	STEMMER_NONE = 0,
	STEMMER_RFUNC,
	STEMMER_SNOWBALL,
};

struct stemmer_rfunc {
	SEXP fn;
	SEXP rho;
};

struct stemmer {
	union {
		struct stemmer_rfunc rfunc;
		struct corpus_stem_snowball snowball;
	} value;
	int type;
	corpus_stem_func stem_func;
	void *stem_context;
	int error;
};

struct rcorpus_text {
	struct utf8lite_text *text;
	struct corpus_filter filter;
	struct corpus_sentfilter sentfilter;
	struct stemmer stemmer;
	R_xlen_t length;
	int has_filter;
	int valid_filter;
	int has_sentfilter;
	int valid_sentfilter;
	int has_stemmer;
};

struct termset {
	struct corpus_termset set;
	struct utf8lite_text *items;
	int has_set;
	int max_length;
	int nitem;
};

/* context */
SEXP alloc_context(size_t size, void (*destroy_func)(void *));
void free_context(SEXP x);
void *as_context(SEXP x);
int is_context(SEXP x);

/* converting text to CHARSXP */
void mkchar_init(struct mkchar *mk);
SEXP mkchar_get(struct mkchar *mk, const struct utf8lite_text *text);

/* converting data to R values */
void decode_init(struct decode *d);
int decode_set_overflow(struct decode *d, int overflow);
int decode_set_underflow(struct decode *d, int underflow);

int decode_logical(struct decode *d, const struct corpus_data *val);
int decode_integer(struct decode *d, const struct corpus_data *val);
double decode_real(struct decode *d, const struct corpus_data *val);
SEXP decode_charsxp(struct decode *d, const struct corpus_data *val);
SEXP decode_sexp(struct decode *d, const struct corpus_data *val,
		 const struct corpus_schema *s);

/* stemmer */
void stemmer_init_none(struct stemmer *s);
void stemmer_init_snowball(struct stemmer *s, const char *algorithm);
void stemmer_init_rfunc(struct stemmer *s, SEXP fn, SEXP rho);
void stemmer_destroy(struct stemmer *s);
const char *stemmer_snowball_name(const char *alias);

SEXP stem_snowball(SEXP x, SEXP algorithm);


/* logging */
SEXP logging_off(void);
SEXP logging_on(void);

/* json */
SEXP alloc_json(SEXP buffer, SEXP field, SEXP rows, SEXP text);
int is_json(SEXP data);
struct json *as_json(SEXP data);

SEXP as_integer_json(SEXP data);
SEXP as_double_json(SEXP data);
SEXP as_factor_json(SEXP data);
SEXP as_list_json(SEXP data);
SEXP as_logical_json(SEXP data);
SEXP as_character_json(SEXP data);
SEXP as_text_json(SEXP data, SEXP filter);
SEXP dim_json(SEXP data);
SEXP length_json(SEXP data);
SEXP names_json(SEXP data);
SEXP print_json(SEXP data);
SEXP simplify_json(SEXP data);
SEXP subscript_json(SEXP data, SEXP i);
SEXP subset_json(SEXP data, SEXP i, SEXP j);

/* data */
SEXP scalar_data(const struct corpus_data *d, const struct corpus_schema *s,
		 int *overflowptr);

/* file buffer */
SEXP alloc_filebuf(SEXP file);
int is_filebuf(SEXP sbuf);
struct corpus_filebuf *as_filebuf(SEXP sbuf);

/* text (core) */
SEXP alloc_text(SEXP sources, SEXP source, SEXP row, SEXP start, SEXP stop,
		SEXP names, SEXP filter);
int is_text(SEXP text);
struct utf8lite_text *as_text(SEXP text, R_xlen_t *lenptr);
struct corpus_filter *text_filter(SEXP x);
struct corpus_sentfilter *text_sentfilter(SEXP x);
SEXP as_text_character(SEXP text, SEXP filter);

SEXP alloc_text_handle(void);
SEXP coerce_text(SEXP x);
SEXP length_text(SEXP text);
SEXP names_text(SEXP text);
SEXP filter_text(SEXP text);
SEXP as_character_text(SEXP text);
SEXP is_na_text(SEXP text);
SEXP anyNA_text(SEXP text);
SEXP text_c(SEXP args, SEXP names, SEXP filter);
SEXP text_trunc(SEXP x, SEXP chars, SEXP right);
SEXP text_valid(SEXP x);

/* text filter */
SEXP as_text_filter_connector(SEXP value);

/* search */
SEXP alloc_search(SEXP sterms, const char *name, struct corpus_filter *filter);
int is_search(SEXP search);
struct corpus_search *as_search(SEXP search);
SEXP items_search(SEXP search);

/* term set */
SEXP alloc_termset(SEXP sterms, const char *name,
		   struct corpus_filter *filter, int allow_dup);
int is_termset(SEXP termset);
struct termset *as_termset(SEXP termset);
SEXP items_termset(SEXP termset);

/* text processing */
SEXP abbreviations(SEXP kind);
SEXP term_stats(SEXP x, SEXP ngrams, SEXP min_count, SEXP max_count,
		SEXP min_support, SEXP max_support, SEXP output_types);
SEXP term_matrix(SEXP x, SEXP ngrams, SEXP select, SEXP group);
SEXP text_count(SEXP x, SEXP terms);
SEXP text_detect(SEXP x, SEXP terms);
SEXP text_locate(SEXP x, SEXP terms);
SEXP text_match(SEXP x, SEXP terms);
SEXP text_nsentence(SEXP x);
SEXP text_ntoken(SEXP x);
SEXP text_ntype(SEXP x, SEXP collapse);
SEXP text_split_sentences(SEXP x, SEXP size);
SEXP text_split_tokens(SEXP x, SEXP size);
SEXP text_sub(SEXP x, SEXP start, SEXP end);
SEXP text_tokens(SEXP x);
SEXP text_types(SEXP x, SEXP collapse);
SEXP stopwords(SEXP kind);

/* json values */
SEXP mmap_ndjson(SEXP file, SEXP text);
SEXP read_ndjson(SEXP buffer, SEXP text);

/* internal utility functions */
double *as_weights(SEXP sweights, R_xlen_t n);
int encodes_utf8(cetype_t ce);
int findListElement(SEXP list, const char *str);
SEXP getListElement(SEXP list, const char *str);

#endif /* RCORPUS_H */
