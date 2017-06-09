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
#include <stdlib.h>
#include <string.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"

// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_sentfilter
#ifdef error
#  undef error
#endif


#define BAIL(msg) \
	do { \
		Rf_error(msg); \
	} while (0)


SEXP text_count_sentences(SEXP sx, SEXP sfilter, SEXP sweights, SEXP sgroup)
{
	SEXP ans, names;
	struct corpus_sentfilter *filter;
	const struct corpus_text *text;
	const double *weights;
	double *count;
	double w;
	const int *group;
	int *is_na;
	R_xlen_t i, n, nunit, g, ngroup;
	int nprot, err;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	// filter
	PROTECT(sfilter = alloc_sentfilter(sfilter)); nprot++;
	filter = as_sentfilter(sfilter);

	// weights
	weights = as_weights(sweights, n);

	// group
	if (sgroup != R_NilValue) {
		names = getAttrib(sgroup, R_LevelsSymbol);
		ngroup = XLENGTH(names);
		group = INTEGER(sgroup);
	} else {
		names = names_text(sx);
		ngroup = n;
		group = NULL;
	}

	PROTECT(ans = allocVector(REALSXP, ngroup)); nprot++;
	setAttrib(ans, R_NamesSymbol, names);
	count = REAL(ans);
	memset(count, 0, ngroup * sizeof(*count));

	is_na = (void *)R_alloc(ngroup, sizeof(*is_na));
	memset(is_na, 0, ngroup * sizeof(*is_na));

	for (i = 0; i < n; i++) {
		w = weights ? weights[i] : 1;
		if (w == 0) {
			continue;
		}

		g = group ? group[i] : i;
		if (g == NA_INTEGER) {
			continue;
		}

		if (!text[i].ptr) { // missing value
			is_na[g] = 1;
			continue;
		}

		if (CORPUS_TEXT_SIZE(&text[i]) == 0) { // empty text
			continue;
		}

		if ((err = corpus_sentfilter_start(filter, &text[i]))) {
			BAIL("memory allocation failure");
		}

		nunit = 0;
		while (corpus_sentfilter_advance(filter)) {
			nunit++;
		}
		if (filter->error) {
			BAIL("memory allocation failure");
		}

		count[g] += w * (double)nunit;
	}

	for (g = 0; g < ngroup; g++) {
		if (is_na[g]) {
			count[g] = NA_REAL;
		}
	}

	UNPROTECT(nprot);
	return ans;
}


#undef BAIL
#define BAIL(msg) \
	do { \
		Rf_error(msg); \
	} while (0)


SEXP text_count_tokens(SEXP sx, SEXP sfilter, SEXP sweights, SEXP sgroup)
{
	SEXP ans, names;
	struct corpus_filter *filter;
	const struct corpus_text *text;
	const double *weights;
	double w;
	double *count;
	R_xlen_t i, n, nunit, g, ngroup;
	const int *group;
	int *is_na;
	int nprot, err;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);

	// filter
	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

	// weights
	weights = as_weights(sweights, n);

	// group
	if (sgroup != R_NilValue) {
		names = getAttrib(sgroup, R_LevelsSymbol);
		ngroup = XLENGTH(names);
		group = INTEGER(sgroup);
	} else {
		names = names_text(sx);
		ngroup = n;
		group = NULL;
	}

	PROTECT(ans = allocVector(REALSXP, ngroup)); nprot++;
	setAttrib(ans, R_NamesSymbol, names);
	count = REAL(ans);
	memset(count, 0, ngroup * sizeof(*count));

	is_na = (void *)R_alloc(ngroup, sizeof(*is_na));
	memset(is_na, 0, ngroup * sizeof(*is_na));

	for (i = 0; i < n; i++) {
		w = weights ? weights[i] : 1;
		if (w == 0) { // no weight
			continue;
		}

		g = group ? group[i] : i;
		if (g == NA_INTEGER) { // missing group
			continue;
		}

		if (!text[i].ptr) { // missing text
			is_na[g] = 1;
			continue;
		}

		if ((err = corpus_filter_start(filter, &text[i],
					       CORPUS_FILTER_SCAN_TOKENS))) {
			BAIL("memory allocation failure");
		}

		nunit = 0;

		while (corpus_filter_advance(filter)) {
			if (filter->type_id < 0) {
				// skip ignored and dropped tokens
				continue;
			}
			nunit++;
		}

		if (filter->error) {
			BAIL("memory allocation failure");
		}

		count[g] += w * (double)nunit;
	}

	for (g = 0; g < ngroup; g++) {
		if (is_na[g]) {
			count[g] = NA_REAL;
		}
	}

	UNPROTECT(nprot);
	return ans;
}
