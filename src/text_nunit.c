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
#include "corpus/src/intset.h"
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


SEXP text_nsentence(SEXP sx, SEXP sfilter)
{
	SEXP ans, names;
	struct corpus_sentfilter *filter;
	const struct corpus_text *text;
	double *count;
	R_xlen_t i, n, nunit;
	int nprot, err = 0;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	names = names_text(sx);

	// filter
	PROTECT(sfilter = alloc_sentfilter(sfilter)); nprot++;
	filter = as_sentfilter(sfilter);

	PROTECT(ans = allocVector(REALSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names);
	count = REAL(ans);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) { // missing value
			count[i] = NA_REAL;
			continue;
		}

		if (CORPUS_TEXT_SIZE(&text[i]) == 0) { // empty text
			count[i] = 0;
			continue;
		}

		TRY(corpus_sentfilter_start(filter, &text[i]));

		nunit = 0;
		while (corpus_sentfilter_advance(filter)) {
			nunit++;
		}
		TRY(filter->error);

		count[i] = (double)nunit;
	}

out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP text_ntoken(SEXP sx, SEXP sfilter)
{
	SEXP ans, names;
	struct corpus_filter *filter;
	const struct corpus_text *text;
	double *count;
	R_xlen_t i, n, nunit;
	int nprot, err = 0;

	nprot = 0;

	// x
	PROTECT(sx = coerce_text(sx)); nprot++;
	text = as_text(sx, &n);
	names = names_text(sx);

	// filter
	PROTECT(sfilter = alloc_filter(sfilter)); nprot++;
	filter = as_filter(sfilter);

	PROTECT(ans = allocVector(REALSXP, n)); nprot++;
	setAttrib(ans, R_NamesSymbol, names);
	count = REAL(ans);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) { // missing text
			count[i] = NA_REAL;
			continue;
		}

		TRY(corpus_filter_start(filter, &text[i],
					CORPUS_FILTER_SCAN_TOKENS));

		nunit = 0;

		while (corpus_filter_advance(filter)) {
			if (filter->type_id < 0) {
				// skip ignored and dropped tokens
				continue;
			}
			nunit++;
		}
		TRY(filter->error);

		count[i] = (double)nunit;
	}

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}
