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
#include "corpus/src/table.h"
#include "corpus/src/census.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
#include "corpus/src/tree.h"
#include "corpus/src/typemap.h"
#include "corpus/src/symtab.h"
#include "corpus/src/wordscan.h"
#include "corpus/src/filter.h"
#include "rcorpus.h"


// the R 'error' is a #define (to Rf_error) that clashes with the 'error'
// member of struct corpus_filter
#ifdef error
#  undef error
#endif


SEXP term_counts_text(SEXP sx, SEXP sprops, SEXP sweights)
{
	SEXP ans, sterm, scount, stext, sfilter, sclass, snames, srow_names;
	const struct corpus_text *text, *type;
	struct mkchar mkchar;
	struct corpus_filter *filter;
	const double *weights;
	double wt;
	struct corpus_census census;
	R_xlen_t i, n;
	int err, term_id, type_id, nprot = 0;

	Rprintf("Coericing text...\n");
	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);

	Rprintf("Building filter...\n");
	PROTECT(sfilter = alloc_filter(sprops)); nprot++;
	filter = as_filter(sfilter);

	Rprintf("Coercing weights...\n");
	if (sweights != R_NilValue) {
		PROTECT(sweights = coerceVector(sweights, REALSXP)); nprot++;
		if (XLENGTH(sweights) != n) {
			Rf_error("invalid 'weights' vector;"
				 " should have length = %"PRIu64,
				 (uint64_t)n);
		}
		weights = REAL(sweights);

		for (i = 0; i < n; i++) {
			if (ISNA(weights[i])) {
				Rf_error("weights[%"PRIu64"] is NA",
					 (uint64_t)(i+1));
			} else if (!R_FINITE(weights[i])) {
				Rf_error("weights[%"PRIu64"] is Inf or NaN",
					 (uint64_t)(i+1));
			}
		}
	} else {
		weights = NULL;
	}

	if ((err = corpus_census_init(&census))) {
		goto error_census;
	}

	Rprintf("Tokenizing...\n");
	
	for (i = 0; i < n; i++) {
		wt = weights ? weights[i] : 1;

		Rprintf("Starting filter of text %d with weight %g...\n",
			(int)(i + 1), wt);
		
		if ((err = corpus_filter_start(filter, &text[i]))) {
			goto error;
		}


		while (corpus_filter_advance(filter, &term_id)) {
			Rprintf("Got token %d.\n", term_id);
			
			if (term_id < 0) {
				Rprintf("Skipping token...\n");
				continue;
			}

			Rprintf("Adding to census...\n");

			if ((err = corpus_census_add(&census, term_id, wt))) {
				goto error;
			}

			Rprintf("Advancing to next token...\n");
		}

		Rprintf("Done with text %d.\n", (int)(i + 1));

		if (filter->error) {
			goto error;
		}
	}

	Rprintf("Sorting...\n");

	if ((err = corpus_census_sort(&census))) {
		goto error;
	}

	Rprintf("Summarizing results...\n");

	PROTECT(sterm = allocVector(STRSXP, census.nitem)); nprot++;
	PROTECT(scount = allocVector(REALSXP, census.nitem)); nprot++;

	mkchar_init(&mkchar);
	
	for (i = 0; i < census.nitem; i++) {
		term_id = census.items[i];
		type_id = filter->type_ids[term_id];
		type = &filter->symtab.types[type_id].text;

		SET_STRING_ELT(sterm, i, mkchar_get(&mkchar, type));
		REAL(scount)[i] = census.weights[i];
	}

	mkchar_destroy(&mkchar);

	PROTECT(ans = allocVector(VECSXP, 2)); nprot++;
	SET_VECTOR_ELT(ans, 0, sterm);
	SET_VECTOR_ELT(ans, 1, scount);

	PROTECT(snames = allocVector(STRSXP, 2)); nprot++;
	SET_STRING_ELT(snames, 0, mkChar("term"));
	SET_STRING_ELT(snames, 1, mkChar("count"));
	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(srow_names = allocVector(REALSXP, 2)); nprot++;
	REAL(srow_names)[0] = NA_REAL;
	REAL(srow_names)[1] = -(double)census.nitem;
	setAttrib(ans, R_RowNamesSymbol, srow_names);

	PROTECT(sclass = allocVector(STRSXP, 1)); nprot++;
	SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
	setAttrib(ans, R_ClassSymbol, sclass);

	Rprintf("Cleaning up...\n");

	err = 0;
error:
	corpus_census_destroy(&census);
error_census:
	if (err) {
		Rf_error("failed computing term counts");
	}
	UNPROTECT(nprot);
	Rprintf("Done.\n");
	return ans;
}
