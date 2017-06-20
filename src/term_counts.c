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
#include <string.h>
#include "corpus/src/error.h"
#include "corpus/src/memory.h"
#include "corpus/src/render.h"
#include "corpus/src/table.h"
#include "corpus/src/tree.h"
#include "corpus/src/termset.h"
#include "corpus/src/ngram.h"
#include "corpus/src/text.h"
#include "corpus/src/textset.h"
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


SEXP term_counts_text(SEXP sx, SEXP sprops, SEXP sweights, SEXP sngrams,
		      SEXP smin_count, SEXP smax_count, SEXP soutput_types)
{
	SEXP ans, stype, sterm, scount, stext, sfilter, sclass, snames,
	     srow_names;
	SEXP *stypes;
	const struct corpus_text *text, *type;
	struct mkchar mkchar;
	struct corpus_render render;
	struct corpus_ngram ngram;
	struct corpus_ngram_iter it;
	struct corpus_filter *filter;
	const double *weights;
	const int *ngrams;
	int *buffer, *ngram_set;
	int ng_max, w;
	double wt, min_count, max_count;
	R_xlen_t i, n, k, ngrams_len, nterm;
	int output_types;
	int err, type_id, nprot = 0;

	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);

	PROTECT(sfilter = alloc_filter(sprops)); nprot++;
	filter = as_filter(sfilter);

	if (sngrams != R_NilValue) {
		PROTECT(sngrams = coerceVector(sngrams, INTSXP)); nprot++;
		ngrams = INTEGER(sngrams);
		ngrams_len = XLENGTH(sngrams);
		ng_max = 1;
		for (k = 0; k < ngrams_len; k++) {
			if (ngrams[k] == NA_INTEGER) {
				continue;
			}
			if (ngrams[k] > ng_max) {
				ng_max = ngrams[k];
			}
		}

		ngram_set = (void *)R_alloc(ng_max + 1, sizeof(*ngram_set));
		memset(ngram_set, 0, (ng_max + 1) * sizeof(*ngram_set));
		for (k = 0; k < ngrams_len; k++) {
			if (ngrams[k] == NA_INTEGER) {
				continue;
			}
			ngram_set[ngrams[k]] = 1;
		}
	} else {
		ng_max = 1;
		ngram_set = (void *)R_alloc(2, sizeof(*ngram_set));
		ngram_set[0] = 0;
		ngram_set[1] = 1;
	}
	buffer = (void *)R_alloc(ng_max, sizeof(*buffer));

	weights = as_weights(sweights, n);
	min_count = REAL(smin_count)[0];
	max_count = REAL(smax_count)[0];

	if (LOGICAL(soutput_types)[0] == TRUE) {
		output_types = 1;
	} else {
		output_types = 0;
	}

	if ((err = corpus_render_init(&render, CORPUS_ESCAPE_NONE))) {
		goto error_render;
	}

	if ((err = corpus_ngram_init(&ngram, ng_max))) {
		goto error_ngram;
	}

	for (i = 0; i < n; i++) {
		wt = weights ? weights[i] : 1;

		if ((err = corpus_filter_start(filter, &text[i],
					       CORPUS_FILTER_SCAN_TOKENS))) {
			goto error;
		}

		if ((err = corpus_ngram_break(&ngram))) {
			goto error;
		}

		while (corpus_filter_advance(filter)) {
			type_id = filter->type_id;
			if (type_id == CORPUS_FILTER_IGNORED) {
				continue;
			} else if (type_id < 0) {
				if ((err = corpus_ngram_break(&ngram))) {
					goto error;
				}
				continue;
			}

			if ((err = corpus_ngram_add(&ngram, type_id, wt))) {
				goto error;
			}
		}

		if ((err = filter->error)) {
			goto error;
		}
	}

	nterm = 0;
	corpus_ngram_iter_make(&it, &ngram, buffer);
	while (corpus_ngram_iter_advance(&it)) {
		if (!ngram_set[it.length]) {
			continue;
		}
		if (!(min_count <= it.weight && it.weight <= max_count)) {
			continue;
		}

		if (nterm == R_XLEN_T_MAX) {
			err = CORPUS_ERROR_OVERFLOW;
			corpus_log(err, "number of terms exceeds maximum"
				   " (%"PRIu64")",
				   (uint64_t)R_XLEN_T_MAX);
		}
		nterm++;
	}

	PROTECT(sterm = allocVector(STRSXP, nterm)); nprot++;
	if (output_types) {
		stypes = (void *)R_alloc(ng_max, sizeof(*stypes));
		for (w = 0; w < ng_max; w++) {
			PROTECT(stypes[w] = allocVector(STRSXP, nterm));
			nprot++;
			for (i = 0; i < nterm; i++) {
				SET_STRING_ELT(stypes[w], i, NA_STRING);
			}
		}
	} else {
		stypes = NULL;
	}
	PROTECT(scount = allocVector(REALSXP, nterm)); nprot++;

	mkchar_init(&mkchar);
	i = 0;
	
	corpus_ngram_iter_make(&it, &ngram, buffer);
	while (corpus_ngram_iter_advance(&it)) {
		if (!ngram_set[it.length]) {
			continue;
		}
		if (!(min_count <= it.weight && it.weight <= max_count)) {
			continue;
		}

		if (it.length == 1) {
			type_id = it.type_ids[0];
			type = corpus_filter_type(filter, type_id);
			stype = mkchar_get(&mkchar, type);
			SET_STRING_ELT(sterm, i, stype);
			if (output_types) {
				SET_STRING_ELT(stypes[0], i, stype);
			}
		} else {
			corpus_render_clear(&render);

			for (w = 0; w < it.length; w++) {
				type_id = it.type_ids[w];
				type = corpus_filter_type(filter, type_id);
				if (output_types) {
					stype = mkchar_get(&mkchar, type);
					SET_STRING_ELT(stypes[w], i, stype);
				}

				if (w > 0) {
					corpus_render_char(&render, ' ');
				}
				corpus_render_text(&render, type);
			}

			if ((err = render.error)) {
				goto error;
			}

			SET_STRING_ELT(sterm, i,
				       mkCharLenCE(render.string,
					           render.length, CE_UTF8));
		}

		REAL(scount)[i] = it.weight;
		i++;
	}

	if (output_types) {
		PROTECT(ans = allocVector(VECSXP, 2 + ng_max)); nprot++;
		SET_VECTOR_ELT(ans, 0, sterm);
		for (w = 0; w < ng_max; w++) {
			SET_VECTOR_ELT(ans, w + 1, stypes[w]);
		}
		SET_VECTOR_ELT(ans, ng_max + 1, scount);

		PROTECT(snames = allocVector(STRSXP, 2 + ng_max)); nprot++;
		SET_STRING_ELT(snames, 0, mkChar("term"));
		for (w = 0; w < ng_max; w++) {
			corpus_render_clear(&render);
			corpus_render_printf(&render, "type%d", w + 1);
			if ((err = render.error)) {
				goto error;
			}
			SET_STRING_ELT(snames, w + 1, mkChar(render.string));
		}
		SET_STRING_ELT(snames, ng_max + 1, mkChar("count"));
	} else {
		PROTECT(ans = allocVector(VECSXP, 2)); nprot++;
		SET_VECTOR_ELT(ans, 0, sterm);
		SET_VECTOR_ELT(ans, 1, scount);

		PROTECT(snames = allocVector(STRSXP, 2)); nprot++;
		SET_STRING_ELT(snames, 0, mkChar("term"));
		SET_STRING_ELT(snames, 1, mkChar("count"));
	}
	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(srow_names = allocVector(REALSXP, 2)); nprot++;
	REAL(srow_names)[0] = NA_REAL;
	REAL(srow_names)[1] = -(double)nterm;
	setAttrib(ans, R_RowNamesSymbol, srow_names);

	PROTECT(sclass = allocVector(STRSXP, 1)); nprot++;
	SET_STRING_ELT(sclass, 0, mkChar("data.frame"));
	setAttrib(ans, R_ClassSymbol, sclass);

	err = 0;
error:
	corpus_ngram_destroy(&ngram);
error_ngram:
	corpus_render_destroy(&render);
error_render:
	if (err) {
		Rf_error("failed computing term counts");
		ans = R_NilValue;
	}
	UNPROTECT(nprot);
	return ans;
}
