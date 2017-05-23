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

#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
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


SEXP term_matrix_text(SEXP sx, SEXP sprops, SEXP sweights, SEXP sgroup)
{
	SEXP ans, snames, si, sj, scount, stext, sfilter, scol_names,
	     srow_names;
	const struct corpus_text *text, *type;
	struct mkchar mkchar;
	struct corpus_filter *filter;
	const double *weights;
	const int *group;
	double w;
	struct corpus_census *census;
	R_xlen_t i, n, g, nc, ngroup, nz, off;
	int err, term_id, type_id, nprot = 0;

	PROTECT(stext = coerce_text(sx)); nprot++;
	text = as_text(stext, &n);

	PROTECT(sfilter = alloc_filter(sprops)); nprot++;
	filter = as_filter(sfilter);

	if (sweights != R_NilValue) {
		weights = REAL(sweights);
	} else {
		weights = NULL;
	}

	if (sgroup != R_NilValue) {
		srow_names = getAttrib(sgroup, R_LevelsSymbol);
		ngroup = XLENGTH(srow_names);
		group = INTEGER(sgroup);
	} else {
		srow_names = names_text(sx);
		ngroup = n;
		group = NULL;
	}

	census = malloc(ngroup * sizeof(*census));
	if (ngroup && !census) {
		Rf_error("failed allocating %d objects of size %d byres",
			 ngroup, sizeof(*census));
	}

	for (nc = 0; nc < ngroup; nc++) {
		if ((err = corpus_census_init(&census[nc]))) {
			goto error;
		}
	}

	for (i = 0; i < n; i++) {
		if (!group) {
			g = i;
		} else if (group[i] == NA_INTEGER) {
			continue;
		} else {
			assert(0 < group[i] && group[i] < ngroup);
			g = (R_xlen_t)(group[i] - 1);
		}

		w = weights ? weights[i] : 1;

		if ((err = corpus_filter_start(filter, &text[i]))) {
			goto error;
		}

		while (corpus_filter_advance(filter, &term_id)) {
			if (term_id < 0) {
				continue;
			}

			if ((err = corpus_census_add(&census[g],
						     term_id, w))) {
				goto error;
			}
		}

		if ((err = filter->error)) {
			goto error;
		}
	}

	nz = 0;
	for (g = 0; g < ngroup; g++) {
		if (census[g].nitem > R_XLEN_T_MAX - nz) {
			while (nc-- > 0) {
				corpus_census_destroy(&census[nc]);
			}
			free(census);

			Rf_error("overflow error; number of matrix elements"
				 " exceeds maximum (%"PRIu64")",
				 (uint64_t)R_XLEN_T_MAX);
		}

		nz += census[g].nitem;
	}

	PROTECT(si = allocVector(REALSXP, nz)); nprot++;
	PROTECT(sj = allocVector(INTSXP, nz)); nprot++;
	PROTECT(scount = allocVector(REALSXP, nz)); nprot++;

	off = 0;
	for (g = 0; g < ngroup; g++) {
		for (i = 0; i < census[g].nitem; i++) {
			REAL(si)[off] = (double)g;
			INTEGER(sj)[off] = census[g].items[i];
			REAL(scount)[off] = census[g].weights[i];
			off++;
		}
	}

	PROTECT(scol_names = allocVector(STRSXP, filter->nterm)); nprot++;
	mkchar_init(&mkchar);
	for (i = 0; i < filter->nterm; i++) {
		type_id = filter->type_ids[i];
		type = &filter->symtab.types[type_id].text;
		SET_STRING_ELT(scol_names, i, mkchar_get(&mkchar, type));
	}
	mkchar_destroy(&mkchar);

	PROTECT(ans = allocVector(VECSXP, 5)); nprot++;
	SET_VECTOR_ELT(ans, 0, si);
	SET_VECTOR_ELT(ans, 1, sj);
	SET_VECTOR_ELT(ans, 2, scount);
	SET_VECTOR_ELT(ans, 3, srow_names);
	SET_VECTOR_ELT(ans, 4, scol_names);

	PROTECT(snames = allocVector(STRSXP, 5)); nprot++;
	SET_STRING_ELT(snames, 0, mkChar("i"));
	SET_STRING_ELT(snames, 1, mkChar("j"));
	SET_STRING_ELT(snames, 2, mkChar("count"));
	SET_STRING_ELT(snames, 3, mkChar("row_names"));
	SET_STRING_ELT(snames, 4, mkChar("col_names"));
	setAttrib(ans, R_NamesSymbol, snames);

	err = 0;
error:
	while (nc-- > 0) {
		corpus_census_destroy(&census[nc]);
	}
	free(census);

	if (err) {
		Rf_error("failed computing term counts");
		ans = R_NilValue;
	}
	UNPROTECT(nprot);
	return ans;
}
