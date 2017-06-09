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

#include <stdlib.h>
#include <Rdefines.h>
#include "corpus/src/table.h"
#include "corpus/src/text.h"
#include "corpus/src/tree.h"
#include "corpus/src/sentscan.h"
#include "corpus/src/sentfilter.h"
#include "rcorpus.h"


#define SENTFILTER_TAG install("corpus::sentfilter")


static struct corpus_sentfilter *sentfilter_new(int flags)
{
	struct corpus_sentfilter *obj = NULL;
	struct corpus_sentfilter f;

	if (corpus_sentfilter_init(&f, flags) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			corpus_sentfilter_destroy(&f);
			error("failed allocating memory (%u bytes)",
			      (unsigned)sizeof(*obj));
		}
		*obj = f;
	} else {
		error("failed creating new filter object");
	}

	return obj;
}


static void sentfilter_free(struct corpus_sentfilter *f)
{
	if (f) {
		corpus_sentfilter_destroy(f);
		free(f);
	}
}


static void free_sentfilter(SEXP sfilter)
{
        struct corpus_sentfilter *f = R_ExternalPtrAddr(sfilter);
	sentfilter_free(f);
}


static int sentence_filter_logical(SEXP filter, const char *key, int nullval)
{
	SEXP val = getListElement(filter, key);
	int ans;

	if (val == R_NilValue) {
		return nullval;
	}

	PROTECT(val = coerceVector(val, LGLSXP));

	if (XLENGTH(val) > 1) {
		error("invalid sentence filter '%s' value", key);
	}

	if (XLENGTH(val) == 0 || INTEGER(val)[0] == NA_LOGICAL) {
		ans = nullval;
	} else {
		ans = INTEGER(val)[0] ? 1 : 0;
	}
	UNPROTECT(1);

	return ans;
}


static int sentence_filter_flags(SEXP filter)
{
	int flags = CORPUS_SENTSCAN_SPCRLF;

	if (filter == R_NilValue) {
		return flags;
	}

	if (sentence_filter_logical(filter, "crlf_break", 0)) {
		flags &= ~CORPUS_SENTSCAN_SPCRLF;
	}

	return flags;
}


static void suppress_terms(struct corpus_sentfilter *f, SEXP ssuppress)
{
	const struct corpus_text *suppress;
	R_xlen_t i, n;
	int err;

	// suppress
	if (ssuppress == R_NilValue) {
		return;
	}

	PROTECT(ssuppress = coerce_text(ssuppress));
	suppress = as_text(ssuppress, &n);

	for (i = 0; i < n; i++) {
		if (!suppress[i].ptr) {
			continue;
		}

		if ((err = corpus_sentfilter_suppress(f, &suppress[i]))) {
			error("failed adding break suppression"
			      " to sentence filter");
		}
	}

	UNPROTECT(1);
}


SEXP alloc_sentfilter(SEXP props)
{
	SEXP sfilter;
	struct corpus_sentfilter *f;
	int flags;

	flags = sentence_filter_flags(props);

	f = sentfilter_new(flags);
	PROTECT(sfilter = R_MakeExternalPtr(f, SENTFILTER_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sfilter, free_sentfilter, TRUE);

	suppress_terms(f, getListElement(props, "suppress"));

	UNPROTECT(1);
	return sfilter;
}


int is_sentfilter(SEXP sfilter)
{
	return ((TYPEOF(sfilter) == EXTPTRSXP)
		&& (R_ExternalPtrTag(sfilter) == SENTFILTER_TAG));
}


struct corpus_sentfilter *as_sentfilter(SEXP sfilter)
{
	if (!is_sentfilter(sfilter)) {
		error("invalid 'sentfilter' object");
	}
	return R_ExternalPtrAddr(sfilter);
}
