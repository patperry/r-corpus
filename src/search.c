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
#include "rcorpus.h"

#define SEARCH_TAG install("corpus::search")


static struct corpus_search *search_new(void);

struct corpus_search *search_new(void)
{
	struct corpus_search *obj;
	int err;

	TRY_ALLOC(obj = corpus_calloc(1, sizeof(*obj)));
	TRY(corpus_search_init(obj));

	err = 0;
out:
	if (err) {
		corpus_free(obj);
		Rf_error("memory allocation failure");
	}

	return obj;
}


void corpus_search_free(struct corpus_search *obj)
{
	if (!obj) {
		return;
	}

	corpus_search_destroy(obj);
	corpus_free(obj);
}


static void free_search(SEXP obj)
{
        struct corpus_search *search = R_ExternalPtrAddr(obj);
	corpus_search_free(search);
	R_ClearExternalPtr(obj);
}


int is_search(SEXP ssearch)
{
	return ((TYPEOF(ssearch) == EXTPTRSXP)
		&& (R_ExternalPtrTag(ssearch) == SEARCH_TAG));
}


struct corpus_search *as_search(SEXP ssearch)
{
	if (!is_search(ssearch)) {
		Rf_error("invalid 'search' object");
	}
	return R_ExternalPtrAddr(ssearch);
}


SEXP alloc_search(SEXP sterms, const char *name, struct corpus_filter *filter)
{
	SEXP ans, sset, items;
	const struct corpus_termset_term *term;
	struct corpus_search *obj;
	struct termset *termset;
	int i, n;
	int err = 0, nprot;

	nprot = 0;

	obj = search_new();
	PROTECT(ans = R_MakeExternalPtr(obj, SEARCH_TAG, R_NilValue)); nprot++;
	R_RegisterCFinalizerEx(ans, free_search, TRUE);

	PROTECT(sset = alloc_termset(sterms, name, filter, 1)); nprot++;
	termset = as_termset(sset);
	items = items_termset(sset);
	R_SetExternalPtrProtected(ans, items);

	n = termset->nitem;
	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);
		term = &termset->set.items[i];
		TRY(corpus_search_add(obj, term->type_ids,
				      term->length, NULL));
	}

out:
	CHECK_ERROR(err);
	UNPROTECT(nprot);
	return ans;
}


SEXP items_search(SEXP ssearch)
{
	return R_ExternalPtrProtected(ssearch);
}
