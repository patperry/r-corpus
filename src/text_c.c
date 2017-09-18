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
#include "corpus/src/array.h"
#include "rcorpus.h"


struct context {
	SEXP *sources;
	int nsource;
	int nsource_max;
	int *map;
	int nmap;
	int nmap_max;
};


static void context_init(struct context *ctx)
{
	ctx->sources = NULL;
	ctx->nsource = 0;
	ctx->nsource_max = 0;
	ctx->map = NULL;
	ctx->nmap_max = 0;
}


static int context_add(struct context *ctx, SEXP source)
{
	SEXP *sources = ctx->sources;
	int i, n = ctx->nsource, nmax = ctx->nsource_max;
	int err = 0;

	for (i = 0; i < n; i++) {
		if (sources[i] == source) {
			goto out;
		}
	}

	if (n == nmax) {
		TRY(corpus_array_size_add(&nmax, sizeof(*sources), n, 1));
		if (n > 0) {
			ctx->sources = (void *)S_realloc((char *)sources,
							 nmax, n,
							 sizeof(*sources));
		} else {
			ctx->sources = (void *)R_alloc(nmax,
						       sizeof(*sources));
		}
		ctx->nsource_max = nmax;
	}
	ctx->sources[n] = source;
	ctx->nsource++;

out:
	CHECK_ERROR(err);
	return i;
}


static void context_set(struct context *ctx, SEXP sources)
{
	SEXP src;
	int *map = ctx->map;
	int i, n, nmap_max = ctx->nmap_max;
	int err = 0;

	// allocate space for the map
	n = (sources == R_NilValue) ? 0 : LENGTH(sources);
	if (nmap_max <= n) {
		TRY(corpus_array_size_add(&nmap_max, sizeof(*map), 0, n + 1));
		map = (void *)R_alloc(nmap_max, sizeof(*map));
		ctx->map = map;
		ctx->nmap_max = nmap_max;
	}

	// map from old -> new source id
	map[0] = 0;
	for (i = 0; i < n; i++) {
		src = VECTOR_ELT(sources, i);
		map[i + 1] = context_add(ctx, src) + 1;
	}

out:
	CHECK_ERROR(err);
}


SEXP text_c(SEXP args, SEXP names, SEXP filter)
{
	SEXP ans, elt, elt_sources, elt_table, elt_source, elt_row,
	     elt_start, elt_stop, ssources, ssource, srow, sstart, sstop;
	struct context ctx;
	double *row;
	const int *src;
	int *source, *start, *stop;
	R_xlen_t iarg, narg, i, n, off, len;
	int nprot = 0, j;

	context_init(&ctx);

	narg = (args == R_NilValue) ? 0 : XLENGTH(args);

	len = 0;
	n = 0;
	for (iarg = 0; iarg < narg; iarg++) {
		RCORPUS_CHECK_INTERRUPT(iarg);

		elt = VECTOR_ELT(args, iarg);
		n = (R_xlen_t)(REAL(length_text(elt))[0]);
		if (len > R_XLEN_T_MAX - n) {
			error("text length exceeds maximum (%"PRIu64
			      " elements)", (uint64_t)R_XLEN_T_MAX);
		}
		len += n;
	}

	PROTECT(ssource = allocVector(INTSXP, len)); nprot++;
	source = INTEGER(ssource);
	PROTECT(srow = allocVector(REALSXP, len)); nprot++;
	row = REAL(srow);
	PROTECT(sstart = allocVector(INTSXP, len)); nprot++;
	start = INTEGER(sstart);
	PROTECT(sstop = allocVector(INTSXP, len)); nprot++;
	stop = INTEGER(sstop);

	off = 0;
	for (iarg = 0; iarg < narg; iarg++) {
		RCORPUS_CHECK_INTERRUPT(iarg);

		elt = VECTOR_ELT(args, iarg);
		n = (R_xlen_t)(REAL(length_text(elt))[0]);

		elt_sources = getListElement(elt, "sources");
		context_set(&ctx, elt_sources);

		elt_table = getListElement(elt, "table");
		elt_source = getListElement(elt_table, "source");
		elt_row = getListElement(elt_table, "row");
		elt_start = getListElement(elt_table, "start");
		elt_stop = getListElement(elt_table, "stop");

		src = INTEGER(elt_source);
		for (i = 0; i < n; i++) {
			RCORPUS_CHECK_INTERRUPT(i);
			source[off + i] = ctx.map[src[i]];
		}

		memcpy(row + off, REAL(elt_row), n * sizeof(*row));
		memcpy(start + off, INTEGER(elt_start), n * sizeof(*start));
		memcpy(stop + off, INTEGER(elt_stop), n * sizeof(*stop));

		off += n;
	}

	PROTECT(ssources = allocVector(VECSXP, ctx.nsource)); nprot++;
	for (j = 0; j < ctx.nsource; j++) {
		SET_VECTOR_ELT(ssources, j, ctx.sources[j]);
	}

	PROTECT(ans = alloc_text(ssources, ssource, srow, sstart, sstop,
				 names, filter)); nprot++;

	UNPROTECT(nprot);
	return ans;
}
