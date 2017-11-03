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

#include "rcorpus.h"


static int text_len(const struct utf8lite_text *text,
		    struct corpus_filter *filter)
{
	int err = 0, len = 0;

	TRY(corpus_filter_start(filter, text));
	while (corpus_filter_advance(filter)) {
		if (filter->type_id == CORPUS_TYPE_NONE) {
			continue;
		}
		len++;
	}
	TRY(filter->error);
out:
	CHECK_ERROR(err);
	return len;
}


SEXP text_sub(SEXP sx, SEXP sstart, SEXP send)
{
	SEXP ans, sources, table, tsource, trow, tstart, tstop, names, sfilter;
	const struct utf8lite_text *text;
	const uint8_t *base, *ptr;
	struct corpus_filter *filter;
	const int *start, *end;
	R_xlen_t i, n, nstart, nend;
	int err = 0, nprot = 0, s, e, j, m;

	text = as_text(sx, &n);
	filter = text_filter(sx);
	sources = getListElement(sx, "sources");
	table = getListElement(sx, "table");
	tsource = getListElement(table, "source");
	trow = getListElement(table, "row");
	tstart = getListElement(table, "start");
	tstop = getListElement(table, "stop");
	names = names_text(sx);
	sfilter = filter_text(sx);

	PROTECT(tstart = duplicate(tstart)); nprot++;
	PROTECT(tstop = duplicate(tstop)); nprot++;

	start = INTEGER(sstart);
	nstart = XLENGTH(sstart);

	end = INTEGER(send);
	nend = XLENGTH(send);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		s = start[i % nstart];
		e = end[i % nend];

		// handle missing text, missing endpoints
		if (!text[i].ptr || s == NA_INTEGER || e == NA_INTEGER) {
			INTEGER(tstart)[i] = NA_INTEGER;
			INTEGER(tstop)[i] = NA_INTEGER;
			continue;
		}

		// convert negative indices to non-negative,
		// except for end = -1
		if (s < 0 || e < -1) {
			m = text_len(&text[i], filter);

			if (s < 0) {
				s = s + m + 1;
				if (s < 0) {
					s = 0;
				}
			}

			if (e < -1) {
				e = e + m + 1;
				if (e < 0) {
					e = 0;
				}
			}
		}

		// clip start to [1,Inf)
		if (s == 0) {
			s = 1;
		}

		base = text[i].ptr - (INTEGER(tstart)[i] - 1);

		// find start
		j = 0;
		TRY(corpus_filter_start(filter, &text[i]));
		while (j != s && corpus_filter_advance(filter)) {
			if (filter->type_id == CORPUS_TYPE_NONE) {
				// skip ignored
				continue;
			}
			j++;
		}
		TRY(filter->error);

		// handle case when start is after end of text
		if (j < s) {
			INTEGER(tstart)[i] = INTEGER(tstop)[i] + 1;
			continue;
		}

		// set subsequence start
		ptr = filter->current.ptr;
		INTEGER(tstart)[i] = (int)(ptr - base) + 1;

		// handle case when end is the last token
		if (e == -1) {
			continue;
		}

		// find end
		while (j != e + 1 && corpus_filter_advance(filter)) {
			if (filter->type_id == CORPUS_TYPE_NONE) {
				// skip ignored
				continue;
			}
			j++;
		}
		TRY(filter->error);

		// handle case when end is after end of text
		if (j < e + 1) {
			continue;
		}

		// set subsequence end
		ptr = filter->current.ptr;
		INTEGER(tstop)[i] = (int)(ptr - base);
	}

	PROTECT(ans = alloc_text(sources, tsource, trow, tstart, tstop,
				 names, sfilter));
	nprot++;

out:
	UNPROTECT(nprot);
	CHECK_ERROR(err);
	return ans;
}
