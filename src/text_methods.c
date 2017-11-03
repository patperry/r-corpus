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
#include <float.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include "rcorpus.h"


SEXP names_text(SEXP text)
{
	if (!is_text(text)) {
		error("invalid text object");
	}
	return getListElement(text, "names");
}


SEXP filter_text(SEXP text)
{
	if (!is_text(text)) {
		error("invalid text object");
	}
	return getListElement(text, "filter");
}


SEXP length_text(SEXP stext)
{
	R_xlen_t len;
	as_text(stext, &len);
	return ScalarReal((double)len);
}


SEXP is_na_text(SEXP stext)
{
	SEXP ans;
	struct utf8lite_text *text;
	R_xlen_t i, n;
	int *isna;

	text = as_text(stext, &n);
	PROTECT(ans = allocVector(LGLSXP, n));
	isna = LOGICAL(ans);

	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (text[i].ptr) {
			isna[i] = FALSE;
		} else {
			isna[i] = TRUE;
		}
	}

	UNPROTECT(1);
	return ans;
}


SEXP anyNA_text(SEXP stext)
{
	struct utf8lite_text *text;
	R_xlen_t i, n;
	int anyNA;

	text = as_text(stext, &n);

	anyNA = FALSE;
	for (i = 0; i < n; i++) {
		RCORPUS_CHECK_INTERRUPT(i);

		if (!text[i].ptr) {
			anyNA = TRUE;
			break;
		}
	}

	return ScalarLogical(anyNA);
}
