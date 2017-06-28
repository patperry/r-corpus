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
#include "corpus/src/unicode.h"
#include "rcorpus.h"


static const char *encoding_name(cetype_t ce)
{
	switch (ce) {
	case CE_LATIN1:
		return "latin1";
	case CE_UTF8:
		return "UTF-8";
	case CE_SYMBOL:
		return "symbol";
	case CE_BYTES:
		return "bytes";
	case CE_ANY:
	case CE_NATIVE:
	default:
		return "unknown";
	}
}


static int is_valid(const uint8_t *str, size_t size, size_t *errptr)
{
	const uint8_t *end = str + size;
	const uint8_t *ptr = str;
	size_t err = (size_t)-1;
	int valid, nbyte;

	while (ptr != end) {
		nbyte = 1 + CORPUS_UTF8_TAIL_LEN(*ptr);
		if (corpus_scan_utf8(&ptr, end)) {
			err = (size_t)(ptr - str);
			valid = 0;
			goto out;
		}
	}
	valid = 1;

out:
	if (!valid) {
		*errptr = err;
	}

	return valid;
}


SEXP utf8_valid(SEXP sx)
{
	char buf[256];
	SEXP ans, sstr;
	cetype_t ce;
	const uint8_t *str;
	size_t size, err;
	R_xlen_t i, n;
	int nprot, raw;
	
	if (sx == R_NilValue) {
		return ScalarLogical(TRUE);
	}

	nprot = 0;
	PROTECT(sx = coerceVector(sx, STRSXP)); nprot++;
	n = XLENGTH(sx);
	for (i = 0; i < n; i++) {
		sstr = STRING_ELT(sx, i);
		if (sstr == NA_STRING) {
			continue;
		}

		ce = getCharCE(sstr);
		raw = ((ce == CE_ANY || ce == CE_UTF8 || ce == CE_BYTES));

		if (raw) {
			str = (const uint8_t *)CHAR(sstr);
			size = (size_t)XLENGTH(sstr);
		} else {
			str = (const uint8_t *)translateCharUTF8(sstr);
			size = strlen((const char *)str);
		}

		if (!is_valid(str, size, &err)) {
			if (ce == CE_UTF8) {
				snprintf(buf, sizeof(buf),
					 "entry %"PRIu64
					 " is marked as \"UTF-8\""
					 " but string byte %"PRIu64
					 " (\"\\x%0x\")"
					 " is invalid in that encoding",
					 (uint64_t)i + 1,
					 (uint64_t)err + 1,
					 (unsigned)str[err]);
			} else {
				snprintf(buf, sizeof(buf),
					 "cannot convert entry %"PRIu64
					 " from \"%s\" encoding to \"UTF-8\";"
					 " %sstring byte %"PRIu64
					 " (\"\\x%0x\") is invalid",
					 (uint64_t)i + 1,
					 encoding_name(ce),
					 ce == CE_BYTES ? ""
					 	        : "after conversion, ",
					 (uint64_t)err + 1,
					 (unsigned)str[err]);
			}
			PROTECT(ans = ScalarString(mkChar(buf))); nprot++;
			goto out;
		}
	}

	ans = ScalarLogical(TRUE);
out:
	UNPROTECT(nprot);
	return ans;
}
