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
#include "rcorpus.h"


SEXP mmap_ndjson(SEXP sfile, SEXP stext)
{
	SEXP ans, sbuf;

	PROTECT(sbuf = alloc_filebuf(sfile));
	PROTECT(ans = alloc_json(sbuf, R_NilValue, R_NilValue, stext));
	as_json(ans); // force data load
	UNPROTECT(2);

	return ans;
}


SEXP read_ndjson(SEXP sbuffer, SEXP stext)
{
	SEXP ans;

	assert(TYPEOF(sbuffer) == RAWSXP);

	PROTECT(ans = alloc_json(sbuffer, R_NilValue, R_NilValue, stext));
	as_json(ans); // force data load
	UNPROTECT(1);

	return ans;
}
