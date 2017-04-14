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
#include "corpus/src/filebuf.h"
#include <Rdefines.h>
#include "rcorpus.h"


#define FILEBUF_TAG install("corpus::filebuf")


static struct filebuf *filebuf_new(const char *filename)
{
	struct filebuf *obj = NULL;
	struct filebuf buf;

	if (filebuf_init(&buf, filename) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			filebuf_destroy(&buf);
			error("failed allocating memory (%zu bytes)",
			      sizeof(*obj));
		}
		*obj = buf;
	}
	return obj;
}


static void filebuf_free(struct filebuf *buf)
{
	if (buf) {
		filebuf_destroy(buf);
		free(buf);
	}
}


static void free_filebuf(SEXP sbuf)
{
        struct filebuf *buf = R_ExternalPtrAddr(sbuf);
	filebuf_free(buf);
}


SEXP alloc_filebuf(SEXP sfile)
{
	SEXP sbuf;
	struct filebuf *buf;
	const char *file;

	if (!(isString(sfile) && LENGTH(sfile) == 1)) {
                error("invalid 'file' argument");
        }
        file = R_ExpandFileName(CHAR(STRING_ELT(sfile, 0)));

	buf = filebuf_new(file);
	PROTECT(sbuf = R_MakeExternalPtr(buf, FILEBUF_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sbuf, free_filebuf, TRUE);

	UNPROTECT(1);
	return sbuf;
}


int is_filebuf(SEXP sbuf)
{
	return ((TYPEOF(sbuf) == EXTPTRSXP)
		&& (R_ExternalPtrTag(sbuf) == FILEBUF_TAG));
}


struct filebuf *as_filebuf(SEXP sbuf)
{
	if (!is_filebuf(sbuf))
		error("invalid 'filebuf' object");
	return R_ExternalPtrAddr(sbuf);
}
