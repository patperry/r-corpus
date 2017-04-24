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

#include <errno.h>
#include <stdlib.h>
#include "corpus/src/filebuf.h"
#include <Rdefines.h>
#include "rcorpus.h"


#define FILEBUF_TAG install("corpus::filebuf")


static struct filebuf *filebuf_new(const char *filename)
{
	struct filebuf *obj = NULL;
	struct filebuf buf;

	errno = 0;

	if (filebuf_init(&buf, filename) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			filebuf_destroy(&buf);
			error("failed allocating memory (%u bytes)",
			      (unsigned)sizeof(*obj));
		}
		*obj = buf;
	} else {
		if (errno) {
			error("cannot open file '%s': %s",
				filename, strerror(errno));
		} else {
			error("cannot open file '%s'", filename);
		}
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
	SEXP ans, sclass, shandle, snames;
	struct filebuf *buf;
	const char *file;

	if (!(isString(sfile) && LENGTH(sfile) == 1)) {
                error("invalid 'file' argument");
        }

	file = R_ExpandFileName(CHAR(STRING_ELT(sfile, 0)));

	PROTECT(shandle = R_MakeExternalPtr(NULL, FILEBUF_TAG, R_NilValue));
	R_RegisterCFinalizerEx(shandle, free_filebuf, TRUE);

	buf = filebuf_new(file);
	R_SetExternalPtrAddr(shandle, buf);

	PROTECT(ans = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(ans, 0, shandle);
	SET_VECTOR_ELT(ans, 1, sfile);

	PROTECT(snames = allocVector(STRSXP, 2));
	SET_STRING_ELT(snames, 0, mkChar("handle"));
	SET_STRING_ELT(snames, 1, mkChar("file"));
	setAttrib(ans, R_NamesSymbol, snames);

	PROTECT(sclass = allocVector(STRSXP, 1));
	SET_STRING_ELT(sclass, 0, mkChar("filebuf"));
	setAttrib(ans, R_ClassSymbol, sclass);

	UNPROTECT(4);
	return ans;
}


int is_filebuf(SEXP sbuf)
{
	SEXP handle, file;

	if (!isVectorList(sbuf)) {
		return 0;
	}

	handle = getListElement(sbuf, "handle");
	if (handle == R_NilValue) {
		return 0;
	}

	file = getListElement(sbuf, "file");
	if (file == R_NilValue) {
		return 0;
	}

	return ((TYPEOF(handle) == EXTPTRSXP)
		&& (R_ExternalPtrTag(handle) == FILEBUF_TAG));
}


struct filebuf *as_filebuf(SEXP sbuf)
{
	SEXP shandle, sfile;
	struct filebuf *buf;
	const char *file;

	if (!is_filebuf(sbuf)) {
		error("invalid 'filebuf' object");
	}

	shandle = getListElement(sbuf, "handle");
	buf = R_ExternalPtrAddr(shandle);

	if (buf == NULL) {
		R_RegisterCFinalizerEx(shandle, free_filebuf, TRUE);

		sfile = getListElement(sbuf, "file");
		file = R_ExpandFileName(CHAR(STRING_ELT(sfile, 0)));
		buf = filebuf_new(file);

		if (buf == NULL) {
			if (errno) {
				error("cannot open file '%s': %s", file,
				      strerror(errno));
			} else {
				error("cannot open file '%s'", file);
			}
		}

		R_SetExternalPtrAddr(shandle, buf);
	}

	return buf;
}
