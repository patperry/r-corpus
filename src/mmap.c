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

#define _FILE_OFFSET_BITS 64    // enable large file support

#include <errno.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <Rdefines.h>
#include "rcorpus.h"


#define MMAP_TAG install("corpus::mmap")


static int mmap_init(struct mmap *buf, const char *filename)
{
	struct stat stat;
	void *addr;
	size_t size;
	int fd;

	if ((fd = open(filename, O_RDONLY)) < 0) {
                error("failed opening file (%s): %s", filename,
		      strerror(errno));
		return errno;
        }

	if (fstat(fd, &stat) < 0) {
		close(fd);
                error("failed determining size of file (%s): %s", filename,
		      strerror(errno));
		return errno;
        }

	if (stat.st_size > SIZE_MAX) {
		close(fd);
		error("file size (%"PRIu64") is too large to memory map",
			(uint64_t)stat.st_size);
		return errno;
	}
	size = (size_t)stat.st_size;

	addr = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);
	if (addr == MAP_FAILED) {
		close(fd);
		error("failed memory-mapping file (%s): %s", filename,
		      strerror(errno));
		return errno;
	}

	buf->fd = fd;
	buf->addr = addr;
	buf->size = size;

	return 0;
}


static void mmap_destroy(struct mmap *buf)
{
	munmap(buf->addr, buf->size);
	close(buf->fd);
}


static struct mmap *mmap_new(const char *filename)
{
	struct mmap *obj = NULL;
	struct mmap buf;

	if (mmap_init(&buf, filename) == 0) {
		if (!(obj = malloc(sizeof(*obj)))) {
			mmap_destroy(&buf);
			error("failed allocating memory (%zu bytes)",
			      sizeof(*obj));
		}
		*obj = buf;
	}
	return obj;
}


static void mmap_free(struct mmap *buf)
{
	if (buf) {
		mmap_destroy(buf);
		free(buf);
	}
}


static void free_mmap(SEXP sbuf)
{
        struct mmap *buf = R_ExternalPtrAddr(sbuf);
	mmap_free(buf);
}


SEXP alloc_mmap(SEXP sfile)
{
	SEXP sbuf;
	struct mmap *buf;
	const char *file;

	if (!(isString(sfile) && LENGTH(sfile) == 1)) {
                error("invalid 'file' argument");
        }
        file = R_ExpandFileName(CHAR(STRING_ELT(sfile, 0)));

	buf = mmap_new(file);
	PROTECT(sbuf = R_MakeExternalPtr(buf, MMAP_TAG, R_NilValue));
	R_RegisterCFinalizerEx(sbuf, free_mmap, TRUE);

	UNPROTECT(1);
	return sbuf;
}


int is_mmap(SEXP sbuf)
{
	return ((TYPEOF(sbuf) == EXTPTRSXP)
		&& (R_ExternalPtrTag(sbuf) == MMAP_TAG));
}


struct mmap *as_mmap(SEXP sbuf)
{
	if (!is_mmap(sbuf))
		error("invalid 'mmap' object");
	return R_ExternalPtrAddr(sbuf);
}
