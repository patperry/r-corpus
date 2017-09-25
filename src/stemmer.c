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
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "rcorpus.h"

struct snowball_stemmer {
	const char *alias;
	const char *name;
};

static const struct snowball_stemmer snowball_stemmers[]  = {
	{ "ar", "arabic" },
	{ "da", "danish" },
	{ "de", "german" },
	{ "en", "english" },
	{ "es", "spanish" },
	{ "fi", "finnish" },
	{ "fr", "french" },
	{ "hu", "hungarian" },
	{ "it",  "italian" },
	{ "nl", "dutch" },
	{ "no", "norwegian" },
	{ "porter", "porter" },
	{ "pt", "portuguese" },
	{ "ro", "romanian" },
	{ "ru", "russian" },
	{ "sv", "swedish" },
	{ "ta", "tamil" },
	{ "tr", "turkish" },
	{ NULL, NULL } };


const char *stemmer_snowball_name(const char *alias)
{
	const struct snowball_stemmer *stem = snowball_stemmers;

	while (stem->alias) {
		if (strcmp(alias, stem->alias) == 0) {
			return stem->name;
		}
		if (strcmp(alias, stem->name) == 0) {
			return stem->name;
		}
		stem++;
	}

	return NULL;
}


void stemmer_init_none(struct stemmer *s)
{
	s->type = STEMMER_NONE;
	s->stem_func = NULL;
	s->stem_context = NULL;
	s->error = 0;
}


void stemmer_init_snowball(struct stemmer *s, const char *algorithm)
{
	const char *name = stemmer_snowball_name(algorithm);
	int err;

	if (!name) {
		s->error = CORPUS_ERROR_INVAL;
		error("unrecognized stemmer: '%s'", algorithm);
	}

	TRY(corpus_stem_snowball_init(&s->value.snowball, name));

	s->type = STEMMER_SNOWBALL;
	s->stem_func = corpus_stem_snowball;
	s->stem_context = &s->value.snowball;
out:
	s->error = err;
	CHECK_ERROR(err);
}


static int stem_rfunc(const uint8_t *ptr, int len, const uint8_t **stemptr,
		      int *lenptr, void *context)
{
	SEXP str, inchr, outchr, fcall, ans;
	struct stemmer *stemmer = context;
	const uint8_t *stem;
	int nprot = 0, stemlen;

	assert(!stemmer->error);

	// assume that the R code will error
	stemmer->error = CORPUS_ERROR_INVAL;

	// default to returning NA
	stem = NULL;
	stemlen = -1;

	// cast argument to SEXP
	PROTECT(str = allocVector(STRSXP, 1)); nprot++;
	inchr = mkCharLenCE((const char *)ptr, len, CE_UTF8);
	SET_STRING_ELT(str, 0, inchr);

	// set up the function call
	PROTECT(fcall = lang2(stemmer->value.rfunc.fn, R_NilValue)); nprot++;
	SETCADR(fcall, str);

	// evaluate the call
	PROTECT(ans = eval(fcall, stemmer->value.rfunc.rho)); nprot++;

	// check for logical NA
	if (TYPEOF(ans) == LGLSXP && XLENGTH(ans) == 1
			&& LOGICAL(ans)[0] == NA_LOGICAL) {
		goto out;
	}

	// check for scalar string
	if (ans != R_NilValue && TYPEOF(ans) != STRSXP) {
		error("'stemmer' returned a non-string value for input \"%s\"",
		      translateChar(inchr));
	} else if (ans == R_NilValue || XLENGTH(ans) == 0) {
		error("'stemmer' did not return a value for input \"%s\"",
		      translateChar(inchr));
	} else if (XLENGTH(ans) > 1) {
		error("'stemmer' returned multiple values for input \"%s\"",
		      translateChar(inchr));
	}

	// convert to UTF-8
	PROTECT(ans = utf8_coerce(ans)); nprot++;

	outchr = STRING_ELT(ans, 0);
	if (outchr != NA_STRING) {
		stem = (const uint8_t *)CHAR(outchr);
		stemlen = LENGTH(outchr);
	}
out:
	if (stemptr) {
		*stemptr = stem;
	}
	if (lenptr) {
		*lenptr = stemlen;
	}

	UNPROTECT(nprot);
	// if we made it here, then the R code didn't error;
	stemmer->error = 0;
	return 0;
}


void stemmer_init_rfunc(struct stemmer *s, SEXP fn, SEXP rho)
{
	s->value.rfunc.fn = fn;
	s->value.rfunc.rho = rho;
	s->type = STEMMER_RFUNC;
	s->stem_func = stem_rfunc;
	s->stem_context = s;
	s->error = 0;
}


void stemmer_destroy(struct stemmer *s)
{
	switch (s->type) {
	case STEMMER_SNOWBALL:
		corpus_stem_snowball_destroy(&s->value.snowball);
		break;
	default:
		break;
	}
}
