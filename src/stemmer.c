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


void stemmer_init_rfunc(struct stemmer *s, SEXP fn, SEXP rho)
{
	s->value.rfunc.fn = fn;
	s->value.rfunc.rho = rho;
	s->type = STEMMER_RFUNC;
	s->stem_func = NULL; // TODO: fix
	s->stem_context = NULL; // TODO: fix
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
