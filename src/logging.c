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
#include "corpus/src/error.h"


static void ignore_message(int code, const char *message)
{
	(void)code;
	(void)message;
}


SEXP logging_off(void)
{
	corpus_log_func = ignore_message;
	return R_NilValue;
}


SEXP logging_on(void)
{
	corpus_log_func = NULL;
	return R_NilValue;
}
