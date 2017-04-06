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
#include <stdint.h>
#include <syslog.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}


static const R_CallMethodDef CallEntries[] = {
        {NULL, NULL, 0}
};


void R_init_corpus(DllInfo *dll)
{
	openlog("corpus", LOG_CONS | LOG_PERROR | LOG_PID, LOG_USER);
	setlogmask(LOG_UPTO(LOG_INFO));
	//setlogmask(LOG_UPTO(LOG_DEBUG));

        syslog(LOG_DEBUG, "loading corpus module");
	R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
}


void R_unload_corpus(DllInfo *dll)
{
	(void)dll;
	syslog(LOG_DEBUG, "unloading corpus module");
        closelog();
}
