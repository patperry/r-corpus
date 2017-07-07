
#include "rcorpus.h"

SEXP test_print(SEXP x)
{
	Rprintf("native: '%s'\n", translateChar(STRING_ELT(x, 0)));
	Rprintf("utf-8:  '%s'\n", translateCharUTF8(STRING_ELT(x, 0)));
	Rprintf("raw:    '%s'\n", CHAR(STRING_ELT(x, 0)));
	return R_NilValue;
}
