
#include "rcorpus.h"

SEXP test_print(SEXP x)
{
	Rprintf("%s\n", translateChar(STRING_ELT(x, 0)));
	return R_NilValue;
}
