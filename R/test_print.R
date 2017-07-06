
test_print <- function(x)
{
    x <- as.character(x)
    invisible(.Call(C_test_print, x))
}
