

if (utils::packageVersion("testthat") <= "1.0.2") {
    capture_output <- function(code, print = FALSE, width = 80) {
        oldwidth <- getOption("width")
        if (width != oldwidth) {
            options(width = width)
            on.exit(options(width = oldwidth), add = TRUE)
        }
        testthat::capture_output(code, print)
    }
}
