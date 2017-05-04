\name{read_ndjson}
\alias{read_ndjson}
\title{JSON Data Input}
\description{
    Read data from a file in newline-delimited JavaScript Object Notation
    (NDJSON) format.
}
\usage{
    read_ndjson(file, simplify = TRUE)
}
\arguments{
    \item{file}{the name of the file which the data are to be read from.
        The file should be encoded as UTF-8, and each line of the file
        should be a valid JSON value.}

    \item{simplify}{whether to attempt to simplify the type of the return
       value. For example, if each line of the file stores an integer,
       if \code{simplify} is set to \code{TRUE} then the return value
       will be an integer vector rather than a \code{jsondata} object.}
}
\details{
    This function is the principal means of reading data for processing
    by the corpus package.

    Internally, the function memory-maps the file rather than reading it into
    memory directly. This allows the operating system to only read the data
    into memory when it is needed, enabling you to transparently process
    large data sets that do not fit into memory.

    The return value from \code{read_ndjson} is a \code{jsondata} object
    that behaves like a data frame.

    The one danger in using this function is that if you delete the file
    after calling \code{read_ndjson} but before processing the data, then
    the results will be undefined, and your computer may crash. (On
    POSIX-compliant systems like Mac OS and Linux, there should be no
    ill effects to deleting the file. On recent versions of Windows,
    the system will not allow you to delete the file as long as the data
    is active; on older versions, the behavior is undefined.)
}
\examples{
    lines <- c('{ "a": 1, "b": true }',
               '{ "b": false, "c": { "nested": 100 }}',
               '{ "a": 3.14, "c": { "nested": 1e23 }}')
    file <- tempfile()
    writeLines(lines, file)
    (data <- read_ndjson(file))

    data$a
    data$b
    data$c
    data$c$nested

    rm("data")
    gc() # force the garbage collector to release the memory-map
    file.remove(file)
}
\keyword{file}
