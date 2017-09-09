\name{read_ndjson}
\alias{corpus_json}
\alias{read_ndjson}
\title{JSON Data Input}
\description{
    Read data from a file in newline-delimited JavaScript Object Notation
    (NDJSON) format.
}
\usage{
read_ndjson(file, mmap = FALSE, simplify = TRUE, text = NULL)
}
\arguments{
    \item{file}{the name of the file which the data are to be read from,
        or a connection (unless \code{mmap} is \code{TRUE}, see below).
        The data should be encoded as UTF-8, and each line should be a
        valid JSON value.}

    \item{mmap}{whether to memory-map the file instead of reading all
        of its data into memory simultaneously. See the
        \sQuote{Memory mapping} section.}

    \item{simplify}{whether to attempt to simplify the type of the return
       value. For example, if each line of the file stores an integer,
       if \code{simplify} is set to \code{TRUE} then the return value
       will be an integer vector rather than a \code{corpus_json} object.}

    \item{text}{a character vector of string fields to interpret as
       \code{text} instead of \code{character}, or \code{NULL} to
       interpret all strings as \code{character}.}
}
\details{
    This function is the recommended means of reading data for processing
    by the corpus package.

    When the \code{text} argument is non-\code{NULL} string data
    fields with names indicated by this argument are decoded as
    \code{text} values, not as \code{character} values.
}
\section{Memory mapping}{
    When you specify \code{mmap = TRUE}, the function memory-maps the file
    instead of reading it into memory directly. In this case, the \code{file}
    argument must be a character string giving the path to the file, not
    a connection object. When you memory-map the file, the operating
    system reads data into memory only when it is needed, enabling
    you to transparently process large data sets that do not fit into
    memory.

    In terms of memory usage, enabling \code{mmap = TRUE} reduces the
    footprint for \code{corpus_json} and \code{corpus_text} objects;
    native R objects (\code{character}, \code{integer}, \code{list},
    \code{logical}, and \code{numeric}) get fully deserialized to
    memory and produce identical results regardless of whether
    \code{mmap} is \code{TRUE} or \code{FALSE}. To process a large
    text corpus with a text field named \code{"text"}, you should set
    \code{text = "text"} and \code{mmap = TRUE}. Or, to reduce the memory
    footprint even further, set \code{simplify = FALSE} and
    \code{mmap = TRUE}.

    One danger in memory-mapping is that if you delete the file
    after calling \code{read_ndjson} but before processing the data, then
    the results will be undefined, and your computer may crash. (On
    POSIX-compliant systems like Mac OS and Linux, there should be no
    ill effects to deleting the file. On recent versions of Windows,
    the system will not allow you to delete the file as long as the data
    is active.)

    Another danger in memory-mapping is that if you serialize a
    \code{corpus_json} object or derived \code{corpus_text} object using
    \code{\link{saveRDS}} or another similar function, and then you
    deserialize the object, R will attempt create a new memory-map
    using the \code{file} argument passed to the original \code{read_ndjson}
    call. If \code{file} is a relative path, then your working directory
    at the time of deserialization must agree with your working directory
    at the time of the \code{read_ndjson} call.  You can avoid this
    situation by specifying an absolute path as the \code{file} argument
    (the \code{\link{normalizePath}} function will convert a relative
    to an absolute path).
}
\value{
    In the default usage, with argument \code{simplify = TRUE}, when
    the lines of the file are records (JSON object literals), the
    return value from \code{read_ndjson} is a data frame with class
    \code{c("corpus_frame", "data.frame")}. With \code{simplify = FALSE},
    the result is a \code{corpus_json} object.
}
\seealso{
    \code{\link{as_corpus_text}}, \code{\link{as_utf8}}.
}
\examples{
# Memory mapping
lines <- c('{ "a": 1, "b": true }',
           '{ "b": false, "nested": { "c": 100, "d": false }}',
           '{ "a": 3.14, "nested": { "d": true }}')
file <- tempfile()
writeLines(lines, file)
(data <- read_ndjson(file, mmap = TRUE))

data$a
data$b
data$nested.c
data$nested.d

rm("data")
invisible(gc()) # force the garbage collector to release the memory-map
file.remove(file)
}
\keyword{file}
