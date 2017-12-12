#  Copyright 2017 Patrick O. Perry.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.


# retrieved from https://www.gutenberg.org/MIRRORS.ALL on 2017-08-24
gutenberg_default_mirrors <- c(
    "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/",
    "http://eremita.di.uminho.pt/gutenberg/",
    "http://mirror.csclub.uwaterloo.ca/gutenberg/",
##    "http://www.gutenberg.org/dirs/", prefer the other mirrors; this one is flaky
    "http://mirrors.xmission.com/gutenberg/",
    "http://gutenberg.pglaf.org/",
    "http://aleph.gutenberg.org/",
    "http://gutenberg.readingroo.ms/")


gutenberg_get_mirror <- function(verbose = TRUE)
{
    # used cached value if possible
    mirror <- getOption("gutenberg_mirror")
    if (!is.null(mirror)) {
        return(mirror)
    }

    # download list of mirrors
    mirrors_url <- "https://www.gutenberg.org/MIRRORS.ALL"
    if (verbose) {
        message("Determining mirror for Project Gutenberg from ", mirrors_url)
    }

    # don't error if the download fails
    meta <- tryCatch({
        readLines(mirrors_url)
    }, error = function(e) {
        character()
    })

    # only use http mirrors
    pattern <- "^.*\\| (http://[^[:space:]]*)[[:space:]]+\\|.*$"
    rows <- grep(pattern, meta, value = TRUE)
    urls <- sub(pattern, "\\1", rows)

    if (length(urls) == 0) {
        message("Failed retrieving MIRRORS.ALL; using list from 2017-08-24 (possibly outdated)")
        urls <- gutenberg_default_mirrors
    }

    # choose a mirror based on the current time
    i <- 1 + as.integer(Sys.time()) %% length(urls)
    mirror <- urls[[i]]

    if (verbose) {
        message("Using mirror ", mirror)
    }

    # cache the result
    options(gutenberg_mirror = mirror)
    return(mirror)
}


# Partly based on gutenbergr::gutenberg_strip in that we use some of the
# same regexes for detecting the header and footer.
#
# The implementation here is different, though, and in particular, unlike
# gutenbergr, we parse the meta-data from the header and convert the character
# encoding if one is declared in the "Character set encoding" field.
#
# There are no lines in common with gutenbergr::gutenberg_strip.
#
gutenberg_parse <- function(lines, verbose = TRUE)
{
    # number the lines
    i <- seq_along(lines)

    # find the empty lines
    empty <- (lines == "")

    # find the end of the Project Gutenberg header
    start_pat <- paste("^[*][*][*].*PROJECT GUTENBERG.*[*][*][*]",
                       "END.*SMALL PRINT", sep = "|")
    start_match <- grep(start_pat, lines)
    if (length(start_match) == 0) {
        start_match <- 0
    }
    start <- start_match[[1]] + 1

    # look for the encoding, and convert to UTF-8
    enc_pat <- "Character set encoding:[[:space:]]*(.*)[[:space:]]*$"
    enc_match <- grep(enc_pat, lines[seq_len(start - 1)])
    if (length(enc_match) > 0) {
        enc <- sub(enc_pat, "\\1", lines[enc_match[[1]]])
        if (!enc %in% c("ASCII", "UTF-8")) {
            if (verbose) {
                message("Converting text from declared encoding \"", enc,
                        "\" to UTF-8")
            }
            lines <- iconv(lines, enc, "UTF-8")
        }
    }

    # find metadata
    fields <- c("title" = "Title",
                "author" = "Author",
                "language" = "Language")

    meta <- rep(NA_character_, length(fields))
    names(meta) <- names(fields)

    for (f in names(fields)) {
        pattern <- paste0("^", fields[[f]], ":[[:space:]]*(.*)[[:space:]]*$")
        match <- grep(pattern, lines)

        match <- match[match < start]
        if (length(match) > 0) {
            # take the field from the first match
            meta[[f]] <- sub(pattern, "\\1", lines[match[[1]]])
        }
    }

    # skip the empty lines at the start
    start <- min(which(start <= i & !empty))

    # find the start of the Project Gutenberg footer
    end_pat <- paste("^End of .*Project Gutenberg.*",
                     "\\\\*\\\\*\\\\*.*END OF.*PROJECT GUTENBERG", sep = "|")

    end_match <- grep(end_pat, lines)
    if (length(end_match) == 0) {
        end_match <- length(lines) + 1
    }
    end <- end_match[[1]] - 1

    # skip the empty lines at the end
    end <- max(which(i <= end & !empty))

    # skip the production notes at the start of the text
    note_start_pat <- paste("produced by",
                            "prepared by",
                            "transcribed from",
                            "project gutenberg",
                            "^[*][*][*]",
                            "^note: ",
                            "^special thanks",
                            "^this is a retranscription",
                            sep = "|")
    note_start <- grep(note_start_pat, lines, ignore.case = TRUE)
    note_start <- note_start[start <= note_start & note_start <= end]

    while (length(note_start) && note_start[[1]] == start) {
        # the note ends at the first empty line
        note_end <- min(which(note_start[[1]] <= i & empty))

        start <- min(which(note_end + 1 <= i & !empty))
        note_start <- note_start[start <= note_start]
    }

    # concatenate the content lines
    text <- paste(lines[start:end], collapse = "\n")

    record <- c(meta, text = text)
    as.list(record)
}


# Partly based on gutenbergr::gutenberg_download but we use a different
# algorithm to form the URL and download the file.
#
# The gutenbergr version attempts to download the ".zip", version of the file,
# and if that fails, attempts to download the "-8.zip" and then the "-0.zip"
# version.  We instead list the available zip files, and then pick one to
# download, preferring, in order "-0.zip", "-8.zip", and ".zip".
#
# The only lines in common between the two implementations are the following:
#
# if (is.null(mirror)) {
#     mirror <- gutenberg_get_mirror(verbose = verbose)
# }
#
gutenberg_download <- function(id, mirror = NULL, verbose = TRUE)
{
    id <- as.integer(id)[[1]]
    if (is.na(id)) {
        return(list(title = NA_character_, author = NA_character_,
                    language = NA_character_, text = NA_character_))
    }

    if (is.null(mirror)) {
        mirror <- gutenberg_get_mirror(verbose = verbose)
    }

    # get the book directory
    sep <- ifelse(grepl("/$", mirror), "", "/")
    path <- gsub("(.)", "\\1/", id %/% 10)
    base_url <- paste0(mirror, sep, path, id, "/")

    # list the zip files
    index <- readLines(base_url, encoding = "UTF-8", warn = FALSE)
    pattern <- paste0("^.*href=\"", id, "([^.]*).zip\".*$")
    suffix <- sub(pattern, "\\1", grep(pattern, index, value = TRUE))

    # only use plain text formats
    suffix <- suffix[suffix %in% c("-0", "-8", "")]

    # fall back to plain text if something went wrong downloading the index
    if (length(suffix) == 0) {
        stop("failed determining available file extensions")
    }

    # Files have typically have suffixes "-0", "-8", "". I couldn't
    # find documentation, but it appears that "-0" is UTF-8 encoded,
    # "-8" is original encoding, "" is unknown.

    # Prefer the first matching file in the directory.
    s <- suffix[[1]]
    base_name <- paste0(id, s)
    url <- paste0(base_url,  base_name, ".zip")
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(url, tmp, quiet = !verbose)
    on.exit(unlink(tmp), add = TRUE)

    con <- unz(tmp, paste0(base_name, ".txt"), encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    lines <- readLines(con, encoding = "UTF-8", warn = FALSE)

    if (id == 1) {
        start <- min(which(lines == "The Declaration of Independence of The United States of America"))
        end <- max(which(lines == "December, 1972  [Etext #2]")) - 4
        record <- list(title = "The Declaration of Independence of The United States of America",
                       author = "Founding Fathers",
                       language = "English",
                       text = paste(lines[start:end], collapse = "\n"))
    } else {
        record <- gutenberg_parse(lines, verbose = verbose)
    }

    record
}


gutenberg_corpus <- function(ids, filter = NULL, mirror = NULL,
                             verbose = TRUE, ...)
{
    with_rethrow({
        ids <- as_integer_vector("ids", ids)
        filter <- as_filter("filter", filter)
        mirror <- as_character_scalar("mirror", mirror)
        verbose <- as_option("verbose", verbose)
    })

    rows <- lapply(ids, gutenberg_download, mirror = mirror, verbose = verbose)

    args <- rows
    names(args) <- NULL
    args[["stringsAsFactors"]] <- FALSE
    data <- do.call(rbind.data.frame, args)

    rownames(data) <- names(rows)
    as_corpus_frame(data, filter, ...)
}
