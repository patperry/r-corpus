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
    meta <- readLines(mirrors_url)

    # only use http mirrors
    pattern <- "^.*\\| (http://[^[:space:]]*)[[:space:]]+\\|.*$"
    rows <- grep(pattern, meta, value = TRUE)
    urls <- sub(pattern, "\\1", rows)

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


# partly based on gutenbergr::gutenberg_strip, written by David Robinson
gutenberg_parse <- function(lines, verbose = TRUE)
{
    # number the lines
    i <- seq_along(lines)

    # find the empty lines
    empty <- (lines == "")

    # find the end of the Project Gutenberg header
    start_pat <- paste("^\\\\*\\\\*\\\\*.*PROJECT GUTENBERG",
                       "END.*SMALL PRINT", sep = "|")
    start_match <- grep(start_pat, lines)
    if (length(start_match) == 0) {
        start_match <- 0
    }
    start <- start_match[[1]] + 1

    # find metadata
    fields <- c("title" = "Title",
                "author" = "Author",
                "language" = "Language",
                "encoding" = "Character set encoding")
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
                            "^note: ",
                            "^special thanks", sep = "|")
    note_start <- grep(note_start_pat, lines, ignore.case = TRUE)
    note_start <- note_start[start <= note_start & note_start <= end]

    while (length(note_start) && note_start[[1]] == start) {
        # the note ends at the first empty line
        note_end <- min(which(note_start[[1]] <= i & empty))

        start <- min(which(note_end + 1 <= i & !empty))
        note_start <- note_start[start <= note_start]
    }

    text <- paste(lines[start:end], collapse = "\n")

    # convert from the declared encoding to UTF-8
    enc <- meta[["encoding"]]
    if (!is.na(enc) && !enc %in% c("ASCII", "UTF-8") && enc %in% iconvlist()) {
        if (verbose) {
            message("Converting text from declared encoding \"", enc,
                    "\" to UTF-8")
        }
        text <- iconv(text, enc, "UTF-8")
    }
    meta <- meta[-match("encoding", names(meta))]

    record <- c(meta, text = text)
    as.list(record)
}


# partly based on gutenbergr::gutenberg_download, written by David Robinson
gutenberg_download <- function(id, mirror = NULL, verbose = TRUE)
{
    id <- as.integer(id)[[1]]

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

    # Files have typically have suffixes "-0", "-8", "". I couldn't
    # find documentation, but it appears that "-0" is UTF-8 encoded,
    # "-8" is original encoding, "" is unknown.

    # Prefer the first matching file in the directory.
    base_name <- paste0(id, suffix[[1]])
    url <- paste0(base_url,  base_name, ".zip")
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(url, tmp, quiet = !verbose)
    on.exit(unlink(tmp), add = TRUE)

    con <- unz(tmp, paste0(base_name, ".txt"))
    on.exit(close(con), add = TRUE)
    lines <- readLines(con, encoding = "UTF-8", warn = FALSE)

    record <- gutenberg_parse(lines, verbose = verbose)

    record
}


gutenberg_corpus <- function(ids, mirror = NULL, verbose = TRUE, ...)
{
    rows <- lapply(ids, gutenberg_download, mirror = mirror, verbose = verbose)

    args <- rows
    names(args) <- NULL
    args[["stringsAsFactors"]] <- FALSE
    data <- do.call(rbind.data.frame, args)

    rownames(data) <- names(rows)
    as_corpus(data, ...)
}
