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

stemmer_make <- function(term, stem, default = NULL, ties = "first",
                         vectorize = TRUE)
{
    call <- sys.call()
    with_rethrow({
        term <- as_character_vector("term", term)
        stem <- as_character_vector("stem", stem)
        default <- as_character_scalar("default", default)
        ties <- as_enum("ties", ties, c("first", "last", "omit", "fail"))
    })

    if (is.null(term)) {
        term <- character()
    }

    if (is.null(stem)) {
        stem <- character()
    }

    if (length(term) != length(stem)) {
        stop("'term' argument length must equal 'stem' argument length")
    }

    if (ties == "last") {
        termlist <- rev(term)
        stemlist <- rev(stem)
        ties <- "first"
    } else {
        termlist <- term
        stemlist <- stem
    }

    if (ties != "first") {
        dup <- duplicated(termlist)
        if (ties == "omit") {
            dups <- termlist[dup]
            rm <- termlist %in% dups
            termlist <- termlist[!rm]
            stemlist <- stemlist[!rm]
        } else if (any(dup)) { # ties == "fail"
            stop("'term' argument entries must be unique")
        }
    }

    # parse dynamically so that we can add a comment with the function call
    comment <- paste("    #", deparse(call), collapse = "\n")
    if (is.null(default)) {
        src <- paste('function(term) {',
            comment,
            '    i <- match(term, termlist, 0L)',
            '    if (i > 0L) {',
            '        stemlist[[i]]',
            '    } else {',
            '        term',
            '    }',
            '}',
            sep = '\n')
    } else {
        src <- paste('function(term) {',
            comment,
            '    i <- match(term, termlist, 0L)',
            '    if (i > 0L) {',
            '        stemlist[[i]]',
            '    } else {',
            '        default',
            '    }',
            '}',
            sep = '\n')
    }

    env <- new.env()
    assign("termlist", termlist, env)
    assign("stemlist", stemlist, env)
    assign("default", default, env)
    stem_term <- eval(parse(text = src), env)

    if (vectorize) {
        vsrc <- paste('function(term) {',
            comment,
            '    use.names <- !is.null(names(term))',
            '    vapply(term, stem_term, "", USE.NAMES = use.names)',
            '}',
            sep = '\n')
        assign("stem_term", stem_term, env)
        stem_term <- eval(parse(text = vsrc, keep.source = TRUE), env)
    } else {
        stem_term
    }
}
