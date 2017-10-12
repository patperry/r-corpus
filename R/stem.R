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


stem_snowball <- function(x, algorithm = "en")
{
    with_rethrow({
        x <- as_character_vector("x", x)
        algorithm <- as_snowball_algorithm("algorithm", algorithm)
    })

    .Call(C_stem_snowball, x, algorithm)
}


new_stemmer<- function(term, stem, default = NULL, duplicates = "first",
                       vectorize = TRUE)
{
    call <- sys.call()
    with_rethrow({
        term <- as_character_vector("term", term)
        stem <- as_character_vector("stem", stem)
        default <- as_character_scalar("default", default)
        duplicates <- as_enum("duplicates", duplicates,
                              c("first", "last", "omit", "fail"))
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

    if (duplicates == "last") {
        term <- rev(term)
        stem <- rev(stem)
        duplicates <- "first"
    }

    if (duplicates != "first") {
        dup <- duplicated(term)
        if (duplicates == "omit") {
            dups <- term[dup]
            rm <- term %in% dups
            term <- term[!rm]
            stem <- stem[!rm]
        } else if (any(dup)) { # duplicates == "fail"
            stop("'term' argument entries must be unique")
        }
    }

    # parse dynamically so that we can add a comment with the function call
    comment <- paste("    #", deparse(call), collapse = "\n")
    if (is.null(default)) {
        src <- paste('function(x) {',
            comment,
            '    i <- match(x, term, 0L)',
            '    if (i > 0L)',
            '        stem[[i]]',
            '    else x',
            '}',
            sep = '\n')
    } else {
        src <- paste('function(x) {',
            comment,
            '    i <- match(x, term, 0L)',
            '    if (i > 0L)',
            '        stem[[i]]',
            '    else default',
            '}',
            sep = '\n')
    }

    env <- new.env()
    assign("term", term, env)
    assign("stem", stem, env)
    assign("default", default, env)
    stem_term <- eval(parse(text = src), env)

    if (vectorize) {
        vsrc <- paste('function(x) {',
            comment,
            '    vapply(x, stem_term, "", USE.NAMES = !is.null(names(x)))',
            '}',
            sep = '\n')
        assign("stem_term", stem_term, env)
        stem_term <- eval(parse(text = vsrc, keep.source = TRUE), env)
    }

    stem_term
}
