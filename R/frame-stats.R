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

na.fail.corpus_frame <- function(object, ...)
{
    if (!any(vapply(object, anyNA, FALSE)))
        object
    else stop("missing values in object")
}


na.omit.corpus_frame <- function(object, ...)
{
    if (!any(vapply(object, anyNA, FALSE))) {
        return(object)
    }

    # find the missing entries; cast to a matrix
    na <- matrix(c(lapply(object, is.na), recursive = TRUE),
                 ncol = length(object))

    # find rows containing missing entries
    omit <- which(apply(na, 1, any))
    names(omit) <- rownames(object)[omit]

    # drop the rows that miss observations
    object <- object[-omit,,drop = FALSE]
    attr(omit, "class") <- "omit"
    attr(object, "na.action") <- omit
    object
}


na.exclude.corpus_frame <- function(object, ...)
{
    object <- na.omit.corpus_frame(object, ...)
    exclude <- attr(object, "na.action")
    if (is.null(exclude)) {
        return(object)
    }

    attr(exclude, "class") <- "exclude"
    attr(object, "na.action") <- exclude
    object
}
