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

na.fail.corpus_text <- function(object, ...)
{
    if (!anyNA(object))
        object
    else stop("missing values in object")
}


na.omit.corpus_text <- function(object, ...)
{
    if (!anyNA(object)) {
        return(object)
    }

    omit <- which(is.na(object))
    names(omit) <- names(object)[omit]
    object <- object[-omit]
    attr(omit, "class") <- "omit"
    attr(object, "na.action") <- omit
    object
}


na.exclude.corpus_text <- function(object, ...)
{
    object <- na.omit.corpus_text(object, ...)
    exclude <- attr(object, "na.action")
    if (is.null(exclude)) {
        return(object)
    }

    attr(exclude, "class") <- "exclude"
    attr(object, "na.action") <- exclude
    object
}
