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


as_utf8 <- function(x)
{
    .Call(C_utf8_coerce, x)
}

# encode an R character string in a form suitable for dsiplay
# on a terminal in the current locale (determined by LC_CTYPE)
utf8_encode <- function(x)
{
    utf8 <- (Sys.getlocale("LC_CTYPE") != "C")
    .Call(C_utf8_encode, x, utf8)
}


utf8_valid <- function(x)
{
    .Call(C_utf8_valid, x)
}


utf8_width <- function(x)
{
    x <- as_utf8(x)
    .Call(C_utf8_width, x)
}
