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


# RStudio doesn't support ANSI faint, use gray instead
style_faint <- "38;5;246" #666666

# RStudio ANSI bold is broken, use color instead
# https://github.com/rstudio/rstudio/issues/1721
style_bold <- "38;5;203" #FF3333
#style_bold <- "36" # cyan
#style_bold <- "38;5;63" #3333FF
