#!/usr/bin/env expect

spawn R --quiet --vanilla

expect {
"> " { send "invisible(Sys.setlocale('LC_COLLATE', 'C')); devtools::load_all('.')\n" }
}

interact
