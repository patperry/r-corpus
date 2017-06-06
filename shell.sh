#!/usr/bin/env expect

spawn R --quiet --vanilla

expect {
"> " { send "invisible(Sys.setlocale(locale = 'C')); devtools::load_all('.')\n" }
}

interact
