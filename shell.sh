#!/usr/bin/env expect

spawn R --quiet --vanilla

expect {
  "> " { send "library('devtools'); load_all('.')\n" }
}

interact
