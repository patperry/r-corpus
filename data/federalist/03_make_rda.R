#!/usr/local/bin/Rscript --vanilla

library("corpus")

raw <- read_ndjson("federalist.json", text = NULL, stringsAsFactors = FALSE)

name <- paste("Federalist No.", raw$paper_id)

venue <- raw$venue
venue[venue == "For the Independent Fournal"] <- "For the Independent Journal"
venue[grep("^From M[cC]", venue)] <- "From McLean's Edition, New York"

author <- raw$author
author[author == "HAMILTON"] <- "Hamilton"
author[author == "HAMILTON AND MADISON"] <- NA
author[author == "HAMILTON OR MADISON"] <- NA
author[author == "JAY"] <- "Jay"
author[author == "MADISON"] <- "Madison"
author[raw$paper_id == 58] <- NA # follow Mosteller and Wallace

date <- raw$date
date <- sub("^(Tuesday|Thursday|Friday),? ", "", date)

invisible(Sys.setlocale("LC_TIME", "C"))
date <- as.Date(date, "%B %d, %Y")

federalist <- data.frame(name,
                         title = raw$title,
                         venue,
                         date,
                         author,
                         text = raw$text,
                         stringsAsFactors = FALSE)

save(federalist, file = "../federalist.rda")
tools::resaveRdaFiles("../federalist.rda")
