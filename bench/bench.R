
Sys.setlocale(locale = "C")
files <- dir("bench", "^bench-.*\\.[rR]$", full.names = TRUE)
for (file in files) {
    name <- substr(file, 1, nchar(file) - 2)
    message("Running ", name, "...", appendLF = FALSE)
    time <- proc.time()
    sink(paste0(file, "out"))
    set.seed(0)
    NS <- new.env()
    source(file, local = NS)
    sink()
    new_time <- proc.time()
    diff <- summary(structure(new_time - time, class = "proc_time"))
    elapsed <- diff[["user"]] + diff[["system"]]
    message("done. (", elapsed, "s)")
}
