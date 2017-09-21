context("text_types")

test_that("'text_types' works elementwise", {
    text <- c("I saw Mr. Jones today.",
              NA,
              "",
              "Split across\na line.",
              "What. Are. You. Doing????",
              "She asked 'do you really mean that?' and I said 'yes.'")
    toks <- text_tokens(text)
    typs <- lapply(toks, function(x) unique(sort(x, method = "radix")))
    typs_tot <- unique(sort(c(toks, recursive = TRUE), method = "radix"))
     
    expect_equal(text_types(text), typs)
    expect_equal(text_types(text, collapse = TRUE), typs_tot)
})


test_that("text_ntype works on types", {
    expect_equal(text_ntype(LETTERS, collapse = TRUE), 26)

    expect_equal(text_ntype(paste(LETTERS, letters, LETTERS)),
                 rep(1, 26))
})


test_that("text_ntype handles NA, empty", {
    expect_equal(text_ntype(c("", NA, "hello world")),
                 c(0, NA, 2))

    expect_equal(text_ntype(c("", NA, "hello world"), collapse = TRUE),
                 NA_real_)
})
