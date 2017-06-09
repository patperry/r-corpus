context("text_split_tokens")


test_that("'split_tokens' can split into threes", {
    text <- c(paste(LETTERS, collapse = " "),
              paste(letters, collapse = " "))

    expect_equal(text_split(text, "tokens", 3),
        data.frame(parent = c(rep(1, 9), rep(2, 9)),
                   index = c(1:9, 1:9),
                   text = as_text(c("A B C ", "D E F ", "G H I ", "J K L ",
                                    "M N O ", "P Q R ", "S T U ", "V W X ",
                                    "Y Z",
                                    "a b c ", "d e f ", "g h i ", "j k l ",
                                    "m n o ", "p q r ", "s t u ", "v w x ",
                                    "y z"))))
})


test_that("'split_tokens' doesn't count dropped tokens", {
    text <- c(paste(LETTERS, collapse = " "),
              paste(letters, collapse = " "))
    f <- token_filter(drop = c("a", "e", "i", "o", "u"))

    expect_equal(text_split(text, "tokens", 5, filter = f),
        data.frame(parent = c(rep(1, 5), rep(2, 5)),
                   index = c(1:5, 1:5),
                   text = as_text(c("A B C D E F G ", "H I J K L M ",
                                    "N O P Q R S ", "T U V W X Y ", "Z",
                                    "a b c d e f g ", "h i j k l m ",
                                    "n o p q r s ", "t u v w x y ", "z"))))
})


test_that("'split_tokens' keeps trailing whitespace", {
    expect_equal(text_split("abc  ", "tokens", 2),
        data.frame(parent = 1, index = 1, text = as_text("abc  ")))
})


test_that("'split_tokens' handles whitespace-only text", {
    expect_equal(text_split("   ", "tokens", 1),
        data.frame(parent = 1, index = 1, text = as_text("   ")))
})


test_that("'split_tokens' handles empty and missing text", {
    expect_equal(text_split(c("", NA, NA, "", "a"), "tokens", 1),
        data.frame(parent = c(1, 4, 5), index = c(1, 1, 1),
                   text = as_text(c("", "", "a"))))
})


test_that("text_count can works on tokens", {
    text <- c(a="He said, 'Are you going?' John Shook his head.",
              b="'Are you going?' John asked",
              c="This. Is. A. Long. Sentence!!!",
              d="Why all the shouting??")
    n0 <- text_count(text, "tokens")
    split <- text_split(text, "tokens")
    n <- c(with(split, tapply(index, parent, length)))
    names(n) <- names(text)
    expect_equal(n, n0)
})
