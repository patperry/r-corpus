context("text_split_tokens")


test_that("'split_tokens' can split into threes", {
    text <- c(paste(LETTERS, collapse = " "),
              paste(letters, collapse = " "))

    expect_equal(text_split(text, "tokens", 3),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(as.character(c(rep(1, 9), rep(2, 9)))),
                   index = c(1:9, 1:9),
                   text = as_corpus_text(c("A B C ", "D E F ", "G H I ", "J K L ",
                                    "M N O ", "P Q R ", "S T U ", "V W X ",
                                    "Y Z",
                                    "a b c ", "d e f ", "g h i ", "j k l ",
                                    "m n o ", "p q r ", "s t u ", "v w x ",
                                    "y z")),
                   row.names = NULL)))
})


test_that("'split_tokens' doesn't count dropped tokens", {
    text <- c(paste(LETTERS, collapse = " "),
              paste(letters, collapse = " "))
    f <- text_filter(drop = c("a", "e", "i", "o", "u"))

    expect_equal(text_split(text, "tokens", 5, filter = f),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(as.character(c(rep(1, 5), rep(2, 5)))),
                       index = c(1:5, 1:5),
                       text = as_corpus_text(c("A B C D E F G ", "H I J K L ",
                                        "M N O P Q ", "R S T U V ", "W X Y Z",
                                        "a b c d e f g ", "h i j k l ",
                                        "m n o p q ", "r s t u v ",
                                        "w x y z"),
                                      filter = f),
                       row.names = NULL)))
})


test_that("'split_tokens' keeps trailing whitespace", {
    expect_equal(text_split("abc  ", "tokens", 2),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor("1"), index = 1,
                       text = as_corpus_text("abc  "), row.names = NULL)))
})


test_that("'split_tokens' handles whitespace-only text", {
    expect_equal(text_split("   ", "tokens", 1),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor("1"),
                       index = 1, text = as_corpus_text("   "), row.names = NULL)))
})


test_that("'split_tokens' handles empty and missing text", {
    expect_equal(text_split(c("", NA, NA, "", "a"), "tokens", 1),
        structure(class = c("corpus_frame", "data.frame"),
            data.frame(parent = factor(c("1", "4", "5"),
                                       levels = as.character(1:5)),
                       index = c(1, 1, 1),
                       text = as_corpus_text(c("", "", "a")),
                       row.names = NULL)))
})
