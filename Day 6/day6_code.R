# Data-wrangling ##############################################################

library(stringr)

data <- readChar("Day 6/day6_data.txt", file.info("Day 6/day6_data.txt")$size)
data <- str_split(data, "\n\n")
n <- length(data[[1]])
for (i in seq_len(n)) {
    data[[1]][i] <- str_remove_all(data[[1]][i], "\n")
}


# Problem 1 ###################################################################

affirmative <- c()
for (i in seq_len(n)) {
    affirmative[i] <- sum(!!str_count(data[[1]][i], letters))
}
sum(affirmative)
!!str_count(data[[1]][1], letters)


# Problem 2 ###################################################################

data <- readChar("Day 6/day6_data.txt", file.info("Day 6/day6_data.txt")$size)
data <- str_split(data, "\n\n")

in_all_val <- rep(0, n)
for (i in seq_len(n)) {
    data2 <- str_split(data[[1]][i], "\n")
    in_all <- rep(1, 26)
    for (j in seq_len(length(data2[[1]]))) {
        in_all <- in_all * as.numeric(!!str_count(data2[[1]][j], letters))
    }
    in_all_val[i] <- sum(in_all)
}
sum(in_all_val)
