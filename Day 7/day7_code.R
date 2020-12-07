# Data-wrangling ##############################################################

library(stringr)
data <- readChar("Day 7/day7_data.txt", file.info("Day 7/day7_data.txt")$size)
data <- str_split(data, "\n")
data <- data[[1]]
