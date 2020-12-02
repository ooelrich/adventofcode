# Packages ##############################################################

library(stringr)
library(tidyr)

# Data-wrangling ########################################################

data <- read.csv("Day 2/day2_data.txt", header = F, sep = "")
data <- separate(data, col = "V1", sep = "-", into = c("upper", "lower"))
data$V2 <- gsub('.{1}$', '', data$V2)
data$upper <- as.numeric(data$upper)
data$lower <- as.numeric(data$lower)
n <- nrow(data)

# Problem 1 #############################################################

correct <- c()
for (i in seq_len(n)) {
  LL <- (str_count(data$V3[i], data$V2[i]) <= data$lower[i])
  UL <- (str_count(data$V3[i], data$V2[i]) >= data$upper[i])
  if (LL && UL) correct[i] <- 1
}
sum(correct, na.rm = TRUE)

# Problem 2 #############################################################

correct <- rep(0, n)
for (i in seq_len(n)) {
  c1 <- as.numeric(substr(data$V3[i], data$lower[i], data$lower[i]) == data$V2[i])
  c2 <- as.numeric(substr(data$V3[i], data$upper[i], data$upper[i]) == data$V2[i])
  no_correct <-  c1 + c2
  if (no_correct == 1) correct[i] <- 1
}
sum(correct)
