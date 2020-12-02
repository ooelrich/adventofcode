# Data-wrangling ###############################################

data <- read.csv("Day 1/day1_data.txt")
n <- nrow(data)


# Problem 1 #####################################################

for (i in seq_len(n-1)) {
  for (j in (i+1):(n)) {
    if (sum(data$val[c(i, j)]) == 2020) sol <- c(i, j)
  }
}

vals <- data$val[sol]
b <- prod(vals)
print(paste(vals[1], " * ", vals[2], " = ", b))


# Problem 2 ####################################################

for (i in seq_len(n-2)) {
  for (j in (i+1):(n-1)) {
    for (k in (j+1):n) {
      if (sum(data$val[c(i, j, k)]) == 2020) sol <- c(i, j, k)
    }
  }
}

vals <- data$val[sol]
b <- prod(vals)
print(paste(vals[1], " * ", vals[2], " * ", vals[3], " = ", b))