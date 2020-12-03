# Data-wrangling ########################################################

data <- read.csv("Day 3/day3_data.txt", header = F)
n <- nrow(data)


# Problem 1 #############################################################

trees <- 0
for (i in seq_len(n)) {
    pos <- ((i - 1) * 3 + 1) %% 31
    if (pos == 0) pos <- 31
    if (substr(data$V1[i], pos, pos) == "#") trees <- trees + 1
}


# Problem 2 #############################################################

d3_fun <- function(right, down) {
    trees <- 0
    for (i in seq(1, n, by = down)) {
        pos <- ((i - 1) * right / down + 1) %% 31
        if (pos == 0) pos <- 31
        if (substr(data$V1[i], pos, pos) == "#") trees <- trees + 1
    }
    return(trees)
}

d3_fun(3, 1) * d3_fun(1, 1) * d3_fun(5, 1) * d3_fun(7, 1) * d3_fun(1, 2)