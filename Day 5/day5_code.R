# Data-wrangling #########################################

data <- read.csv("Day 5/day5_data.txt", header = F)


# Problem 1 ##############################################

for (i in seq_len(nrow(data))) {
    data$row[i] <- substr(data$V1[i], 1, 7)
    data$seat[i] <- substr(data$V1[i], 8, 10)

}

data$row <- gsub("F", "0", data$row)
data$row <- gsub("B", "1", data$row)


data$row_number <- as.numeric(substr(data$row, 1, 1)) * 2^6 +
                   as.numeric(substr(data$row, 2, 2)) * 2^5 +
                   as.numeric(substr(data$row, 3, 3)) * 2^4 +
                   as.numeric(substr(data$row, 4, 4)) * 2^3 +
                   as.numeric(substr(data$row, 5, 5)) * 2^2 +
                   as.numeric(substr(data$row, 6, 6)) * 2^1 +
                   as.numeric(substr(data$row, 7, 7)) * 2^0

data$seat <- gsub("L", "0", data$seat)
data$seat <- gsub("R", "1", data$seat)

data$seat_number <- as.numeric(substr(data$seat, 1, 1)) * 2^2 +
                    as.numeric(substr(data$seat, 2, 2)) * 2^1 +
                    as.numeric(substr(data$seat, 3, 3)) * 2^0

data$seat_id <- data$row_number * 8 + data$seat_number
max(data$seat_id)


# Part 2 ##########################################################

for (i in min(data$seat_id):max(data$seat_id)) {
    if (i %in% data$seat_id != TRUE) print(i)
}