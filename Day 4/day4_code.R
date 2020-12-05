# Data-wrangling #############################################

library(stringr)

data <- readChar("Day 4/day4_data.txt", file.info("Day 4/day4_data.txt")$size)
data <- str_split(data, "\n\n")
for (i in seq_len(285)) {
    data[[1]][i] <- gsub("\n", " ", data[[1]][i])
}

# Problem 1 ###########################################################

no_correct <- 0
for (i in seq_len(285)) {
    a <- rep(0, 7)
    a[1] <- as.numeric(grepl("byr", data[[1]][i], fixed = TRUE))
    a[2] <- as.numeric(grepl("iyr", data[[1]][i], fixed = TRUE))
    a[3] <- as.numeric(grepl("eyr", data[[1]][i], fixed = TRUE))
    a[4] <- as.numeric(grepl("hgt", data[[1]][i], fixed = TRUE))
    a[5] <- as.numeric(grepl("hcl", data[[1]][i], fixed = TRUE))
    a[6] <- as.numeric(grepl("ecl", data[[1]][i], fixed = TRUE))
    a[7] <- as.numeric(grepl("pid", data[[1]][i], fixed = TRUE))
    if (sum(a) == 7) no_correct <- no_correct + 1
}
no_correct

# Problem 2 ###############################################################


is_ok <- rep(0, 285)
for (i in seq_len(285)) {
    a <- c()
    current <- data[[1]][i]
    current <- str_split(current, "\\:| ")

    byr_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "byr") {
            if (1920 <= as.numeric(current[[1]][j + 1])) {
                if (2002 >= as.numeric(current[[1]][j + 1])) byr_correct <- TRUE
            }
        }
    }
    a[1] <- as.numeric(byr_correct)

    iyr_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "iyr") {
            if (2010 <= as.numeric(current[[1]][j + 1])) {
                if (2020 >= as.numeric(current[[1]][j + 1])) iyr_correct <- TRUE
            }
        }
    }
    a[2] <- as.numeric(iyr_correct)

    eyr_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "eyr") {
            if (2020 <= as.numeric(current[[1]][j + 1])) {
                if (2030 >= as.numeric(current[[1]][j + 1])) eyr_correct <- TRUE
            }
        }
    }
    a[3] <- as.numeric(eyr_correct)

    hgt_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "hgt") {
            hgt <- current[[1]][j + 1]
            type <- substr(hgt, nchar(hgt) - 1, nchar(hgt))
            val <- as.numeric(substr(hgt, 1, nchar(hgt) - 2))
            if (type == "cm") {
                if ((val <= 193) && (val >= 150)) hgt_correct <- TRUE
            } else if (type == "in") {
                if ((val <= 76) && (val >= 59)) hgt_correct <- TRUE
            }
        }
    }
    a[4] <- as.numeric(hgt_correct)

    hcl_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "hcl") {
            hcl <- current[[1]][j + 1]
            if (nchar(hcl) == 7) {
                if (substr(hcl, 1, 1) == "#") {
                    hcl <- substr(hcl, 2, 7)
                    if (grepl("[a-f0-9]+$", hcl) == TRUE) hcl_correct <- TRUE
                }
            }
        }
    }
    a[5] <- as.numeric(hcl_correct)

    ecl_correct <- FALSE
    ecl_opts <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "ecl") {
            ecl <- current[[1]][j + 1]
            if (ecl %in% ecl_opts) ecl_correct <- TRUE
        }
    }
    a[6] <- as.numeric(ecl_correct)

    pid_correct <- FALSE
    for (j in seq_len(length(current[[1]]))) {
        if (current[[1]][j] == "pid") {
            pid <- current[[1]][j + 1]
            if (nchar(pid) == 9) {
                if (grepl("[0-9]+$", pid) == TRUE) pid_correct <- TRUE
            }
        }
    }
    a[7] <- as.numeric(pid_correct)

    if (sum(a) == 7) is_ok[i] <- 1
}

sum(is_ok)