library('stats')
library('reshape2')
library('ggplot2')

print("Working out correlation between algorithm processing speed and input length");

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

command_line_args <- commandArgs(trailingOnly = TRUE)

num_args <- length(command_line_args)

if (num_args < 3) {
    stop("Not enough parameters specified.")
}

files <- command_line_args[3:num_args]
input_file_name_full <- command_line_args[1]

data_frame <- read_json_data(files)
data_frame <- data_frame[data_frame[["inputFile"]] == input_file_name_full,]

unique_file_names <- unique(c(as.character(data_frame[["inputFile"]])))

print(paste("Found", length(unique_file_names), "unique file names."))

if (input_file_name_full %in% unique_file_names) {
    print(paste("Data contains items with", input_file_name_full, "file name"))
} else {
    stop(paste("Data does not contain any items with", input_file_name_full, "file name"))
}

unique_input_ids <- unique(c(as.character(data_frame[["inputId"]])))
number_unique_ids <- length(unique_input_ids)
unique_algorithms <- unlist(strsplit(command_line_args[2], ":"))
number_unique_algorithms <- length(unique_algorithms)

print(unique_algorithms)

print(paste("Found", number_unique_ids, "unique IDs. Found", number_unique_algorithms, "unique algorithms."))

number_scatter_points <- number_unique_ids * number_unique_algorithms

print(paste("That makes", number_scatter_points, "unique combinations."))

scatter_means <- numeric(number_scatter_points)
scatter_lengths <- numeric(number_scatter_points)
scatter_ids <- character(number_scatter_points)
scatter_algorithms <- character(number_scatter_points)

count <- 0

for (i in 1:number_unique_algorithms) {
    unique_algorithm <- unique_algorithms[i]
    data_frame_subset <- data_frame[data_frame[["algorithm"]] == unique_algorithm, ]

    for (j in 1:number_unique_ids) {
        count <- count + 1
        print(paste(count, "/", number_scatter_points))
        unique_input_id <- unique_input_ids[j]

        data_frame_subset_subset <- data_frame_subset[data_frame_subset[["inputId"]] == unique_input_id, ]
        length_subset <- nrow(data_frame_subset_subset)

        if (length_subset == 0) {
            next
        }

        mean <- mean(data_frame_subset_subset[, "elapsed"]) / millis_in_nano
        length <- data_frame_subset_subset[1, "inputLength"]

        scatter_means[count] <- mean;
        scatter_lengths[count] <- length
        scatter_ids[count] <- unique_input_id
        scatter_algorithms[count] <- unique_algorithm
    }
}

scatter_data_frame = data.frame(scatter_ids, scatter_algorithms, scatter_means, scatter_lengths)
names(scatter_data_frame) <- c("inputId", "algorithm", "mean", "length")

scatter_data_frame <- scatter_data_frame[complete.cases(scatter_data_frame), ]

for (i in 1:number_unique_algorithms) {
    unique_algorithm <- unique_algorithms[i]
    data_frame_subset <- data_frame[data_frame[["algorithm"]] == unique_algorithm, ]

    print(unique_algorithm)
    print(cor(data_frame_subset[sapply(data_frame_subset, is.numeric)]))
}
