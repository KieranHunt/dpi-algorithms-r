library('stats')
library('reshape2')
library('ggplot2')

print("Generating Scatter Plot for mean processing time vs file size");

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

command_line_args <- commandArgs(trailingOnly = TRUE)

num_args <- length(command_line_args)

if (num_args < 2) {
    stop("Not enough parameters specified.")
}

files <- command_line_args[2:num_args]
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
unique_algorithms <- unique(c(as.character(data_frame[["algorithm"]])))
number_unique_algorithms <- length(unique_algorithms)

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

    for (j in 1:number_unique_ids) {
        count <- count + 1
        unique_input_id <- unique_input_ids[j]

        data_frame_subset <- data_frame[data_frame[["inputId"]] == unique_input_id, ]
        data_frame_subset <- data_frame_subset[data_frame_subset[["algorithm"]] == unique_algorithm, ]
        length_subset <- nrow(data_frame_subset)

        if (length_subset == 0) {
            next
        }

        mean <- mean(data_frame_subset[, "elapsed"]) / millis_in_nano
        length <- data_frame_subset[1, "inputLength"]

        scatter_means[count] <- mean;
        scatter_lengths[count] <- length
        scatter_ids[count] <- unique_input_id
        scatter_algorithms[count] <- unique_algorithm
    }
}

scatter_data_frame = data.frame(scatter_ids, scatter_algorithms, scatter_means, scatter_lengths)
names(scatter_data_frame) <- c("inputId", "algorithm", "mean", "length")

scatter_data_frame <- scatter_data_frame[complete.cases(scatter_data_frame), ]

number_of_points <- nrow(scatter_data_frame)

print(paste("Generated", number_of_points, "datapoints."))

print("Generating Scatter Plot")

plot <- ggplot(scatter_data_frame)
plot <- plot + geom_point(aes(x = length, y = mean, color = algorithm), alpha=0.50)
plot <- plot + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot <- plot + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot <- plot + labs(x = "Length of Input", y = "Mean Processing Time (ms)", title = paste("Mean Processing Time per Algorithm vs Input Length for", input_file_name_full))
plot <- plot + fte_theme()

ggsave("graphs/scatter_mean_time_each_algorithm_vs_input_length.png", dpi = 1200, width = 8, height = 6, type = "cairo")

print("Done.")
