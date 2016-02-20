library('stats')
library('reshape2')
library('ggplot2')

print("Generating line graph of mean processing time per algorithm and runId vs input length");

source("read-json-data.R")
source("column_total.R")
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

unique_input_lengths <- unique(c(as.character(data_frame[["inputLength"]])))
number_unique_input_lengths <- length(unique_input_lengths)
unique_algorithms <- unique(c(as.character(data_frame[["algorithm"]])))
number_unique_algorithms <- length(unique_algorithms)
unique_runIds <- unique(c(as.character(data_frame[["runId"]])))
number_unique_runIds <- length(unique_runIds)

print(paste("Found", number_unique_input_lengths, "unique input lengths. Found", number_unique_algorithms, "unique algorithms. Found", number_unique_runIds, "unique run IDs"))

number_line_points <- number_unique_input_lengths * number_unique_algorithms * number_unique_runIds

print(paste("That makes", number_line_points, "line points."))

line_data_frame <- expand.grid(unique_input_lengths, unique_algorithms, unique_runIds)
names(line_data_frame) <- c("inputLength", "algorithm", "runId")

length_line_data_frame <- nrow(line_data_frame)

line_means <- numeric(length_line_data_frame)

for (i in 1:length_line_data_frame) {
    data_frame_subset <- data_frame[data_frame[["inputLength"]] == line_data_frame[i, "inputLength"], ]
    data_frame_subset <- data_frame_subset[data_frame_subset[["algorithm"]] == line_data_frame[i, "algorithm"], ]
    data_frame_subset <- data_frame_subset[data_frame_subset[["runId"]] == line_data_frame[i, "runId"], ]

    mean <- mean(data_frame_subset[, "elapsed"])

    line_data_frame[i, "mean"] <- mean / millis_in_nano
}

print("Sorting by input length")

line_data_frame[["inputLength"]] <- as.numeric(paste(line_data_frame[["inputLength"]]))
line_data_frame <- line_data_frame[with(line_data_frame, order(inputLength)), ]
rownames(line_data_frame) <- 1:nrow(line_data_frame)

print("Working out cumulative sums")

for (i in 1:length_line_data_frame) {
    data_frame_subset <- line_data_frame[which(line_data_frame[["runId"]] == line_data_frame[i, "runId"]), ]
    data_frame_subset <- data_frame_subset[which(data_frame_subset[["algorithm"]] == line_data_frame[i, "algorithm"]), ]
    data_frame_subset <- data_frame_subset[which(as.numeric(rownames(data_frame_subset)) <= i), ]

    cumsum <- column_total(data_frame_subset, "mean")

    line_data_frame[i, "cumsum"] <- cumsum
}


print("Generating Line Plot")

plot <- ggplot(line_data_frame)
plot <- plot + geom_line(aes(x = inputLength, y = cumsum, group = interaction(runId, algorithm), color = algorithm, shape = runId), stat = "identity")
plot <- plot + labs(x = "Length of Input", y = "Mean Processing Time (ms)", title = paste("Mean Processing Time per Algorithm and Run ID vs Input Length for", input_file_name_full))
plot <- plot + fte_theme()

ggsave("graphs/line_mean_processing_time_per_algorithm_and_run_id_vs_input_length.png", dpi = 1200, width = 8, height = 6, type = "cairo")

print("Done.")
