library('stats')
source("read-json-data.R")

command_line_args <- commandArgs(trailingOnly = TRUE)
num_args <- length(command_line_args)

files <- command_line_args[2:num_args]
input <- command_line_args[1]

data_frame <- read_json_data(files)
data_frame <- data_frame[data_frame[["inputFile"]] == input,]

average_input_input_id_unique <- unique(data_frame[c("inputId")])[,"inputId"]
number_unique_input_ids <- length(average_input_input_id_unique)
average_input_lengths <- numeric(number_unique_input_ids)

for (i in 1:number_unique_input_ids) {
    unique_input_id <- average_input_input_id_unique[i]
    data_frame_subset <- data_frame[data_frame[["inputId"]] == unique_input_id, ]

    length_of_input_id <- data_frame_subset[1, "inputLength"]

    average_input_lengths[i] <- length_of_input_id
}

summary(average_input_lengths)
