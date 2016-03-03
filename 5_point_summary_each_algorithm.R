library('stats')
library('ggplot2')

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

command_line_args <- commandArgs(trailingOnly = TRUE)
num_args <- length(command_line_args)

files <- command_line_args[2:num_args]
input <- command_line_args[1]

data_frame <- read_json_data(files)
data_frame <- data_frame[data_frame[["inputFile"]] == input,]

summary_unique_algorithms <- unique(data_frame[c("algorithm")])[,"algorithm"]
number_unique_algorithms <- length(summary_unique_algorithms)

for (i in 1:number_unique_algorithms) {
    unique_algorithm <- as.character(summary_unique_algorithms[i])
    sub_dat_frame <- data_frame[data_frame[["algorithm"]] == unique_algorithm, ]
    print(unique_algorithm)
    print(summary(sub_dat_frame[["elapsed"]] / millis_in_nano))
}
