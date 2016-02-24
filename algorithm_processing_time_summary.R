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

summary_algorithm <- character(number_unique_algorithms)
summary_sd <- numeric(number_unique_algorithms)

for (i in 1:number_unique_algorithms) {
    unique_algorithm <- summary_unique_algorithms[i]
    data_frame_subset <- data_frame[data_frame[["algorithm"]] == unique_algorithm, ]
    times_for_algorithm <- data_frame_subset[, "elapsed"] / millis_in_nano

    summary_algorithm[i] <- as.character(unique_algorithm)
    summary_sd[i] <- sd(times_for_algorithm)
}

summary_data_frame = data.frame(summary_algorithm, summary_sd)
names(summary_data_frame) <- c("algorithm", "sd")

summary_data_frame[["algorithm"]] <-factor(summary_data_frame[["algorithm"]], levels=summary_data_frame[order(summary_data_frame[["sd"]]), "algorithm"])

plot <- ggplot(summary_data_frame, aes(x = algorithm , y = summary_data_frame[["sd"]], fill = algorithm))
plot <- plot + geom_bar(stat = "identity")
plot <- plot + labs(x = "Algorithm", y = "Processing Time Standard Deviation", title = paste("Algorithm Processing Times Standard Deviation for", input))
plot <- plot + fte_theme()
plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot <- plot + theme(legend.position="none")

ggsave("graphs/bar_graph_algorithm_standard_deviation.png", dpi = 1200, width = 8, height = 6, type = "cairo")
