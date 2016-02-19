library('stats')
library('reshape2')
library('ggplot2')

print("Generating bar graph for one file");

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

input_pre <- "resources/"

command_line_args <- commandArgs(trailingOnly = TRUE)

num_args <- length(command_line_args)

if (num_args < 2) {
    stop("Not enough parameters specified.")
}

files <- command_line_args[2:num_args]
input <- command_line_args[1]

data_frame <- read_json_data(files)

plot <- ggplot()

bar_graph_algorithms <- unique(data_frame[c("algorithm")])[,"algorithm"]
bar_graph_input_files_unique <- unique(data_frame[c("inputFile")])[,"inputFile"]
bar_graph_means <- numeric(length(bar_graph_algorithms))
bar_graph_mins <- numeric(length(bar_graph_algorithms))
bar_graph_maxes <- numeric(length(bar_graph_algorithms))

for (algorithm in bar_graph_algorithms) {
    data_frame_subset <- data_frame[data_frame[["algorithm"]] == algorithm & data_frame[["inputFile"]] == paste(input_pre, input, sep = ""), ]
    elapsed <- data_frame_subset[, "elapsed"] / millis_in_nano

    if (length(elapsed) == 0) {
        min <- NaN
        max <- NaN
    } else {
        min <- min(elapsed)
        max <- max(elapsed)
    }

    mean <- mean(elapsed)

    bar_graph_means <- c(bar_graph_means, mean)
    bar_graph_mins <- c(bar_graph_mins, min)
    bar_graph_maxes <- c(bar_graph_maxes, max)
}

bar_graph_data_frame <- data.frame(id = bar_graph_algorithms, bar_graph_mins, bar_graph_means, bar_graph_maxes)
names(bar_graph_data_frame) <- c("algorithm", "min", "mean", "max")
bar_graph_data_frame <- bar_graph_data_frame[bar_graph_data_frame[["mean"]] != 0, ]

bar_graph_data_frame <- bar_graph_data_frame[with(bar_graph_data_frame, order(-mean, algorithm)), ]
rownames(bar_graph_data_frame) <- 1:nrow(bar_graph_data_frame)

plot <- ggplot(bar_graph_data_frame, aes(x = reorder(algorithm, bar_graph_data_frame[["mean"]]), y = bar_graph_data_frame[["mean"]], fill = algorithm))
plot <- plot + geom_bar(position = "identity", stat = "identity")
plot <- plot + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot <- plot + labs(x = "Algorithm", y = "Mean Processing Time (ms)", title = paste("Algorithm Mean Processing Times for", input))
plot <- plot + fte_theme()
plot <- plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot <- plot + theme(legend.position="none")

ggsave("graphs/bar_graph_mean_one_input.png", dpi = 1200, width = 8, height = 6, type = "cairo")
