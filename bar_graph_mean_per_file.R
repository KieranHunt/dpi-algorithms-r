library('stats')
library('reshape2')
library('ggplot2')

print("Generating bar graph per file");

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

files <- c("51e6a4c8")

data_frame <- read_json_data(files)

plot <- ggplot()

bar_graph_algorithms <- unique(data_frame[c("algorithm")])[,"algorithm"]
bar_graph_input_files_unique <- unique(data_frame[c("inputFile")])[,"inputFile"]
bar_graph_means <- vector()
bar_graph_mins <- vector()
bar_graph_maxes <- vector()
bar_graph_input_files <- vector()

for (inputFile in bar_graph_input_files_unique) {
    inputFile_reduced_vector <- unlist(strsplit(inputFile, "/"), use.names = FALSE)
    inputFile_reduced <- inputFile_reduced_vector[length(inputFile_reduced_vector)]

    for (algorithm in bar_graph_algorithms) {
        data_frame_subset <- data_frame[data_frame[["algorithm"]] == algorithm & data_frame[["inputFile"]] == inputFile, ]
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
        bar_graph_input_files <- c(bar_graph_input_files, inputFile_reduced);
    }
}

bar_graph_data_frame <- data.frame(id = bar_graph_algorithms, bar_graph_mins, bar_graph_means, bar_graph_maxes, bar_graph_input_files)
bar_graph_data_frame <- bar_graph_data_frame[complete.cases(bar_graph_data_frame), ]
names(bar_graph_data_frame) <- c("algorithm", "min", "mean", "max", "inputFile")

bar_graph_data_frame <- bar_graph_data_frame[with(bar_graph_data_frame, order(-mean, algorithm)), ]
rownames(bar_graph_data_frame) <- 1:nrow(bar_graph_data_frame)

plot <- ggplot(bar_graph_data_frame, aes(x = bar_graph_data_frame[["inputFile"]], y = bar_graph_data_frame[["mean"]], fill = algorithm))
plot <- plot + geom_bar(stat = "identity", position = position_dodge())
plot <- plot + geom_errorbar(aes(ymax = bar_graph_data_frame[["max"]], ymin = bar_graph_data_frame[["min"]]), position = position_dodge(), stat = "identity")
plot <- plot + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot <- plot + labs(x = "Input File", y = "Mean Processing Time (ms)", title = "Algorithm Mean Processing Times")
plot <- plot + fte_theme()

ggsave("graphs/bar_graph_mean_per_file.png", dpi=1200, width=10, height=6)
