library('stats')
library('reshape2')
library('ggplot2')

print("");

source("read-json-data.R")
source("fte-theme.R")

millis_in_nano <- 10000000

command_line_args <- commandArgs(trailingOnly = TRUE)

num_args <- length(command_line_args)

if (num_args < 1) {
    stop("Not enough parameters specified.")
}

files <- command_line_args[2:num_args]
unique_algorithms <- unlist(strsplit(command_line_args[1], ":"))

data_frame <- read_json_data(files)

data_frame <- data_frame[data_frame[["found"]] > 0,]

data_frame[["elapsed"]] <- data_frame[["elapsed"]] / millis_in_nano

for (i in 1:length(unique_algorithms)) {
    algorithm <- unique_algorithms[i]
    sub_data_frame <- data_frame[data_frame[["algorithm"]] == algorithm,]

    standard_deviation <- sd(sub_data_frame[["elapsed"]])
    mean_value <- mean(sub_data_frame[["elapsed"]])

    #Remove values above and below one standard deviation.
    sub_data_frame <- sub_data_frame[sub_data_frame[["elapsed"]] < mean_value + standard_deviation, ]
    sub_data_frame <- sub_data_frame[sub_data_frame[["elapsed"]] > mean_value - standard_deviation, ]

    # Note that in the line below the log of the elapsed time has been taken as there is an exponential relationship
    # between elapsed and found.
    fit <- lm(log(sub_data_frame[["elapsed"]]) ~ sub_data_frame[["found"]], data = sub_data_frame)
    print(algorithm)
    print(summary(fit))
    reg_alpha <- (fit[["coefficients"]])[1][["(Intercept)"]]
    reg_beta <- (fit[["coefficients"]])[2][['sub_data_frame[["found"]]']]

    sub_data_frame[["predicted"]] <- predict(fit)

    plot <- ggplot()
    plot <- plot + geom_point(data = sub_data_frame, aes(x = found, y = elapsed, color = -inputLength), alpha=0.50)
    plot <- plot + geom_line(data = sub_data_frame, aes(x = found, y = exp(predicted)))
    plot <- plot + labs(x = "Number of Matches", y = "Processing Time (ms)", title = paste("Processing Time vs Number of Matches for", algorithm))
    plot <- plot + annotate("text", x = 0, y = Inf, vjust = 2, hjust = 0, label = sprintf("f(x) = e^(%f + %fx)", reg_alpha, reg_beta))
    plot <- plot + theme(legend.position="none")

    ggsave(paste("graphs/scatter_elapsed_vs_found_", algorithm, ".png", sep = ""), dpi = 1200, width = 8, height = 6, type = "cairo")
}
