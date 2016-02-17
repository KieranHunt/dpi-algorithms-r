library("rjson")
library("ggplot2")
require("stats")

file_ids = c("3bb511f1", "5dfec1c2")
file_prefix= "data/raw-results-"
file_suffix= ".json"

raw_data <- list()

for (id in file_ids) {
    file_location = paste(file_prefix, id, file_suffix, sep = "")
    print(paste("Reading JSON from:", file_location))
    raw_data = c(raw_data, fromJSON(file=file_location))
}

length_of_data = length(raw_data)

print(paste("Read in", length_of_data, "objects."))

algorithm_names = list()

for (item in raw_data) {
    algorithm_name <- item[["algorithm"]]
    exists = 0;
    for (name in algorithm_names) {
        if (algorithm_name == name) {
            exists = 1;
        }
    }
    if (exists == 0) {
        algorithm_names[[length(algorithm_names) + 1]] <- algorithm_name
    }
}

print(paste("Found", length(algorithm_names), "algorithms."))

ids <- c()
algorithms <- c()
runIds <- c()
elapseds <- c()
inputIds <- c()
inputFiles <- c()

for (i in 1:length_of_data) {
    ids[[i]] <- i
    algorithms[[i]] <- raw_data[[i]][["algorithm"]]
    runIds[[i]] <- raw_data[[i]][["runId"]]
    elapseds[[i]] <- raw_data[[i]][["elapsed"]]
    inputIds[[i]] <- raw_data[[i]][["inputID"]]
    inputFiles[[i]] <- raw_data[[i]][["inputFile"]]
}

data_frame = data.frame(id=ids, algorithms, runIds, elapseds, inputIds, inputFiles)
names(data_frame) <- c("id", "algorithm", "runId", "elapsed", "inputId", "inputFile")

# by(data_frame[, "elapsed"], data_frame[, "algorithm"], summary)["BoyerMoore"]
#
# # g <- ggplot(data_frame, aes(algorithm, mean(elapsed))) + geom_bar()
# #
# # ggsave("bar.png", dpi=1200, width=5, height=3)
