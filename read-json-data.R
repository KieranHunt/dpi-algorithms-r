library("rjson")

read_json_data <- function(file_ids) {
    file_prefix= "data/raw-results-"
    file_suffix= ".json"

    print(paste("Looking for", length(file_ids), "files."))

    raw_data <- list()

    for (id in file_ids) {
        file_location = paste(file_prefix, id, file_suffix, sep = "")
        print(paste("Reading JSON from:", file_location))
        raw_data = c(raw_data, fromJSON(file=file_location))
    }

    length_of_data = length(raw_data)

    print(paste("Read in", length_of_data, "objects."))

    ids <- numeric(length_of_data)
    algorithms <- character(length_of_data)
    runIds <- character(length_of_data)
    elapseds <- numeric(length_of_data)
    inputIds <- character(length_of_data)
    inputFiles <- character(length_of_data)
    inputLengths <- numeric(length_of_data)

    for (i in 1:length_of_data) {
        ids[[i]] <- i
        algorithms[[i]] <- raw_data[[i]][["algorithm"]]
        runIds[[i]] <- raw_data[[i]][["runId"]]
        elapseds[[i]] <- raw_data[[i]][["elapsed"]]
        inputIds[[i]] <- raw_data[[i]][["inputID"]]
        inputFiles[[i]] <- raw_data[[i]][["inputFile"]]
        inputLengths[[i]] <- raw_data[[i]][["inputLength"]]
    }

    print(paste("Generating data frame"))

    data_frame = data.frame(id=ids, algorithms, runIds, elapseds, inputIds, inputFiles, inputLengths)
    names(data_frame) <- c("id", "algorithm", "runId", "elapsed", "inputId", "inputFile", "inputLength")

    print(paste("Generated dataframe of length", nrow(data_frame)))

    return(data_frame)
}
