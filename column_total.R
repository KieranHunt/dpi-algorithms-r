column_total <- function(data_frame, column_name) {
    sum <- 0

    for (i in 1:nrow(data_frame)) {
        sum <- sum + data_frame[i, column_name]
    }

    return(sum)
}
