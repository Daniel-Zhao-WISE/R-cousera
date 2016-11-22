complete <- function(directory, id = 1:332){
        nobs <- c()
        for (item in id) {
                if(item < 10){
                        filepath <- paste(c(directory, "/00", item, ".csv"), collapse = "")
                }else{
                        if(item < 100){
                                filepath <- paste(c(directory, "/0", item, ".csv"), collapse = "")
                        }else{
                                filepath <- paste(c(directory, "/", item, ".csv"), collapse = "")
                        }
                }
                data <- read.csv(filepath)
                nobs1 <- sum((!is.na(data$sulfate)) & (!is.na(data$nitrate)))
                nobs <- c(nobs, nobs1)
        }
        data_frame <- data.frame(id = id, nobs = nobs)
        data_frame
}