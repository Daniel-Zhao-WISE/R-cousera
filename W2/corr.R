corr <- function(directory, threshold = 0){
        corr <- c()
        for (item in 1:332) {
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
                index <- (!is.na(data$sulfate)) & (!is.na(data$nitrate))
                len <- sum(index)
                if(len > threshold){
                        corr <- c(corr, cor(data$sulfate[index], data$nitrate[index]))
                }
                
        }
        corr
}