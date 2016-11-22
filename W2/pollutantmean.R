pollutantmean <- function(directory, pollutant, id = 1:332) {
        pollu2 <- c()
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
                pollu1 <- data[pollutant][!is.na(data[pollutant])]
                pollu2 <- c(pollu2, pollu1)
        }
        mean(pollu2)
}