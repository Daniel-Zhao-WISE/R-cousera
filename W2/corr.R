## This funciton returns the a vector containig correlation of two pollutant's value (sulfate and nitrate) for an id
## if the number of the non-NA observations of that id is larger than the threshold
## Data are in the specdata.rar file

corr <- function(directory, threshold = 0){
        ## Variable description
        ## directory is a string vector of the directory of the data (usually it's "specdata")
        ## threshold is a numerical number which you set as a constriction that only correlation of data whose
        ## number of observations is greater than the threshold will be included in the output vector.
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
