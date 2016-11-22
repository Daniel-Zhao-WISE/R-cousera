## This function return the mean of the pollution value of a pollutant for some range of ids
## Data are in the specdata.rar, and all is .csv file

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## variable description
        ## directory is the directory of the files
        ## pollutant can be "sulfate" or "nitrate"
        ## id is the id number of the monitor, and it's also the name of each file in the specdata.rar
        
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
