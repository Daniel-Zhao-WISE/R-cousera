## This function returns a dataframe of two columns which contains the id and its non-Na observation's number
complete <- function(directory, id = 1:332){
        ## Variable description
        ## direcotry is a sting whihc is thd directory of the data, usually it's "specdata"
        ## id is the id of the monitor, and it's also the name of each file in specdata.rar
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
