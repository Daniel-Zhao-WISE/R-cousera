best <- function(state, outcome) {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!any(state == outcomedata[ , 7])){
                stop("invalid state")
        }
        outname <- c("heart attack", "heart failure", "pneumonia")
        if (!any(outcome == outname)){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        if (outcome == outname[1]){
                index <- 11
        }else{
                if (outcome == outname[2]){
                        index <- 17
                }else{
                        index <- 23
                }
        }
        stateout <- outcomedata[state == outcomedata$State, ]
        bestout <- min(as.numeric(stateout[ , index]), na.rm = TRUE)
        bestname <- stateout[which(as.numeric(stateout[ , index]) == bestout), 2]
        
        ## rate
        if (length(bestname) == 1){
                return(bestname)
        }else{
                return(sort(bestname)[1])
        }
}