rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
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
        outorder <- order(as.numeric(stateout[ , index]), stateout$Hospital.Name, na.last = NA)
        if (num == "best"){
                num <- 1
        }
        if (num == "worst"){
                num <- length(outorder)
        }
        rankname <- stateout[outorder[num], 2]
        return(rankname)
}