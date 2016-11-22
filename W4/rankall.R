rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        State <- names(split(outcomedata, outcomedata$State))
        
        ## Check that outcome is valid
        outname <- c("heart attack", "heart failure", "pneumonia")
        if (!any(outcome == outname)){
                stop("invalid outcome")
        }
        
        ## For each state, find the hospital of the given rank
        if (outcome == outname[1]){
                index <- 11
        }else{
                if (outcome == outname[2]){
                        index <- 17
                }else{
                        index <- 23
                }
        }
        if (num == "best"){
                num <- 1
        }
        hospital <- c()
        for (state in State) {
                stateout <- outcomedata[state == outcomedata$State, ]
                outorder <- order(as.numeric(stateout[ , index]), stateout$Hospital.Name, na.last = NA)
                if (num == "worst"){
                        rankname <- stateout[outorder[length(outorder)], 2]
                }else {
                        rankname <- stateout[outorder[num], 2] 
                }
                hospital <- c(hospital, rankname)
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        data.frame(hospital = hospital, state = State)
}
