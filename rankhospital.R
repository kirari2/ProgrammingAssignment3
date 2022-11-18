# R Programming assignment 3

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# 3. Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # Heart attack
    data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # Heart failure
    data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # Pneumonia
    
    ## Check that state and outcome are valid
    validState <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
                    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
                    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", 
                    "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VI", "VA", "WA", 
                    "WV", "WI", "WY", "GU")
    if (!(state %in% validState == TRUE)) {
        stop(paste("invalid state"))
    }
    validCondition <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% validCondition == TRUE)) {
        stop(paste("invalid outcome"))
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    temp <- subset(data, State == state)
    if (outcome == "heart attack") {
        n = 11
    } else if (outcome == "heart failure") {
        n = 17
    } else if (outcome == "pneumonia") {
        n = 23
    }
    
    # Sort according to 30-day mortality rate
    ordered <- temp[order(temp[, n], temp[, 2]), ]
    ordered <- na.omit(ordered)
    
    # Settle ties by sorting hospital names
    if (!((num <= nrow(ordered) || (num == "best") || (num == "worst")))) {
        # Suppress error message
        on.exit(options(show.error.messages = FALSE))
    } 
    if (num == "best") {
        num <- 1
    }
    if (num == "worst") {
        num <- nrow(ordered)
    }
    print(ordered[num , 2])
}    

# Test the function
source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
