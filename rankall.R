# R Programming assignment 3
# 4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # Heart attack
    data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # Heart failure
    data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # Pneumonia
    
    ## Check that outcome are valid
    validCondition <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% validCondition == TRUE)) {
        stop(paste("invalid outcome"))
    }
    if (outcome == "heart attack") {
        n = 11
    } else if (outcome == "heart failure") {
        n = 17
    } else if (outcome == "pneumonia") {
        n = 23
    }
    
    ## For each state, find the hospital of the given rank
    validState <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL",
                    "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND",
                    "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA",
                    "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT",
                    "WA", "WI", "WV", "WY")    
    
    output <- matrix(ncol = 2, nrow = length(validState))
    for (i in seq_along(validState)) {
        temp <- subset(data, State == validState[i])
        ordered <- temp[order(temp[, n], temp[, 2]), ]
        # ordered <- na.omit(ordered) (Do not need this line for correct answer)
        z <- num
        
        if (num == "best") {
            z <- 1
        }
        if (num == "worst") {
            # Need to assign a different variable to represent num in the loop
            z <- length(na.omit(ordered[, n]))
        }
        
        output[i, 1] <- ordered[z, 2]
        output[i, 2] <- validState[i]
    }
    
    ## Return a date frame with the hospital names and the (abbreviated) state name
    colnames(output) <- c("hospital", "state")
    rownames(output) <- validState
    # output save as data.frame for quiz questions
    output <- as.data.frame(output)
}

# Test the function
source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)