# R Programming assignment 3
# 1. Plot the 30-day mortality rates for heart attack

# Read the outcome data into R via the read.csv function
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
dim(outcome)
str(outcome)

# Histogram
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11],
     xlab = "",
     ylim = c(0, 900),
     main = "30-day mortality rates for heart attack")

# 2. Finding the best hospital in a state
# best
best <- function(state, outcome){
    ## Read outcome data
    x <- state
    y <- outcome
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # outcome[, 7]  # State
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
    if (!(x %in% validState == TRUE)) {
        stop(paste("invalid state"))
    }
    validCondition <- c("heart attack", "heart failure", "pneumonia")
    if (!(y %in% validCondition == TRUE)) {
        stop(paste("invalid outcome"))
    }
    ## Return hospital name in that state with lowest 30-day death rate
    temp <- subset(data, State == x)
    if (y == "heart attack") {
        n = 11
    } else if (y == "heart failure") {
        n = 17
    } else if (y == "pneumonia") {
        n = 23
    }
    min <- min((temp[, n]), na.rm = TRUE)
    hosp <- temp[which(temp[, n] == min), 2]
    hosp
}

# Test the function
source("best.R")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")