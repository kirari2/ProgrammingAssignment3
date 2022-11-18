# R Programming assignment 3
# 4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        ## Read outcome data
    
        ## Check that state and outcome are valid
    
        ## For each state, find the hospital of the given rank
    
        ## Return a date frame with the hosptial names and the (abbrevated) state name
    
}

# Test the function
source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)