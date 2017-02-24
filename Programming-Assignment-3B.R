



## Part 1 data set read in assignment - 

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Part 2 - Finding the best hospital in the state


best <- function(state, outcome) {
        
## obtain data
        alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
## insure valid state input
        if (!state %in% unique(alldata[, 7])) 
                {stop("invalid state")}

## insure valid outcome input        
        switch(outcome, `heart attack` = {col = 11},
                        `heart failure` = {col = 17},
                        `pneumonia` = {col = 23}, 
                        stop("invalid outcome"))
        
## print hospital with smallest death rate
        hospital_name = alldata[alldata$State == state, c(2, col)]
        hospital_name[which.min(hospital_name[, 2]), 1]
}
## example input ##best("TX", "heart attack")



##Part 3 - Ranking hospitals by outcome in a state
##Write a function called rankhospital that takes three arguments: 
##the 2-character abbreviated name of a state (state), an outcome (outcome), 
##and the ranking of a hospital in that state for that outcome (num). 
##The function reads the outcome-of-care-measures.csv ﬁle and returns a character 
##vector with the name of the hospital that has the ranking speciﬁed by the 
##num argument.


rankhospital <- function(state, outcome, num = "best") {
        
## obtain data
        alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
## insure valid state input
        if (!state %in% unique(alldata[, 7])) 
        {stop("invalid state")}
        
## insure valid outcome input        
        switch(outcome, `heart attack` = {col = 11},
               `heart failure` = {col = 17},
               `pneumonia` = {col = 23}, 
               stop("invalid outcome"))
        

        alldata[, col] = as.numeric(alldata[, col])
        
        hospital_name = alldata[alldata[, 7] == state, c(2, col)]
        hospital_name = na.omit(hospital_name)
        hospital = nrow(hospital_name)
        
        switch(num, best = {num = 1},
                worst = {num = hospital})
        
        if (num > hospital) {return(NA)}
        
## print hospital with input rating
        
        rank_order = order(hospital_name[, 2], hospital_name[, 1])
        hospital_name[rank_order, ][num, 1]
}
## example input: rankhospital("TX", "heart failure", 4)


        
##Part 4 - Ranking hospitals in all states
##Write a function called rankall that takes two arguments: an outcome name 
##(outcome) and a hospital rank- ing (num). The function reads the 
##outcome-of-care-measures.csv file and returns a 2-column data frame 
##containing the hospital in each state that has the ranking specified in num.
        

rankall <- function(outcome, num = "best") {
        
## Obtain data
        alldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       
## insure valid state input
        states = unique(alldata[, 7])
       
        
## insure valid outcome input        
        switch(outcome, `heart attack` = {col = 11},
               `heart failure` = {col = 17},
               `pneumonia` = {col = 23}, 
               stop("invalid outcome"))
        
        
## Print hospital and death rate ranking
        alldata[, col] = as.numeric(alldata[, col])
        alldata = alldata[, c(2, 7, col)]  
        
        
# omit all but name, state, and death rate
        alldata = na.omit(alldata)
    
        
        rank_in_state <- function(state) {
                hospital_name = alldata[alldata[, 2] == state, ]
                hospital = nrow(hospital_name)
                
        switch(num, best = {num = 1},
                worst = {num = hospital})
                
        if (num > hospital) {result = NA}
                rank_order = order(hospital_name[, 3], hospital_name[, 1])
                result = hospital_name[rank_order, ][num, 1]
                c(result, state)
        }
        
        output = do.call(rbind, lapply(states, rank_in_state))
        output = output[order(output[, 2]), ]
        rownames(output) = output[, 2]
        colnames(output) = c("hospital", "state")
        data.frame(output)
}


## example: head(rankall("heart attack", 20), 10)

