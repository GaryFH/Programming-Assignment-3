



## Part 1 data set read in assignment - 

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Part 2 - Finding the best hospital in the state


best <- function(state, outcome) {
        
        ## Read the outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (!state %in% unique(dat[, 7])) {
                stop("invalid state")
        }
        switch(outcome, `heart attack` = {
                col = 11
        }, `heart failure` = {
                col = 17
        }, pneumonia = {
                col = 23
        }, stop("invalid outcome"))
        ## Return hospital name in that state with lowest 30-day death rate
        df = dat[dat$State == state, c(2, col)]
        df[which.min(df[, 2]), 1]
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
        
        ## Read the outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (!state %in% unique(dat[, 7])) {
                stop("invalid state")
        }
        switch(outcome, `heart attack` = {
                col = 11
        }, `heart failure` = {
                col = 17
        }, pneumonia = {
                col = 23
        }, stop("invalid outcome"))
        dat[, col] = as.numeric(dat[, col])
        df = dat[dat[, 7] == state, c(2, col)]
        df = na.omit(df)
        nhospital = nrow(df)
        switch(num, best = {
                num = 1
        }, worst = {
                num = nhospital
        })
        if (num > nhospital) {
                return(NA)
        }
        ## Return hospital name in that state with the given rank 30-day death rate
        
        o = order(df[, 2], df[, 1])
        df[o, ][num, 1]
}
## example input: rankhospital("TX", "heart failure", 4)


        
##Part 4 - Ranking hospitals in all states
##Write a function called rankall that takes two arguments: an outcome name 
##(outcome) and a hospital rank- ing (num). The function reads the 
##outcome-of-care-measures.csv file and returns a 2-column data frame 
##containing the hospital in each state that has the ranking specified in num.
        

rankall <- function(outcome, num = "best") {
        ## Read the outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        states = unique(dat[, 7])
        switch(outcome, `heart attack` = {
                col = 11
        }, `heart failure` = {
                col = 17
        }, pneumonia = {
                col = 23
        }, stop("invalid outcome"))
        
        ## Return hospital name in that state with the given rank 30-day death rate
        dat[, col] = as.numeric(dat[, col])
        dat = dat[, c(2, 7, col)]  # leave only name, state, and death rate
        dat = na.omit(dat)
        # head(dat) Hospital.Name State 1 SOUTHEAST ALABAMA MEDICAL CENTER AL 2
        # MARSHALL MEDICAL CENTER SOUTH AL 3 ELIZA COFFEE MEMORIAL HOSPITAL AL 7 ST
        # VINCENT'S EAST AL 8 DEKALB REGIONAL MEDICAL CENTER AL 9 SHELBY BAPTIST
        # MEDICAL CENTER AL
        # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 1 14.3 2 18.5 3
        # 18.1 7 17.7 8 18.0 9 15.9
        rank_in_state <- function(state) {
                df = dat[dat[, 2] == state, ]
                nhospital = nrow(df)
                switch(num, best = {
                        num = 1
                }, worst = {
                        num = nhospital
                })
                if (num > nhospital) {
                        result = NA
                }
                o = order(df[, 3], df[, 1])
                result = df[o, ][num, 1]
                c(result, state)
        }
        output = do.call(rbind, lapply(states, rank_in_state))
        output = output[order(output[, 2]), ]
        rownames(output) = output[, 2]
        colnames(output) = c("hospital", "state")
        data.frame(output)
}
## example: head(rankall("heart attack", 20), 10)

