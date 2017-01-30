rankall <- function(outcome, num = "best") {
        ## Read outcome data
        setwd("~/Dropbox/Pessoal/Especializacao/Data Science/ProgrammingAssignment3")
        outcome_data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character",
                                 na.strings = "Not Available",
                                 stringsAsFactors = FALSE)
        
        ## Create vector to store the possible unique and valid hospitals
        hospital_vector <- outcome_data[, 2]
        hospital_vector <- unique(hospital_vector[!is.na(hospital_vector)])
        
        ## Create vector to store the possible outcomes
        outcome_vector <- c("heart attack", "heart failure", "pneumonia") 
        
        if (!outcome %in% outcome_vector) {
                stop("Invalid outcome.")    
        }        
        
        if(class(num) == "character"){
                if (! (num == "best" || num == "worst")){
                        stop("invalid number")
                }
        }
        
        ## For each state, find the hospital of the given rank
        if (outcome == "heart attack") {
                outcome_data = outcome_data[, c(2, 7, 11)]
        }
        if (outcome == "heart failure") {
                outcome_data = outcome_data[, c(2, 7, 17)]
        }
        if (outcome == "pneumonia") {
                outcome_data = outcome_data[, c(2, 7, 23)]
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        names(outcome_data)[3] = "Deaths"
        outcome_data[, 3] = suppressWarnings( as.numeric(outcome_data[, 3]) )
        ### remove invalid rows
        outcome_data = outcome_data[!is.na(outcome_data$Deaths),]
        
        ### split the data
        outcome_splitted = split(outcome_data, outcome_data$State)
        outcome_rank = lapply(outcome_splitted, function(x, num) {
        
        # Order results - Death and Hospital
        x = x[order(x$Deaths, x$Hospital.Name),]
                
        # Return
        if(class(num) == "character") {
                if(num == "best") {
                        return (x$Hospital.Name[1])
                }
                else if(num == "worst") {
                        return (x$Hospital.Name[nrow(x)])
                }
        }
        else {
                return (x$Hospital.Name[num])
        }
        }, num)
        
        #Return data.frame with format
        return ( data.frame(hospital=unlist(outcome_rank), state=names(outcome_rank)) )
        
        
        
        
}