rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        setwd("~/Dropbox/Pessoal/Especializacao/Data Science/ProgrammingAssignment3")
        outcome_data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character",
                                 na.strings = "Not Available",
                                 stringsAsFactors = FALSE)
        
        ## Create vector to store the possible unique and valid states
        state_vector <- outcome_data[, 7]
        state_vector <- unique(state_vector[!is.na(state_vector)])
        
        ## Create vector to store the possible unique and valid hospitals
        hospital_vector <- outcome_data[, 2]
        hospital_vector <- unique(hospital_vector[!is.na(hospital_vector)])
        
        ## Create vector to store the possible outcomes
        outcome_vector <- c("heart attack", "heart failure", "pneumonia") 
        
        ## Check whether the input parameters are valid
        if (!state %in% state_vector) {
                stop("Invalid state.")    
        }
        
        if (!outcome %in% outcome_vector) {
                stop("Invalid outcome.")    
        }        
        
        if(class(num) == "character"){
                if (! (num == "best" || num == "worst")){
                        stop("invalid number")
                }
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        outcome_data_state = outcome_data[outcome_data$State == state,]
        
        if (outcome == "heart attack") {
                outcome_data_state = outcome_data_state[, c(2, 11)]
        }
        if (outcome == "heart failure") {
                outcome_data_state = outcome_data_state[, c(2, 17)]
        }
        if (outcome == "pneumonia") {
                outcome_data_state = outcome_data_state[, c(2, 23)]
        }
        
        names(outcome_data_state)[2] = "Deaths"
        outcome_data_state[, 2] = suppressWarnings( as.numeric(outcome_data_state[, 2]) )
        outcome_data_state = outcome_data_state[!is.na(outcome_data_state$Deaths),]
        
        # Returns NA depending on the number of valid entries
        if(class(num) == "numeric" && num > nrow(outcome_data_state)){
                return (NA)
        }
        
        outcome_data_state = outcome_data_state[order(outcome_data_state$Deaths, outcome_data_state$Hospital.Name),]
        
        if(class(num) == "character") {
                if(num == "best") {
                        return (outcome_data_state$Hospital.Name[1])
                }
                else if(num == "worst") {
                        return (outcome_data_state$Hospital.Name[nrow(outcome_data_state)])
                }
        }
        else {
                return (outcome_data_state$Hospital.Name[num])
        }
}