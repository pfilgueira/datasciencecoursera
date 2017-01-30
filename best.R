best <- function(state, outcome) {
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

        ## Check that state and outcome are valid
        if (!state %in% state_vector) {
                stop("Invalid state.")    
        }
        
        if (!outcome %in% outcome_vector) {
                stop("Invalid outcome.")    
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ## Select rows for the requested state and rename the column Deaths
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
        
        ## Prepare the output and sort death and hospitals in that order
        outcome_data_state[, 2] = suppressWarnings( as.numeric(outcome_data_state[, 2]) )
        outcome_data_state = outcome_data_state[!is.na(outcome_data_state$Deaths),]
        outcome_data_state = outcome_data_state[order(outcome_data_state$Deaths, outcome_data_state$Hospital.Name),]
        
        return (outcome_data_state$Hospital.Name[1])
        
}
