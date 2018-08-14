best <- function(state, outcome){
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE,
                         na.strings = "Not Available")
        name.cols <- c("State","Outcome","State")
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(state %in% data$State && outcome %in% valid.outcomes) {
                if(outcome == "heart attack"){
                        i = 11
                } else if (outcome == "heart failure") {
                        i = 17
                } else {
                        i = 23
                }
        
               my_data <- data.frame("State" = data$State,
                                     "Outcome" = data[,i],
                                     "Hospital" = data$Hospital.Name, 
                                     stringsAsFactors = FALSE)
               my_data <- my_data[order(my_data$State, my_data$Outcome, my_data$Hospital),]
               my_data <- na.omit(my_data)
               split.by.state <- split(my_data,my_data$State)
               outcome.state <- split.by.state[[state]]
               return(outcome.state[1,3])
              
        } else {
                if(state %in% data$State != TRUE){
                        stop("Invalid state")     
                } else if(outcome %in% valid.outcomes != TRUE) {
                        stop("Invalid outcome")
                }
                
        }
}