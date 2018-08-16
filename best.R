##Short -description of what the below function does
##The below code will read the data from the excel provided in this
##git repo and then it will rank the best hospital corresponding to
##the particular disease and state. Best hospitals are chosen on the
##basis of hospitals with low mortality in the period for which the
## data is collected. In the event of tie, it will follow the 
##order as per English alphabet.

best <- function(state, outcome){
##reading the data from outcome-of-care-measures file and converting missing values to NA.
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE,
                         na.strings = "Not Available")

        ##name.cols <- c("State","Outcome","State")
##these are the only 3 valid diesease names.
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")

##validating whether the entered state and outcome belong to the data
##in the data frame.        
        if(state %in% data$State && outcome %in% valid.outcomes) {
##setting the value of i to specific column number in the dataframe
                if(outcome == "heart attack"){
                        i = 11
                } else if (outcome == "heart failure") {
                        i = 17
                } else {
                        i = 23
                }
##fetching only the relevant data to a new dataframe    
               my_data <- data.frame("State" = data$State,
                                     "Outcome" = data[,i],
                                     "Hospital" = data$Hospital.Name, 
                                     stringsAsFactors = FALSE)
##ordering the resultant data frame based on State then outcome and then hospital name
               my_data <- my_data[order(my_data$State, my_data$Outcome, my_data$Hospital),]
##omiting all na values from the above dataframe
               my_data <- na.omit(my_data)
##the below line will result in a list with sublists for each state
               split.by.state <- split(my_data,my_data$State)
##based users input taking the result for particular state
               outcome.state <- split.by.state[[state]]
##returing the first row as result
               return(outcome.state[1,3])
              
        } else {
##if invalid state is entered
                if(state %in% data$State != TRUE){
                        stop("Invalid state")     
## if invalid outcome is entered
                } else if(outcome %in% valid.outcomes != TRUE) {
                        stop("Invalid outcome")
                }
                
        }
}
