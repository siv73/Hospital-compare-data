rankall <- function(outcome , num = "best"){
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE,
                         na.strings = "Not Available")
        valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
        count <- 1
        hospital.name <- NULL
        state.name <- NULL
        
        if(outcome %in% valid.outcomes) {
                if(outcome == "heart attack"){
                        i = 11
                } else if (outcome == "heart failure") {
                        i = 17
                } else {
                        i = 23
                }
                unique.states <- sort(unique(data$State))
               # return(unique.states)
                my_data <- data.frame("State" = data$State,
                                      "Outcome" = data[,i],
                                      "Hospital" = data$Hospital.Name, 
                                      stringsAsFactors = FALSE)
                my_data <- my_data[order(my_data$State, my_data$Outcome, my_data$Hospital),]
                my_data <- na.omit(my_data)
                split.by.state <- split(my_data,my_data$State)
                
               # outcome.state <- split.by.state[[state]]
               
               # return(unique.states)
                #return(num)
                for(i in 1:length(unique.states)){
                
                  if(num == "best"){
                          num <- 1
                  } else  if(num == "worst"){
                          #return(unique.states[i])
                          #return(split.by.state[["WI"]])
                          num <- 0
                          x <- split.by.state[[unique.states[i]]]
                          num <- nrow(x)
                         
                  } 
                  hospital.name[count] <- split.by.state[[unique.states[i]]][num,3]
                  state.name[count] <- unique.states[i]
                  count <- count + 1
                }
                z <- cbind(hospital.name, state.name)
                z <- as.data.frame(z)
                colnames(z) <- c("hospital","state")
                z
               # return(outcome.state[num,3])
                
        } else {
                 if(outcome %in% valid.outcomes != TRUE) {
                        stop("Invalid outcome")
                }
                
        }
}