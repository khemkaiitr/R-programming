best  <- function(state, outcome){
  
  # Read the csv file
  fileName  <- "outcome-of-care-measures.csv"
  outcomeData <- read.csv(fileName, colClasses = "character")
  
  # check the name of state, if not valid return error
  nameState  <- outcomeData[,7]
  if (sum(nameState == state) == 0){
    stop("invalid state")
  }
  
  # check the name of the outcome, if not valid, returns error
  validOutcome  <- c("heart attack", "heart failure", "pneumonia")
  if (sum(validOutcome == outcome) == 0){
    stop("invalid outcome")
  }
  
  # if the arguments are valid, check for the best hospital
  if (outcome == "heart attack"){
    state_data  <- outcomeData[outcomeData[,7]==state,]
    mortality_rate  <- as.numeric(state_data[,11]);
    formated_data  <<- state_data[!is.na(mortality_rate),]
    temp  <<- sort(as.numeric(formated_data[,11]), decreasing=FALSE, index.return = TRUE)
    sorted_mortValue  <<- temp$x
    get_rep  <<- rle(sorted_mortValue)
    if (get_rep[[1]][1] >1){
      sameMort_stateName  <- formated_data[formated_data[,11]==get_rep[[2]][3],2]
      best_hospital <- sort(sameMort_stateName)
      return(best_hospital[1])
    }
    else{
      return(formated_data[temp$ix[1],2])
    }
  }
  
  # if the arguments are valid, check for the best hospital
  if (outcome == "heart failure"){
    state_data  <- outcomeData[outcomeData[,7]==state,]
    mortality_rate  <- as.numeric(state_data[,17]);
    formated_data  <<- state_data[!is.na(mortality_rate),]
    temp  <<- sort(as.numeric(formated_data[,17]), decreasing=FALSE, index.return = TRUE)
    sorted_mortValue  <<- temp$x
    get_rep  <<- rle(sorted_mortValue)
    if (get_rep[[1]][1] >1){
      sameMort_stateName  <- formated_data[as.numeric(formated_data[,17])==get_rep[[2]][1],2]
      best_hospital <- sort(sameMort_stateName)
      return(best_hospital[1])
    }
    else{
      return(formated_data[temp$ix[1],2])
    }
  }
  
  # if the arguments are valid, check for the best hospital
  if (outcome == "pneumonia"){
    state_data  <- outcomeData[outcomeData[,7]==state,]
    mortality_rate  <- as.numeric(state_data[,23]);
    formated_data  <<- state_data[!is.na(mortality_rate),]
    temp  <<- sort(as.numeric(formated_data[,23]), decreasing=FALSE, index.return = TRUE)
    sorted_mortValue  <<- temp$x
    get_rep  <<- rle(sorted_mortValue)
    if (get_rep[[1]][1] >1){
      sameMort_stateName  <- formated_data[as.numeric(formated_data[,23])==get_rep[[2]][1],2]
      best_hospital <- sort(sameMort_stateName)
      return(best_hospital[1])
    }
    else{
      return(formated_data[temp$ix[1],2])
    }
  }
  
}