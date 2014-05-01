bestHospital  <- function(state, outcome){
  
  # Read the csv file
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # check the name of state, if not valid return error
  if (sum(outcomeData[,7] == state) == 0) stop("invalid state")
  
  # check the name of the outcome, if not valid, returns error
  validOutcome  <- c("heart attack", "heart failure", "pneumonia")
  if (sum(validOutcome == outcome) == 0) stop("invalid outcome")
  
  # if the arguments are valid, check for the best hospital
  dataset  <- split(outcomeData, outcomeData[,7])
  if (outcome == "heart attack"){
    stateData  <- dataset[[state]]
    ndata  <- stateData[order(as.numeric(stateData[,11]), stateData[,2]),]
    print(ndata[1,2])
  }
  
  # if the arguments are valid, check for the best hospital
  if (outcome == "heart failure"){ 
    stateData  <- dataset[[state]]
    ndata  <<- stateData[order(as.numeric(stateData[,17]), stateData[,2]),]
    print(ndata[1,2])
  }
  
  # if the arguments are valid, check for the best hospital
  if (outcome == "pneumonia"){
    stateData  <- dataset[[state]]
    ndata  <- stateData[order(as.numeric(stateData[,23]), stateData[,2]),]
    print(ndata[1,2])
  }
}