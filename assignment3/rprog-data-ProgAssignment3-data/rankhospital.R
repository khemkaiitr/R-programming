rankhospital  <- function(state, outcome, num = "best"){
  # Load the data file
  fileName  <- "outcome-of-care-measures.csv"
  outcomeData <- read.csv(fileName, colClasses = "character")
  
  # check the name of state, if not valid return error
  if (sum(outcomeData[,7] == state) == 0) stop("invalid state")
  
  # check the name of the outcome, if not valid, returns error
  validOutcome  <- c("heart attack", "heart failure", "pneumonia")
  if (sum(validOutcome == outcome) == 0) stop("invalid outcome")
  
  # split the dataset in lists separated by statenames
  dataset  <- split(outcomeData, outcomeData[,7])
  
  # Get the column number according to the disease specified
  if (outcome == "heart attack") numCol  <-  11
  else if (outcome == "heart failure") numCol  <-  17
  else if (outcome == "pneumonia") numCol  <- 23
  
  # get the dataset for your specific state
  stateData  <- dataset[[state]]
  stateData  <- stateData[!is.na(as.numeric(stateData[,numCol])),]
  orderedData  <- stateData[order(as.numeric(stateData[,numCol]), stateData[,2]),]
  rankedData  <- data.frame(Hospital.Name=orderedData[,2], Rate = orderedData[,numCol], Rank = rank(as.numeric(orderedData[,numCol]), ties.method ="first"))

  if (num == "best") return(as.character(rankedData[[1,1]]))
  else if (num == "worst") return(as.character(rankedData[[nrow(rankedData),1]]))
  else if (num > nrow(rankedData)) return(NA)
  else return(as.character(rankedData[[num,1]]))
}