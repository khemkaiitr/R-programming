rankFunction  <- function(state, outcome, num="best"){
  # Get the column number according to the disease specified
  if (outcome == "heart attack") numCol  <-  11
  else if (outcome == "heart failure") numCol  <-  17
  else if (outcome == "pneumonia") numCol  <- 23
  
  # get the dataset for your specific state
  stateData  <- state
  stateData  <- stateData[!is.na(as.numeric(stateData[,numCol])),]
  orderedData  <- stateData[order(as.numeric(stateData[,numCol]), stateData[,2]),]
  rankedData  <- data.frame(Hospital.Name=orderedData[,2], Rate = orderedData[,numCol], Rank = rank(as.numeric(orderedData[,numCol]), ties.method ="first"))
  
  if (num == "best") return(as.character(rankedData[[1,1]]))
  else if (num == "worst") return(as.character(rankedData[[nrow(rankedData),1]]))
  else if (num > nrow(rankedData)) return(NA)
  else return(as.character(rankedData[[num,1]]))
}