corr  <- function(directory, threshold = 0){
  # corr is a fucntion that provides the correlation between the level of nitrate and sulfate provided the number
  # of observations in the given datafile exceeds the threshold.
  # The function takes two input arguments
  # Input arguments:
  #                 directory: directory where the files are located, a character type variable
  #                 threshold: Provide a numeric threshold value, default value is zero
  ## --------------------------------------------------------------------------------------------
  # start the code here
  # Initiate a loop to get the id name and get corresponding file name
  cr  <- vector()
  id  <-  1:332
  for (i in id){
    if (id[i] < 10) {
      temp <- paste("00", id[i],".csv", sep="")
    }
    else if(id[i]>9 & id[i] <100){
      temp  <- paste("0", id[i],".csv", sep ="")
    } else {
      temp  <- paste(id[i], ".csv",sep="")
    }
    fileName  <- file.path(directory, temp)
    
    # Read the .csv file 
    data  <- read.csv(fileName)
    temp  <- data[!is.na(data$sulfate)& !is.na(data$nitrate),1]
    if (length(temp) > threshold){
      sulfatedata  <- data[!is.na(data$sulfate)& !is.na(data$nitrate),2]
      nitratedata  <- data[!is.na(data$sulfate)& !is.na(data$nitrate),3]
      foo  <- cor(sulfatedata,nitratedata )
      cr  <- c(cr, foo)
    }
  }
  print(head(cr))
  print(length(cr))
  return(cr)
}