complete <- function(directory, id=1:332){
  # complete is a fucntion that provides the complete case of the provided file
  # The function takes two input arguments
  # Input arguments:
  #                 directory: directory where the files are located, a character type variable
  #                 id: id number, a numeric vector for getting the specific files
  
  ## --------------------------------------------------------------------------------------------
  # start the code here
  # Initiate a loop to get the id name and get corresponding file name
  dataId  <- vector()
  nobs  <- vector()
  for (i in 1:length(id)){
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
    # get no of complete onservations
    foo  <- data[!is.na(data$sulfate)& !is.na(data$nitrate),1]
    # get the data vectors
    dataId = c(dataId, id[i])
    nobs = c(nobs, length(foo))
  }
  myData  <- data.frame(id = dataId, nobs = nobs)
  print(myData)
}