pollutantmean <- function(directory, pollutant, id= 1:332){
  #pollutantmean is a fucntion that computes the mean of the polutant.
  # The function takes three input arguments
  # Input arguments:
  #                 directory: directory where the files are located, a character type variable
  #                 pollutant: pollutant type -- "sulfate", "nitrate", a character type variable
  #                 id: id number, a numeric vector for getting the specific files
  
  ## --------------------------------------------------------------------------------------------
  # start the code here
  # Initiate a loop to get the id name and get corresponding file name
  myData  <- vector()
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
    foo <- data[pollutant] # store specific pollutant data in a foo vector
    pollutantData  <- foo[!is.na(foo)] # remove NA values
    myData <- c(myData, pollutantData) # Get a vector with all the pollutant data
  }
  pollutantmean <- mean(myData) # Get the mean
  print(round(pollutantmean, digit=3)) # print the mean
}
  