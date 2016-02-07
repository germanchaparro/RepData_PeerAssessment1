## use library dplyr for cleaning data purpouses 
library(dplyr)
## use library ggplot2 for ploting results 
library(ggplot2)

# Parameters for reading and ploting the data
activityInputFileName <- "activity.csv"

print("Program started...")

# Check that the file exists in the working directory
print(paste("Reading file:", activityInputFileName, sep = " "))
if(!file.exists(activityInputFileName))
{
  print(paste("Input file", activityInputFileName, "does not exist in working directory: ", getwd(), sep = " "))
  print("Please check...")
  stop()
}


# Reading input file
inputData <- read.csv( activityInputFileName, header = TRUE, 
                       sep = ",", na.strings = "NA")

