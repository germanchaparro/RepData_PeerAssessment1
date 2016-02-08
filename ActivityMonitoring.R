## use library dplyr for cleaning data purpouses 
library(dplyr)
## use library ggplot2 for ploting results 
library(ggplot2)
## use library lubridate for managing date/times 
library(lubridate)
## use library chron for is.weekend function 
library(chron)


charMinutes_to_Time <- function(charMinutes)
{
  if(nchar(charMinutes) == 1)
  {
    paste("0:0", charMinutes, sep = "")
  }
  else if(nchar(charMinutes) == 2)
  {
    paste("0:", charMinutes, sep = "")
  }
  else if(nchar(charMinutes) == 3)
  {
    paste("0", substr(charMinutes, 1, 1), ":", substr(charMinutes, 2, 3), sep = "")
  }
  else if(nchar(charMinutes) == 4)
  {
    paste(substr(charMinutes, 1, 2), ":", substr(charMinutes, 2, 3), sep = "")
  }
}


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

# With dplyr get the total, mean and median of steps by date
point1 <-
  inputData %>%
  group_by(date) %>%
  summarize(totalSteps = sum(steps, na.rm = TRUE),
            dataMean = mean(steps, na.rm = TRUE),
            dataMedian = median(steps, na.rm = TRUE)) %>%
  arrange(date)

# Plot the histogram of the total number of steps taken each day
p1_1 <-
  ggplot ( data = point1, aes(totalSteps) ) +
  geom_histogram( bins = 40, col = "black", fill = "steelblue" ) +
  labs(x = "Steps") +  
  labs(y = "Frequency") +
  labs(title = "Total number of steps per day") 
print(p1_1)

# Plot the the mean and median of the total number of steps taken per day
p1_2 <-
  ggplot( data = point1, aes(x = date, y = dataMean, group = 1) ) +
  geom_line(col="steelblue") + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Day") +  
  labs(y = "Mean of number of steps") +
  labs(title = "Mean number of steps per day") 
print(p1_2)

p1_3 <-
  ggplot( data = inputData, aes(x = factor(date), y = steps)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Day") +  
  labs(y = "Median of number of steps") +
  labs(title = "Median of number of steps per day") 
print(p1_3)
 

#g <-
#  ggplot ( data = point1, aes(x = date, y = totalSteps) ) + 
#  geom_bar(stat = "identity", fill="steelblue", na.rm = TRUE) +
#  theme_bw(base_family = "Avenir", base_size = 10) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#print(g)


#g <- ggplot( data = inputData, aes(x = date, y = steps) ) +
#  geom_boxplot(na.rm = TRUE) + 
#  theme_bw(base_family = "Avenir", base_size = 10) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#print(g)





# What is the average daily activity pattern?
avgDayActDF <-
  inputData %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarize(dataMean = mean(steps, na.rm = TRUE)) %>%
  arrange(interval)

avgDayActPlot <- 
  ggplot ( data = avgDayActDF, aes(x = interval, y = dataMean) ) +
  geom_line() +
  labs(x = "Time interval") +  
  labs(y = "Mean of number of steps") +
  labs(title = "Mean of number of steps per time interval") 
print(avgDayActPlot)

avgMaxNumSteps <-
  avgDayActDF %>%
  select(interval, dataMean) %>%
  filter(dataMean == max(dataMean, na.rm = TRUE))

print(avgMaxNumSteps)

## Imputing missing values


table(is.na(inputData$steps))

## Are there differences in activity patterns between weekdays and weekends?

newInputData <-
  inputData %>%
  mutate(isWeekend = factor(is.weekend(ymd(date)), labels=c("WEEKDAY", "WEEKEND"))  ) %>%
  group_by(interval, isWeekend) %>%
  summarize(meanSteps = mean(steps, na.rm = TRUE))

weekdayPlot <-
  ggplot ( data = newInputData, aes(x = interval, y = meanSteps) ) +
  geom_line(col="steelblue") +
  facet_grid(isWeekend ~ .) +
  labs(x = "Time interval") +  
  labs(y = "Mean of number of steps") +
  labs(title = "Comparisson of the mean of number of steps per time interval during weekends and weekdays") 
print(weekdayPlot)

  
  