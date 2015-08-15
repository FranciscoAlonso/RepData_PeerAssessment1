PA1_4 <- function()
{
  library(lubridate)
  library(dplyr)
  steps <- read.csv("activity.csv")
  
  steps$date <- ymd(steps$date)
  
  #number of missing values
  missingValues <- filter(steps, is.na(steps))
  #print(length(missingValues$steps))
  
  stepsNoNA <- na.omit(steps)
  
  #get intervals
  Intervals <- seq(from = min(stepsNoNA$interval), to = max(stepsNoNA$interval), by = 5)
  stepsPerInterval <- as.data.frame(Intervals, row.names(c("Intervals")))
  
  stepMean <- c()
  
  for(i in Intervals)
  {
    temp <- filter(stepsNoNA, interval == i) %>% select(steps) %>% arrange()
    if(length(temp$steps) > 0)
    {
      stepMean <- c(stepMean, mean(temp$steps, na.rm = T))
    }
    else
    {
      stepMean <- c(stepMean, 0)
    }
  }
  
  #steps average per interval
  stepsPerInterval <- mutate(stepsPerInterval, StepsMean = stepMean)
  
  #replace NA values with mean for the corresponding interval
  for(i in 1:length(steps$steps))
  {
    if(is.na(steps$steps[i]))
    {
      intervalMean <- filter(stepsPerInterval, Intervals == steps$interval[i])
      steps$steps[i] <- intervalMean$StepsMean
    }
  }
  
  #missingValues_after <- filter(steps, is.na(steps))
  #print(length(missingValues_after$steps))
  
  
  
}