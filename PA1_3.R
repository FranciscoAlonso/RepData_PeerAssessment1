PA1_3 <- function()
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
  
  
  allDates <- seq(from = min(steps$date), to = max(steps$date), by = "day")
  
  stepsPerDate <- as.data.frame(allDates, row.names(c("Dates", "Steps")))
  stepcount <- c()
  stepMean <- c()
  stepMedian <- c()
  for(day in allDates)
  {
    temp <- filter(steps, date == day) %>% select(steps) %>% arrange()
    temp2 <- order(temp$steps, decreasing = F)
    if(length(temp$steps) > 0)
    {
      stepcount <- c(stepcount, sum(temp$steps, na.rm = T))
      stepMean <- c(stepMean, mean(temp$steps, na.rm = T))
      stepMedian <- c(stepMedian, myMedian(temp2))
    }
    else
    {
      stepcount <- c(stepcount, 0)
      stepMean <- c(stepMean, 0)
      stepMedian <- c(stepMedian, 0)
    }
  }
  
  stepsPerDate <- mutate(stepsPerDate, Dates = allDates)
  stepsPerDate <- mutate(stepsPerDate, Steps = stepcount)
  stepsPerDate <- mutate(stepsPerDate, Mean = stepMean)
  stepsPerDate <- mutate(stepsPerDate, Median = stepMedian)
  
  
  
  png("figure/P3_Dates-Steps.png")
  plot(stepsPerDate$Dates, stepsPerDate$Steps, type = "l")
  dev.off()
  png("figure/P3_Dates-Mean.png")
  plot(stepsPerDate$Dates, stepsPerDate$Mean, type = "l")
  dev.off()
  png("figure/P3_Dates-Median.png")
  plot(stepsPerDate$Dates, stepsPerDate$Median, type = "l")
  dev.off()
  
  #----
  #Total, mean and median of total number ofsteps per day
  
  #histogram of total number of steps per day
  png("figure/P3_Hist-Steps.png") #create the png graph device
  hist(stepsPerDate$Steps, col = "red")
  dev.off()
  #----
  
}
myMedian <- function(x)
{
  if(length(x)%%2 == 0)
  {
    (x[length(x)/2]+x[length(x)/2+1])/2
  }
  else
  {
    x[as.integer(length(x)/2)+1]
  }
}