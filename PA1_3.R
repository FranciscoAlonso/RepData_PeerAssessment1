PA1_3 <- function()
{
  library(lubridate)
  library(dplyr)
  steps <- read.csv("activity.csv", na.strings = "NA")
  
  steps$date <- ymd(steps$date)
  
  #number of missing values
  missingValues <- filter(steps, is.na(steps))
  print("Nmber of missing values (NA):")
  print(length(missingValues$steps))
  
  
  #stepsNoNA <- na.omit(steps)
  stepsNoNA <- steps[complete.cases(steps),]

  allDates <- seq(from = min(steps$date), to = max(steps$date), by = "day")
  stepsPerDate_NoNA <- as.data.frame(allDates, row.names(c("Dates", "Steps")))
  stepcount_NoNA <- c()
  stepMean_NoNA <- c()
  stepMedian_NoNA <- c()
  for(day in allDates)
  {
    temp <- filter(stepsNoNA, date == day) %>% select(steps)
    #temp2 <- order(temp$steps, decreasing = F)
    if(length(temp$steps) > 0)
    {
      stepcount_NoNA <- c(stepcount_NoNA, sum(temp$steps, na.rm = T))
      stepMean_NoNA <- c(stepMean_NoNA, mean(temp$steps, na.rm = T))
      stepMedian_NoNA <- c(stepMedian_NoNA, median(temp$steps, na.rm = T))
    }
    else
    {
      stepcount_NoNA <- c(stepcount_NoNA, 0)
      stepMean_NoNA <- c(stepMean_NoNA, 0)
      stepMedian_NoNA <- c(stepMedian_NoNA, 0)
    }
  }
  
  stepsPerDate_NoNA <- mutate(stepsPerDate_NoNA, Dates = allDates)
  stepsPerDate_NoNA <- mutate(stepsPerDate_NoNA, Steps = stepcount_NoNA)
  stepsPerDate_NoNA <- mutate(stepsPerDate_NoNA, Mean = stepMean_NoNA)
  stepsPerDate_NoNA <- mutate(stepsPerDate_NoNA, Median = stepMedian_NoNA)
  write.csv(stepsPerDate_NoNA, "VALUES.csv")
  #print(missingValues <- filter(stepsNoNA, is.na(steps)))
  
  #get intervals
  Intervals <- seq(from = min(stepsNoNA$interval), to = max(stepsNoNA$interval), by = 5)
  stepsPerInterval <- as.data.frame(Intervals, row.names(c("Intervals")))
  
  stepMean <- c()
  
  for(i in Intervals)
  {
    temp <- filter(stepsNoNA, interval == i) %>% select(steps) 
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
    temp <- filter(steps, date == day) %>% select(steps)
    temp2 <- order(temp$steps, decreasing = F)
    if(length(temp$steps) > 0)
    {
      stepcount <- c(stepcount, sum(temp$steps, na.rm = T))
      stepMean <- c(stepMean, mean(temp$steps, na.rm = T))
      stepMedian <- c(stepMedian, median(temp$steps, na.rm = T))
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
  write.csv(stepsPerDate, "VALUES2.csv")
  par(mfrow = c(2,1))
  hist(stepsPerDate_NoNA$Steps
       , col = "red"
       , main = "Histogram of the total number of steps taken each day (NA removed)"
       , xlab = "Number of steps per day"
       , ylim = c(0,30))
  
  hist(stepsPerDate$Steps
       , col = "red"
       , main = "Histogram of the total number of steps taken each day (NA filled)"
       , xlab = "Number of steps per day"
       , ylim = c(0,30))
  par(mfrow = c(2,1))
#   png("figure/P3_Dates-Steps.png")
#   plot(stepsPerDate$Dates, stepsPerDate$Steps, type = "l")
#   dev.off()
#   png("figure/P3_Dates-Mean.png")
   plot(stepsPerDate_NoNA$Dates, stepsPerDate_NoNA$Mean
        , type = "l"       
        , main = "Mean of total number of steps per date (NA removed)"
        , xlab = "Dates"
        , ylab = "Mean")
   plot(stepsPerDate$Dates, stepsPerDate$Mean
        , type = "l"       
        , main = "Mean of total number of steps per date (NA filled)"
        , xlab = "Dates"
        , ylab = "Mean")
#   dev.off()
#   png("figure/P3_Dates-Median.png")
   par(mfrow = c(2,1))
   plot(stepsPerDate_NoNA$Dates, stepsPerDate_NoNA$Median
        , type = "l"
        , main = "Median of total number of steps per date (NA removed)"
        , xlab = "Dates"
        , ylab = "Median"
        , ylim = c(0,270))
   plot(stepsPerDate$Dates, stepsPerDate$Median
        , type = "l"
        , main = "Median of total number of steps per date (NA filled)"
        , xlab = "Dates"
        , ylab = "Median"
        , ylim = c(0,270))
#   dev.off()
  
  #----
  #Total, mean and median of total number ofsteps per day
  
  #histogram of total number of steps per day
  #png("figure/P3_Hist-Steps.png") #create the png graph device
  

  #dev.off()
  #----
  
}
myMedian <- function(x)
{
  if(length(x)%%2 == 0)
  {
    mean(x[length(x)/2],x[length(x)/2+1])
  }
  else
  {
    x[as.integer(length(x)/2)+1]
  }
}