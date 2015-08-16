PA1_4 <- function()
{
  library(lubridate)
  library(dplyr)
  library(lattice)
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
  
  steps <- mutate(steps, Weekday = weekdays(date))
  steps <- mutate(steps, isWeekday = "Weekday")
  
  for(i in 1:length(steps$Weekday))
  {
    if(steps$Weekday[i] == "Saturday" || steps$Weekday[i] == "Sunday")
    {
      steps$isWeekday[i] <- "Weekend"
    }
  }
  
  steps <- select(steps, -Weekday)
  
  WeekendDays <- filter(steps, isWeekday == "Weekend")
  WeekdayDays <- filter(steps, isWeekday == "Weekday")
  
  Intervals <- seq(from = min(WeekendDays$interval), to = max(WeekendDays$interval), by = 5)
  Weekend_stepsPerInterval <- as.data.frame(Intervals, row.names(c("Intervals")))
  stepMean <- c()
  for(i in Intervals)
  {
    temp <- filter(WeekendDays, interval == i) %>% select(steps) %>% arrange()
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
  Weekend_stepsPerInterval <- mutate(stepsPerInterval, StepsMean = stepMean)
  Weekend_stepsPerInterval <- mutate(Weekend_stepsPerInterval, dayType = rep("Weekend", length(Weekend_stepsPerInterval$Intervals)))
  
  
  Intervals <- seq(from = min(WeekdayDays$interval), to = max(WeekdayDays$interval), by = 5)
  Weekdays_stepsPerInterval <- as.data.frame(Intervals, row.names(c("Intervals")))
  
  stepMean <- c()
  
  for(i in Intervals)
  {
    temp <- filter(WeekdayDays, interval == i) %>% select(steps) %>% arrange()
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
  Weekdays_stepsPerInterval <- mutate(stepsPerInterval, StepsMean = stepMean)
  Weekdays_stepsPerInterval <- mutate(Weekdays_stepsPerInterval, dayType = rep("Weekdays", length(Weekdays_stepsPerInterval$Intervals)))
  
  total <- rbind(Weekend_stepsPerInterval, Weekdays_stepsPerInterval)
  png("figure/WEEKENDS.png")
  p <- xyplot(StepsMean ~ Intervals | dayType, data = total, type = "l", layout = c(1,2))
  print(p)
  dev.off()
  
}