PA1_2 <- function()
{
  library(lubridate)
  library(dplyr)
  steps <- read.csv("activity.csv")
  
  #convert to date format
  steps$date <- ymd(steps$date)
  
  steps <- na.omit(steps)
  
  #get intervals
  Intervals <- seq(from = min(steps$interval), to = max(steps$interval), by = 5)
  stepsPerInterval <- as.data.frame(Intervals, row.names(c("Intervals")))
  
  stepMean <- c()
  
  for(i in Intervals)
  {
    temp <- filter(steps, interval == i) %>% select(steps) %>% arrange()
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
  
  #print(stepsPerInterval)
  
  #interval with maximun average of steps
  maxActivityInterval <- filter(stepsPerInterval, StepsMean == max(StepsMean))
  #print(maxActivityInterval)

  plot(stepsPerInterval$Intervals, stepsPerInterval$StepsMean, type = "l")
}