PA1 <- function()
{
  library(lubridate)
  library(dplyr)
  steps <- read.csv("activity.csv")
  
  #convert to date format
  steps$date <- ymd(steps$date)
  
  steps <- na.omit(steps)
  
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
  print(length(stepMedian))
  
  stepsPerDate <- mutate(stepsPerDate, Steps = stepcount)
  stepsPerDate <- mutate(stepsPerDate, Mean = stepMean)
  stepsPerDate <- mutate(stepsPerDate, Median = stepMedian)
  
  #----
  #Total, mean and median of total number ofsteps per day
  
  #histogram of total number of steps per day
  hist(stepsPerDate$Steps, col = "red")
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