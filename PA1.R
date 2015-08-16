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
  
  stepsPerDate <- mutate(stepsPerDate, Dates = allDates)
  stepsPerDate <- mutate(stepsPerDate, Steps = stepcount)
  stepsPerDate <- mutate(stepsPerDate, Mean = stepMean)
  stepsPerDate <- mutate(stepsPerDate, Median = stepMedian)
  
  #----
  #Total, mean and median of total number ofsteps per day
  
  #histogram of total number of steps per day
  #png("figure/P1_Hist-Steps.png") #create the png graph device
  hist(stepsPerDate$Steps, col = "red", main = "Histogram of the total number of steps taken each day", xlab = "Number of steps per day")
  #dev.off()
  #----
  
  #png("figure/P1_Dates-Steps.png") #create the png graph device
  #plot(stepsPerDate$Dates, stepsPerDate$Steps, type = "l")
  #dev.off()
  #png("figure/P1_Dates-Mean.png") #create the png graph device
  plot(stepsPerDate$Dates, stepsPerDate$Mean, type = "l"
       , main = "Mean of total number of steps per date"
       , xlab = "Dates"
       , ylab = "Mean")
  #dev.off()
  #png("figure/P1_Dates-Median.png") #create the png graph device
  plot(stepsPerDate$Dates, stepsPerDate$Median, type = "l"
       , main = "Median of total number of steps per date"
       , xlab = "Dates"
       , ylab = "Median")
  #dev.off()
  
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