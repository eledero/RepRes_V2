
#Loading and preprocessing the data#

rawData <- read.csv("activity.csv")

complCas <- complete.cases(rawData)

procData <- rawData[complCas, ]


#What is mean total number of steps taken per day?#

uniqueCasesDate <- unique(procData$date)

dailySteps <- c()

meanDailySteps <- c()

for (i in 1:length(uniqueCasesDate)){
  
  dailySteps[i] <- sum(procData[which(procData$date == uniqueCasesDate[i]),"steps"])
  meanDailySteps[i] <- sum(procData[which(procData$date == uniqueCasesDate[i]),"steps"])/length(procData[which(procData$date == uniqueCasesDate[i]),"steps"])
}

complData <- data.frame(uniqueCasesDate, meanDailySteps)

hist(dailySteps)

meanSteps <- sum(dailySteps)/length(dailySteps)
medianSteps <- median(dailySteps)

print(paste("- The mean of the daily steps is ", meanSteps, sep = ""))
print("")
print(paste("- The median of the daily steps is ", medianSteps, sep = ""))

#What is the average daily activity pattern?#

uniqueCasesInter <- unique(procData$interval)

interSteps <- c()

for (i in 1:length(uniqueCasesInter)){
  
  interSteps[i] <- mean(procData[which(procData$interval == uniqueCasesInter[i]),"steps"])#/length(procData[which(procData$interval == uniqueCasesInter[i]),"steps"])
}

plot(uniqueCasesInter, interSteps, type = "l")


maxInterval <- uniqueCasesInter[which.max(interSteps)]

print(paste("- The interval with the most average steps is ", maxInterval, sep = ""))


#Imputing missing values#

nComplCas <- sum(!complCas)

print(paste("- The number of rows with NA's is ", nComplCas, sep = ""))


corrRawData <- c()

lastAvg <- 0

for (i in 1:dim(rawData)[1]){
  
  if (!complCas[i]) {
    
    if(length(interSteps[rawData[i, "interval"]]) !=0){
      
      corrRawData[i] <- interSteps[rawData[i, "interval"]]
      
      lastAvg <- corrRawData[i]
      
    }else{
      
      corrRawData[i] <-  lastAvg
    }
    
  } else {
    
    corrRawData[i] <- rawData[i, "steps"]
  }
  
  
  
}

noNAdata <- data.frame(corrRawData, rawData$date, rawData$interval)
names(noNAdata) <- c("steps", "date", "interval")

dailySteps2 <- c()
meanDailySteps2 <- c()

for (i in 1:length(uniqueCasesDate)){
  
  dailySteps2[i] <- sum(noNAdata[which(noNAdata$date == uniqueCasesDate[i]),"steps"])
  meanDailySteps2[i] <- sum(noNAdata[which(noNAdata$date == uniqueCasesDate[i]),"steps"])/length(procData[which(procData$date == uniqueCasesDate[i]),"steps"])
}


hist(dailySteps2)



