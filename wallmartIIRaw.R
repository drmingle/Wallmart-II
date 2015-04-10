#Wallmart II, job hunt second round
#Ver. 0.0.2 #Weather mapping ready

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("zoo")
require("h2o")
require("leaps")
require("ggplot2")

#Read Settings file(EXPERIMENTAL)
directories <- fromJSON(file = "SETTINGS.json")

#Set Directories
workingDirectory <- directories$workingDirectory
setwd(workingDirectory)
dataDirectory <- directories$dataDirectory
#h2o location
h2o.jarLoc <- directories$h2o.jarLoc

#Define extra functions
source(file.path(workingDirectory, "traceMissingRemoval.R"))

#Detect available cores
numCores <- detectCores()

#Load Raw Data----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
keys <- fread(file.path(dataDirectory, directories$keyFile), verbose = TRUE)
weather <- fread(file.path(dataDirectory, directories$weatherFile), verbose = TRUE)

#Traces and missing values removal------------------
weather <- as.data.frame(weather)
for (station in sort(unique(weather$station_nbr))){
  
  #This function fills out the missing data defined by different user defined methods  
  stationWeather <- traceMissingRemoval(weather[weather$station_nbr == station, ])
  weather[weather$station_nbr == station, ] <- stationWeather
  
  print(paste0("weather station # ", station, " processed"))
}

#Transform "M"s to NAs
weather[weather == "M"] <- NA
weather[weather == "-"] <- NA
weather <- as.data.table(weather)

#EDA--------------------------------
#EDA #1; Find Missing values in weather data
NAsInWeather <- as.data.frame(colSums(is.na(weather)) / nrow(weather) * 100)
colnames(NAsInWeather) <-  "MissingValuesProportion"
NAsInWeather$names <- rownames(NAsInWeather)

#Plot the amount of missing values in weather data
ggplot(data = NAsInWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

#Merge stores with their respective weather stations----------------
#Define mapping of stores and stations as list
stationsStoresList <- as.list(keys[, station_nbr])

train$station_nbr <- sapply(train$store_nbr, function(sNumber){
  station <- stationsStoresList[[sNumber]]
  return(station)
})

test$station_nbr <- sapply(test$store_nbr, function(sNumber){
  station <- stationsStoresList[[sNumber]]
  return(station)
})

#Select weather columns without NAs
NAsInWeather <- as.data.frame(colSums(is.na(weather)) / nrow(weather) * 100)
colnames(NAsInWeather) <-  "MissingValues"

validColumns <- rownames(NAsInWeather)[NAsInWeather$MissingValues == 0]

trainWithWeather <- merge(train, weather[, validColumns, with = FALSE], by = c("date", "station_nbr"))
testWithWeather <- merge(test, weather[, validColumns, with = FALSE], by = c( "date", "station_nbr"))

#Make a submission .csv / .zip--------------
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
