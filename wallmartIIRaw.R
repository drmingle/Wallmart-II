#Wallmart II, job hunt second round
#Ver. 0.0.4 #GBM Modeling Evaluation code complete

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("zoo")
require("h2o")
require("leaps")
require("ggplot2")
require("Metrics")

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
#EDA #1; Find Missing before and after transformation
NAsInWeather <- as.data.frame(colSums(is.na(weather)) / nrow(weather) * 100)
colnames(NAsInWeather) <-  "MissingValuesProportion"
NAsInWeather$names <- rownames(NAsInWeather)

#Plot the amount of missing values in weather data
ggplot(data = NAsInWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

#Missing weather modeling-------------------

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

#Linear Feature Selection------------

#Models comparison-------------------
##Data Preparation
#Length Data Splits
trainTrainData <- floor(nrow(trainWithWeather) * 0.6)   #60% of training data used to train models
trainValidationData <- floor(nrow(trainWithWeather) * 0.2)   #20% of training data to validate models
trainTestData <- floor(nrow(trainWithWeather) * 0.2)   #20% of training data to score models

idxsdiff <- nrow(trainWithWeather) - (trainTrainData + trainValidationData + trainTestData)
groupsVector <- sample(c(rep(1, trainTrainData), rep(2, trainValidationData), 
                         rep(3, trainTestData + idxsdiff)), 
                       nrow(trainWithWeather))

#Shuffle Indices
set.seed(1001001)
dataSplits <- split(seq(1, nrow(trainWithWeather)), as.factor(groupsVector))

##GBM Model
#Start h2o from command line
system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Smaller train dataset indices
smallerDatasplit1 <- sample(dataSplits[[1]], floor(length(dataSplits[[1]]) * 0.1))
#Cross Validation
wallmartGBMModelCV <- h2o.gbm(x = names(h2oWallmartTrain)[c(-2, -5)], y = "units",
                              data = h2oWallmartTrain[smallerDatasplit1, ],
                              nfolds = 5,
                              distribution = "gaussian",
                              interaction.depth = c(7, 11),
                              shrinkage = c(0.001, 0.003),                           
                              n.trees = 250,
                              importance = TRUE,                           
                              grid.parallelism = numCores)

#Save importance plot
GBMImportanceDf <- as.data.frame(wallmartGBMModelCV@model[[1]]@model$varimp$Percent.Influence)
GBMImportanceDf$variables <- names(h2oWallmartTrain)[c(-2, -5)]
names(GBMImportanceDf) <- c("PercentInfluence", "variables")
variableImportnceGBM <- ggplot(data = GBMImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity")

#Best Hyperparameters
bestInteraction.depth <- wallmartGBMModelCV@model[[1]]@model$params$interaction.depth
bestShrinkage <- wallmartGBMModelCV@model[[1]]@model$params$shrinkage

#h2o.ai GBM Modelling    
wallmartGBMModel <- h2o.gbm(x = names(h2oWallmartTrain)[c(-2, -5)], y = "units",
                            data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]]), ],
                            distribution = "gaussian",
                            interaction.depth = bestInteraction.depth,
                            shrinkage = bestShrinkage,                           
                            n.trees = 6000)

#Regression Prediction 
predictionGBMValidation <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTrain[dataSplits[[3]], ]), ]))[, 1]
print(paste0("GBMs model built")) 

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Evaluate Model against known data
actualAmountOfUnits <- trainWithWeather[dataSplits[[3]], "clicks"]
testrmsle <- rmsle(predictionGBMValidation, actualAmountOfUnits)
print(paste0("testNWMSE error of: ", testrmsle))


#Make a submission .csv / .zip--------------
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
