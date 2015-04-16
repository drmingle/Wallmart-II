#Wallmart II, job hunt second round
#Ver. 0.1.3 #weather types mining included + all three weather data tested

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("zoo")
require("h2o")
require("prospectr")
require("leaps")
require("ggplot2")
require("Metrics")

#Read Settings file
directories <- fromJSON(file = "SETTINGS.json")

#Set Directories
workingDirectory <- directories$workingDirectory
setwd(workingDirectory)
dataDirectory <- directories$dataDirectory
#h2o location
h2o.jarLoc <- directories$h2o.jarLoc

#Define extra functions
source(file.path(workingDirectory, "normalOutliersFinder.R"))
source(file.path(workingDirectory, "traceMissingRemoval.R"))
source(file.path(workingDirectory, "weatherModeling.R"))
source(file.path(workingDirectory, "weatherType2Table.R"))
source(file.path(workingDirectory, "multiplot.R"))
source(file.path(workingDirectory, "weatherDistributionPlot.R"))

#Detect available cores
numCores <- detectCores()

#Load Raw Data----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
keys <- fread(file.path(dataDirectory, directories$keyFile), verbose = TRUE)
weather <- fread(file.path(dataDirectory, directories$weatherFile), verbose = TRUE)

#Traces and missing values removal------------------
#Keep an object with the original weather for comparison
originalWeather <- as.data.frame(weather)
#Transform "M"s to NAs
originalWeather[originalWeather == "M"] <- NA
originalWeather[originalWeather == "-"] <- NA
originalWeather <- as.data.table(originalWeather)

#Remove unknown values and bad data 
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

#Missing weather modeling-------------------
#Modeling from data without NAs only
weatherNoNAs <- weather
modeledWetbulb  <- weatherModeling(weather, col2Predict = "wetbulb")
weatherNoNAs$wetbulb <- modeledWetbulb
modeledStnpressure <- weatherModeling(weather, col2Predict = "stnpressure")
weatherNoNAs$stnpressure <- modeledStnpressure
modeledSealevel <- weatherModeling(weather, col2Predict = "sealevel")
weatherNoNAs$sealevel <- modeledSealevel
modeledSnowfall <- weatherModeling(weather, col2Predict = "snowfall")
weatherNoNAs$snowfall <- modeledSnowfall
modeledSunrise <- weatherModeling(weather, col2Predict = "sunrise")
weatherNoNAs$sunrise <- modeledSunrise
modeledSunset <- weatherModeling(weather, col2Predict = "sunset")
weatherNoNAs$sunset <- modeledSunset
modeledDepart <- weatherModeling(weather, col2Predict = "depart")
weatherNoNAs$depart <- modeledDepart
#Save progress
save(weatherNoNAs, file = "weatherNoNAs.RData")

#Modeling from all data progresively
weatherNoNAsProg <- weather
modeledWetbulb  <- weatherModeling(weatherNoNAsProg, col2Predict = "wetbulb")
weatherNoNAsProg$wetbulb <- modeledWetbulb
modeledStnpressure <- weatherModeling(weatherNoNAsProg, col2Predict = "stnpressure")
weatherNoNAsProg$stnpressure <- modeledStnpressure
modeledSealevel <- weatherModeling(weatherNoNAsProg, col2Predict = "sealevel")
weatherNoNAsProg$sealevel <- modeledSealevel
modeledSnowfall <- weatherModeling(weatherNoNAsProg, col2Predict = "snowfall")
weatherNoNAsProg$snowfall <- modeledSnowfall
modeledSunrise <- weatherModeling(weatherNoNAsProg, col2Predict = "sunrise")
weatherNoNAsProg$sunrise <- modeledSunrise
modeledSunset <- weatherModeling(weatherNoNAsProg, col2Predict = "sunset")
weatherNoNAsProg$sunset <- modeledSunset
modeledDepart <- weatherModeling(weatherNoNAsProg, col2Predict = "depart")
weatherNoNAsProg$depart <- modeledDepart
#Save progress
save(weatherNoNAsProg, file = "weatherNoNAsProg.RData")

#EDA--------------------------------
#EDA #1; Find Missing before and after transformation & Weather Modeling
#Original Weather
NAsInOriginalWeather <- as.data.frame(colSums(is.na(originalWeather)) / nrow(originalWeather) * 100)
colnames(NAsInOriginalWeather) <-  "MissingValuesProportion"
NAsInOriginalWeather$names <- rownames(NAsInOriginalWeather)
#Plot the amount of missing values in original weather data
missingOriginalWeather <- ggplot(data = NAsInOriginalWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

#Transformed Weather
NAsInWeather <- as.data.frame(colSums(is.na(weather)) / nrow(weather) * 100)
colnames(NAsInWeather) <-  "MissingValuesProportion"
NAsInWeather$names <- rownames(NAsInWeather)
#Plot the amount of missing values in weather data
missingTransformedWeather <- ggplot(data = NAsInWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

#Modeled Missing Weather
NAsInWeather <- as.data.frame(colSums(is.na(weatherNoNAs)) / nrow(weatherNoNAs) * 100)
colnames(NAsInWeather) <-  "MissingValuesProportion"
NAsInWeather$names <- rownames(NAsInWeather)
#Plot the amount of missing values in weather data
missingModeledWeather <- ggplot(data = NAsInWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

multiplot(missingOriginalWeather, missingTransformedWeather, missingModeledWeather, cols = 1)

#EDA #2; Vizualize Weather data distributions, originally, transformed and after modeling
#Original Weather
weatherDistributionPlot(originalWeather)
#Original Weather
weatherDistributionPlot(weather)
#Original Weather
weatherDistributionPlot(weatherNoNAs)

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

weatherValidColumns <- rownames(NAsInWeather)[NAsInWeather$MissingValues == 0]

trainWithWeather <- merge(train, weather[, weatherValidColumns, with = FALSE], by = c("date", "station_nbr"))
testWithWeather <- merge(test, weather[, weatherValidColumns, with = FALSE], by = c("date", "station_nbr"))

rm(train, test, keys, weather, stationsStoresList)

#Linear Feature Selection------------
#Exclude codsums and dates
validColumns <- c("units", "item_nbr", 
                  weatherValidColumns[c(-which(weatherValidColumns == "codesum"), -which(weatherValidColumns == "date"), 
                                        -which(weatherValidColumns == "station_nbr"))])

#Valid rows  
validRowsTrain <- which(complete.cases(trainWithWeather))
#Reduce data size
reducedIdxs <- sample(validRowsTrain, floor(length(validRowsTrain)) * 0.1)

linMatrixData <- as.data.frame(trainWithWeather)[reducedIdxs, validColumns]

for (columnName in validColumns[-2]){
  linMatrixData[, columnName] <- signif(as.numeric(linMatrixData[, columnName]), digits = 5)  
}

linMatrixData$item_nbr <- as.factor(linMatrixData$item_nbr)  

#Transform Data to a matrix
linMatrixData <- model.matrix(~ . , data = linMatrixData)
#Find best linear combination of features
linearBestModels <- regsubsets(x = linMatrixData[, c(-1, -2)],
                               y = as.numeric(linMatrixData[, "units"]), 
                               method = "forward", nvmax = 30)

#Plot the best number of predictors
bestMods <- summary(linearBestModels)
bestNumberOfPredictors <- which.min(bestMods$cp)
plot(bestMods$cp, xlab="Number of Variables", ylab="CP Error", main ="Best Predictors")
points(bestNumberOfPredictors, bestMods$cp[bestNumberOfPredictors], pch=20, col="red")

#Name of the most predictive rankings
predictors1 <- as.data.frame(bestMods$which)
predictors <- names(sort(apply(predictors1[, -1], 2, sum), decreasing = TRUE)[1:bestNumberOfPredictors])

#Model comparison-------------------
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

##RF Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", 
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Station numbers as factors
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
#Item numbers as factors
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
#Store numbers as factors
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)

#Smaller train dataset indices
smallerDatasplit1 <- sample(dataSplits[[1]], floor(length(dataSplits[[1]]) * 0.01))
#Cross Validation
wallmartRFModelCV <- h2o.randomForest(x = validColumns, y = "units",
                                      data = h2oWallmartTrain[smallerDatasplit1, ],
                                      nfolds = 5,
                                      classification = FALSE,
                                      ntree = c(75, 100),
                                      depth = c(25, 50),  
                                      type = "BigData",
                                      importance = TRUE)

#Save importance plot
RFImportanceDf <- as.data.frame(t(wallmartRFModelCV@model[[1]]@model$varimp[1, ]))
RFImportanceDf$variables <- as.character(names(wallmartRFModelCV@model[[1]]@model$varimp[1, ]))
names(RFImportanceDf) <- c("PercentInfluence", "variables")
variableImportnceRF <- ggplot(data = RFImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity")

#Best Hyperparameters
bestNtree <- wallmartRFModelCV@model[[1]]@model$params$ntree
bestDepth <- wallmartRFModelCV@model[[1]]@model$params$depth

#h2o.ai GBM Modelling    
wallmartRFModel <- h2o.randomForest(x = validColumns, y = "units",
                                    data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]]), ],
                                    classification = FALSE,
                                    ntree = bestNtree,
                                    depth = bestDepth,   
                                    type = "BigData")

#Regression Prediction 
predictionRFValidation <- as.data.frame(h2o.predict(wallmartRFModel, newdata = h2oWallmartTrain[dataSplits[[3]]]))[, 1]
#Round negative values to zero
predictionRFValidation[predictionRFValidation < 0] <- 0

print(paste0("RFs model built")) 

#Save model
h2o.saveModel(wallmartRFModel, dir = dataDirectory, name = "RFMiniModel", save_cv = FALSE, force = FALSE)
#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Evaluate Model against known data
actualAmountOfUnits <- as.data.frame(trainWithWeather[, "units", with = FALSE])[dataSplits[[3]], 1]
testrmsle <- rmsle(predictionRFValidation, actualAmountOfUnits)
print(paste0("testNWMSE error of: ", testrmsle))

##Make a submission file .csv / .zip
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTest <- as.h2o(h2oServer, testWithWeather)

#Load h2o Model
wallmartRFModel <- h2o.loadModel(h2oServer, path = file.path(dataDirectory, "RFMiniModel"))

#Station numbers as factors
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
#Item numbers as factors
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
#Store numbers as factors
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)

#Regression Prediction 
predictionRFValidation <- as.data.frame(h2o.predict(wallmartRFModel, newdata = h2oWallmartTest))[, 1]
#Round negative values to zero
predictionRFValidation[predictionRFValidation < 0] <- 0

#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

print(paste0("RF predictions ready"))

sampleSubmissionFile$id <- paste(testWithWeather$store_nbr, testWithWeather$item_nbr, testWithWeather$date, sep = "_")
sampleSubmissionFile$units <- predictionRFValidation

#Write File
write.csv(sampleSubmissionFile, file = "RFMiniNoNAsProgI.csv", row.names = FALSE)
system('zip RFMiniNoNAsProgI.zip RFMiniNoNAsProgI.csv')

##GBM Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", 
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])
                                      
#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Station numbers as factors
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
#Item numbers as factors
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
#Store numbers as factors
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)

#Smaller train dataset indices
smallerDatasplit1 <- sample(dataSplits[[1]], floor(length(dataSplits[[1]]) * 0.02))
#Cross Validation
wallmartGBMModelCV <- h2o.gbm(x = validColumns, y = "units",
                              data = h2oWallmartTrain[smallerDatasplit1, ],
                              nfolds = 5,
                              distribution = "gaussian",
                              interaction.depth = c(4, 7, 11),
                              shrinkage = c(0.001, 0.003),                           
                              n.trees = 250,
                              importance = TRUE,                           
                              grid.parallelism = numCores)

#Save importance plot
GBMImportanceDf <- as.data.frame(wallmartGBMModelCV@model[[1]]@model$varimp$Percent.Influence)
GBMImportanceDf$variables <- rownames(wallmartGBMModelCV@model[[1]]@model$varimp)
names(GBMImportanceDf) <- c("PercentInfluence", "variables")
variableImportnceGBM <- ggplot(data = GBMImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity")

#Best Hyperparameters
bestInteraction.depth <- wallmartGBMModelCV@model[[1]]@model$params$interaction.depth
bestShrinkage <- wallmartGBMModelCV@model[[1]]@model$params$shrinkage

#h2o.ai GBM Modelling    
wallmartGBMModel <- h2o.gbm(x = validColumns, y = "units",
                            data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]]), ],
                            distribution = "gaussian",
                            interaction.depth = bestInteraction.depth,
                            shrinkage = bestShrinkage,                           
                            n.trees = 6000)

#Regression Prediction 
predictionGBMValidation <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTrain[dataSplits[[3]]]))[, 1]
#Round negative values to zero
predictionGBMValidation[predictionGBMValidation < 0] <- 0

print(paste0("GBMs model built")) 

#Save model
h2o.saveModel(wallmartGBMModel, dir = dataDirectory, name = "GBMMiniModel", save_cv = FALSE, force = FALSE)
#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Evaluate Model against known data
actualAmountOfUnits <- as.data.frame(trainWithWeather[, "units", with = FALSE])[dataSplits[[3]], 1]
testrmsle <- rmsle(predictionGBMValidation, actualAmountOfUnits)
print(paste0("testNWMSE error of: ", testrmsle))

##Make a submission file .csv / .zip
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTest <- as.h2o(h2oServer, testWithWeather)

#Load h2o Model
wallmartGBMModel <- h2o.loadModel(h2oServer, path = file.path(dataDirectory, "GBMMiniModel"))

#Station numbers as factors
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
#Item numbers as factors
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
#Store numbers as factors
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)

#Regression Prediction 
predictionGBMValidation <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTest))[, 1]
#Round negative values to zero
predictionGBMValidation[predictionGBMValidation < 0] <- 0

#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

print(paste0("GBM predictions ready"))

sampleSubmissionFile$id <- paste(testWithWeather$store_nbr, testWithWeather$item_nbr, testWithWeather$date, sep = "_")
sampleSubmissionFile$units <- predictionGBMValidation

#Write File
write.csv(sampleSubmissionFile, file = "GBMMiniI.csv", row.names = FALSE)
system('zip GBMMiniI.zip GBMMiniI.csv')

#Modeling with full data--------------------
##GBM Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", 
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Station numbers as factors
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
h2oWallmartTrain$station_nbr <- as.factor(h2oWallmartTrain$station_nbr)
#Item numbers as factors
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
h2oWallmartTrain$item_nbr <- as.factor(h2oWallmartTrain$item_nbr)
#Store numbers as factors
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)
h2oWallmartTrain$store_nbr <- as.factor(h2oWallmartTrain$store_nbr)

#h2o.ai GBM Modelling    
wallmartGBMModel <- h2o.gbm(x = validColumns, y = "units",
                            data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]], dataSplits[[3]]), ],
                            distribution = "gaussian",
                            interaction.depth = bestInteraction.depth,
                            shrinkage = bestShrinkage,                           
                            n.trees = 6000)

#Save model
h2o.saveModel(wallmartGBMModel, dir = dataDirectory, name = "GBMFullModel", save_cv = FALSE, force = FALSE)
#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Make predictions on (unknown) test data
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTest <- as.h2o(h2oServer, testWithWeather)

#Load h2o Model
wallmartGBMModel <- h2o.loadModel(h2oServer, path = file.path(dataDirectory, "GBMFullModel"))

#Station numbers as factors
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
h2oWallmartTest$station_nbr <- as.factor(h2oWallmartTest$station_nbr)
#Item numbers as factors
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
h2oWallmartTest$item_nbr <- as.factor(h2oWallmartTest$item_nbr)
#Store numbers as factors
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)
h2oWallmartTest$store_nbr <- as.factor(h2oWallmartTest$store_nbr)

#Regression Prediction 
predictionGBMValidation <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTest))[, 1]
#Round negative values to zero
predictionGBMValidation[predictionGBMValidation < 0] <- 0

#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

print(paste0("GBM predictions ready"))

sampleSubmissionFile$id <- paste(testWithWeather$store_nbr, testWithWeather$item_nbr, testWithWeather$date, sep = "_")
sampleSubmissionFile$units <- predictionGBMFull

#Write File
write.csv(sampleSubmissionFile, file = "GBMFullI.csv", row.names = FALSE)
