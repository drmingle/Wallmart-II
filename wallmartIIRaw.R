#Wallmart II, job hunt second round
#Ver. 0.2.3 #impoved modeling included

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
#EDA Plots location
EDAPlotsLoc <- directories$EDALoc

#Define extra functions
source(file.path(workingDirectory, "constantPredictionsFinder.R"))
source(file.path(workingDirectory, "availabilityModeling.R"))
source(file.path(workingDirectory, "weatherCorrelations.R"))
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

#Data transformation and unnecesary data removal
#Remove station 5 (store 35) from training file
train <- train[!train$store_nbr == 35]

#remove constant values out training and testing datasets
constrantItemStoreCombinations <- constantPredictionsFinder(train)

train <- merge(train, constrantItemStoreCombinations, by = c("item_nbr", "store_nbr"))
test <- merge(test, constrantItemStoreCombinations, by = c("item_nbr", "store_nbr"))

trainConstant <- train[availabilityDt == FALSE, .(date, store_nbr, item_nbr, units)]
train <- train[availabilityDt == TRUE, .(date, store_nbr, item_nbr, units)]
testConstant <- test[availabilityDt == FALSE, .(date, store_nbr, item_nbr)]
testConstant$units <- 0
test <- test[availabilityDt == TRUE, .(date, store_nbr, item_nbr)]

#Availability Data plus projected sales for that day
availableItemsModeled <- availabilityModeling(train, test)

#Merge Data
train <- merge(train, availableItemsModeled, by = c("item_nbr", "store_nbr", "date"))
test <- merge(test, availableItemsModeled, by = c("item_nbr", "store_nbr", "date"))

#Traces and missing values removal------------------
#Keep an object with the original weather for comparison
originalWeather <- as.data.frame(weather)
#Transform "M"s to NAs
originalWeather[originalWeather == "M"] <- NA
originalWeather[originalWeather == "-"] <- NA
originalWeather <- as.data.table(originalWeather)

#Add a factor column containing the day
originalWeather$day <- substr(originalWeather$date, 9, 10)
#Add a factor column containing the month information
originalWeather$month <- substr(originalWeather$date, 6, 7)
#Add a factor column containing the year information
originalWeather$year <- substr(originalWeather$date, 1, 4)

#Remove unknown values and bad data 
weather <- as.data.frame(weather)

#Missing Values and Traces fillout
for (station in sort(unique(weather$station_nbr))){
  
  #This function fills out the missing data defined by different user defined methods  
  stationWeather <- traceMissingRemoval(weather[weather$station_nbr == station, ])
  weather[weather$station_nbr == station, ] <- stationWeather
  
  print(paste0("weather station # ", station, " processed"))
}

#Add a factor column containing the day
weather$day <- substr(weather$date, 9, 10)
#Add a factor column containing the month information
weather$month <- substr(weather$date, 6, 7)
#Add a factor column containing the year information
weather$year <- substr(weather$date, 1, 4)

#Transform "M"s to NAs
weather[weather == "M"] <- NA
weather[weather == "-"] <- NA
weather <- as.data.table(weather)

##Codesum column as table
codesumTable <- mclapply(weather$codesum, weatherType2Table, mc.cores = numCores)
#Concatenate lists into a data.frame
codesumTable <- as.data.table(do.call(rbind, codesumTable))

#Concatenated Weather
weather <- cbind(weather, codesumTable)
rm(codesumTable)

#Remove station 5 temporarely for modeling
weatherNo5 <- weather[!weather$station_nbr == 5, ]

#Missing weather modeling-------------------
#Modeling from data without NAs only
weatherNoNAs <- weatherNo5
modeledSealevel <- weatherModeling(weatherNo5, col2Predict = "sealevel")
weatherNoNAs$sealevel <- modeledSealevel
modeledSnowfall <- weatherModeling(weatherNo5, col2Predict = "snowfall")
weatherNoNAs$snowfall <- modeledSnowfall
modeledSunset <- weatherModeling(weatherNo5, col2Predict = "sunset")
weatherNoNAs$sunset <- modeledSunset
modeledSunrise <- weatherModeling(weatherNo5, col2Predict = "sunrise")
weatherNoNAs$sunrise <- modeledSunrise
modeledDepart <- weatherModeling(weatherNo5, col2Predict = "depart")
weatherNoNAs$depart <- modeledDepart

#Save progress
save(weatherNoNAs, file = "weatherNoNAs.RData")

#Progressive modeling
weatherProg <- weatherNo5
modeledSealevel <- weatherModeling(weatherProg, col2Predict = "sealevel")
weatherProg$sealevel <- modeledSealevel
modeledSnowfall <- weatherModeling(weatherProg, col2Predict = "snowfall")
weatherProg$snowfall <- modeledSnowfall
modeledSunset <- weatherModeling(weatherProg, col2Predict = "sunset")
weatherProg$sunset <- modeledSunset
modeledSunrise <- weatherModeling(weatherProg, col2Predict = "sunrise")
weatherProg$sunrise <- modeledSunrise
modeledDepart <- weatherModeling(weatherProg, col2Predict = "depart")
weatherProg$depart <- modeledDepart

#Save progress
save(weatherProg, file = "weatherProg.RData")

#EDA--------------------------------
#EDA 0.5 Explore missing weather correlations
predictorsList <- lapply(names(weatherNo5)[c(seq(3, 12), seq(14, 20))], function(colname, weather2Process){
  return(weatherCorrelations(colname, weather2Process))
}, weather2Process = weatherNo5)

#EDA #1; Find Missing before and after transformation & Weather Modeling
#Original Weather
NAsInOriginalWeather <- as.data.frame(colSums(is.na(originalWeather)) / nrow(originalWeather) * 100)
colnames(NAsInOriginalWeather) <-  "MissingValuesProportion"
NAsInOriginalWeather$names <- rownames(NAsInOriginalWeather)
#Plot the amount of missing values in original weather data
missingOriginalWeather <- ggplot(data = NAsInOriginalWeather, aes(x = names, y = MissingValuesProportion, fill = names)) + geom_bar(stat = "identity") 

#Transformed Weather without station 5
NAsInWeather <- as.data.frame(colSums(is.na(weatherNo5)) / nrow(weatherNo5) * 100)
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
#Gaps filled Weather
weatherDistributionPlot(weather)
#Predicted Weather NoNas
weatherDistributionPlot(weatherNoNAs)
#Predicted Weather NoNas Progresive
weatherDistributionPlot(weatherNoNAsProg)
#Predicted Weather NoNas Progresive Correlations
weatherDistributionPlot(weatherNoNAsCorr)

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

weather <- weatherNoNAs
#weather <- weatherProg

#Select weather columns without NAs
NAsInWeather <- as.data.frame(colSums(is.na(weather)) / nrow(weather) * 100)
colnames(NAsInWeather) <-  "MissingValues"

#Only columns without NAs
#weatherValidColumns <- rownames(NAsInWeather)[NAsInWeather$MissingValues == 0]
#All Columns
weatherValidColumns <- rownames(NAsInWeather)

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
reducedIdxs <- sample(validRowsTrain, floor(length(validRowsTrain)) * 0.01)

linMatrixData <- as.data.frame(trainWithWeather)[reducedIdxs, validColumns]

for (columnName in validColumns[-2]){
  linMatrixData[, columnName] <- as.numeric(linMatrixData[, columnName])
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
#Variable Training data splits
trainTrainData <- floor(nrow(trainWithWeather) * 0.6)   #60% of training data used to train models
trainValidationData <- floor(nrow(trainWithWeather) * 0.2)   #20% of training data to validate models
trainTestData <- floor(nrow(trainWithWeather) * 0.2)   #20% of training data to score models

idxsdiff <- nrow(trainWithWeather) - (trainTrainData + trainValidationData + trainTestData)
set.seed(1001000)
groupsVector <- sample(c(rep(1, trainTrainData), rep(2, trainValidationData), 
                         rep(3, trainTestData + idxsdiff)), 
                       nrow(trainWithWeather))

#Shuffle Indices
set.seed(1001001)
dataSplits <- split(seq(1, nrow(trainWithWeather)), as.factor(groupsVector))

##RF Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", "unitsProjected", "availableUnitsLogical",
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#Smaller train dataset indices
smallerDatasplit1 <- sample(dataSplits[[1]], floor(length(dataSplits[[1]]) * 0.5))
#Cross Validation
wallmartRFModelCV <- h2o.randomForest(x = validColumns, y = "units",
                                      data = h2oWallmartTrain[smallerDatasplit1, ],
                                      nfolds = 5,
                                      classification = FALSE,
                                      ntree = c(100, 125),
                                      depth = c(50, 75), 
                                      mtries = floor(ncol(h2oWallmartTrain) / 2),
                                      type = "BigData",
                                      importance = TRUE)

#Save importance plot
RFImportanceDf <- as.data.frame(t(wallmartRFModelCV@model[[1]]@model$varimp[1, ]))
RFImportanceDf$variables <- as.character(names(wallmartRFModelCV@model[[1]]@model$varimp[1, ]))
names(RFImportanceDf) <- c("PercentInfluence", "variables")
print(ggplot(data = RFImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity"))
#Small pause
Sys.sleep(3)
#Save Plot
dev.print(file = paste0("ImportanceVariablesRandomForest"), device = png, width = 1200)

#Best Hyperparameters
bestNtree <- wallmartRFModelCV@model[[1]]@model$params$ntree
bestDepth <- wallmartRFModelCV@model[[1]]@model$params$depth
#bestNtree <- 125
#bestDepth <- 75
#Remove Grid Search model
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Reload R data into h2o
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#h2o.ai RF Modelling    
wallmartRFModel <- h2o.randomForest(x = validColumns, y = "units",
                                    data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]]), ],
                                    classification = FALSE,
                                    mtries = floor(ncol(h2oWallmartTrain) / 2),                                    
                                    ntree = bestNtree,
                                    depth = bestDepth,   
                                    type = "BigData")

#Regression Prediction 
predictionRFValidation <- as.data.frame(h2o.predict(wallmartRFModel, newdata = h2oWallmartTrain[dataSplits[[3]]]))[, 1]
#Round negative values to zero
predictionRFValidation[predictionRFValidation < 0] <- 0

print(paste0("RFs model built")) 

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Evaluate Model against known data
constantValuesIncluded <- sample(trainConstant$units, floor(nrow(trainConstant) * 0.2))
actualAmountOfUnits <- as.data.frame(trainWithWeather[, "units", with = FALSE])[dataSplits[[3]], 1]

testrmsle <- rmsle(c(rep(0, floor(nrow(trainConstant) * 0.2)), predictionRFValidation), 
                   c(constantValuesIncluded, actualAmountOfUnits))
print(paste0("testNWMSE error of: ", testrmsle))
save(testrmsle, file = "testrmsle.RData")

#Training with full data
##RF Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", "unitsProjected", "availableUnitsLogical",
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#h2o.ai RF Modelling    
wallmartRFModel <- h2o.randomForest(x = validColumns, y = "units",
                                    data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]], dataSplits[[3]]), ],
                                    classification = FALSE,
                                    ntree = bestNtree,
                                    depth = bestDepth,   
                                    mtries = floor(ncol(h2oWallmartTrain) / 2),                                    
                                    type = "BigData")

#Save model
h2o.saveModel(wallmartRFModel, dir = dataDirectory, name = "RFFullModel", save_cv = FALSE, force = FALSE)
#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

print(paste0("RFs model built")) 

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

##Make a submission file .csv / .zip
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTest <- as.h2o(h2oServer, testWithWeather)

#Load h2o Model
wallmartRFModel <- h2o.loadModel(h2oServer, path = file.path(dataDirectory, "RFFullModel"))

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTest[, columnName] <- as.factor(h2oWallmartTest[, columnName])
  h2oWallmartTest[, columnName] <- as.factor(h2oWallmartTest[, columnName])
}

#Regression Prediction 
predictionRF <- as.data.frame(h2o.predict(wallmartRFModel, newdata = h2oWallmartTest))[, 1]
#Round negative values to zero
predictionRF[predictionRF < 0] <- 0

#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

print(paste0("RF predictions ready"))

variablePredictionsIds <- paste(testWithWeather$store_nbr, testWithWeather$item_nbr, testWithWeather$date, sep = "_")
variablePredictions <- predictionRF
constantPredictionsIds <- paste(testConstant$store_nbr, testConstant$item_nbr, testConstant$date, sep = "_")
constantPredictions <- rep(0, nrow(testConstant))

sampleSubmissionFile$id <- c(variablePredictionsIds, constantPredictionsIds)
sampleSubmissionFile$units <- c(variablePredictions, constantPredictions)

#Write File
write.csv(sampleSubmissionFile, file = "RFWeatherNoNaNo5.csv", row.names = FALSE)
system('zip RFWeatherNoNaNo5.zip RFWeatherNoNaNo5.csv')

##GBM Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", "unitsProjected", "availableUnitsLogical",
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#Smaller train dataset indices
smallerDatasplit1 <- sample(dataSplits[[1]], floor(length(dataSplits[[1]]) * 0.5))

#Cross Validation
wallmartGBMModelCV <- h2o.gbm(x = validColumns, y = "units",
                              data = h2oWallmartTrain[smallerDatasplit1, ],
                              nfolds = 5,
                              distribution = "gaussian",
                              interaction.depth = c(7, 11),
                              shrinkage =  0.003,                           
                              n.trees = 250,
                              importance = TRUE,                           
                              grid.parallelism = numCores)

#Save importance plot
GBMImportanceDf <- as.data.frame(wallmartGBMModelCV@model[[1]]@model$varimp$Percent.Influence)
GBMImportanceDf$variables <- rownames(wallmartGBMModelCV@model[[1]]@model$varimp)
names(GBMImportanceDf) <- c("PercentInfluence", "variables")
print(ggplot(data = GBMImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity"))
#Small pause
Sys.sleep(3)
#Save Plot
dev.print(file = paste0("ImportanceVariablesGBM"), device = png, width = 1200)

#Best Hyperparameters
#bestInteraction.depth <- wallmartGBMModelCV@model[[1]]@model$params$interaction.depth
#bestShrinkage <- wallmartGBMModelCV@model[[1]]@model$params$shrinkage
bestInteraction.depth <- 11
bestShrinkage <- 0.001

#Remove Grid Search model
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Reload R data into h2o
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#h2o.ai GBM Modelling    
wallmartGBMModel <- h2o.gbm(x = validColumns, y = "units",
                            data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]]), ],
                            distribution = "gaussian",
                            interaction.depth = bestInteraction.depth,
                            shrinkage = bestShrinkage,                           
                            n.trees = 3000)

#Regression Prediction 
predictionGBMValidation <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTrain[dataSplits[[3]]]))[, 1]
#Round negative values to zero
predictionGBMValidation[predictionGBMValidation < 0] <- 0

print(paste0("GBMs model built")) 

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

#Evaluate Model against known data
constantValuesIncluded <- sample(trainConstant$units, floor(nrow(trainConstant) * 0.2))
actualAmountOfUnits <- as.data.frame(trainWithWeather[, "units", with = FALSE])[dataSplits[[3]], 1]

testrmsleGBM <- rmsle(c(rep(0, floor(nrow(trainConstant) * 0.2)), predictionGBMValidation), 
                      c(constantValuesIncluded, actualAmountOfUnits))
print(paste0("testNWMSE error of: ", testrmsleGBM))
save(testrmsleGBM, file = "testrmsleGBM.RData")

##Train full Model
#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

validColumns <- c("store_nbr", "item_nbr", "unitsProjected", "availableUnitsLogical",
                  weatherValidColumns[c(-which(weatherValidColumns == "date"), -which(weatherValidColumns == "codesum"))])

#R data.table to h2o.ai
h2oWallmartTrain <- as.h2o(h2oServer, trainWithWeather)

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
  h2oWallmartTrain[, columnName] <- as.factor(h2oWallmartTrain[, columnName])
}

#h2o.ai GBM Modelling    
wallmartGBMModel <- h2o.gbm(x = validColumns, y = "units",
                            data = h2oWallmartTrain[c(dataSplits[[1]], dataSplits[[2]], dataSplits[[3]]), ],
                            distribution = "gaussian",
                            interaction.depth = bestInteraction.depth,
                            shrinkage = bestShrinkage,                           
                            n.trees = 3000)

#Save model
h2o.saveModel(wallmartGBMModel, dir = dataDirectory, name = "GBMFullModel", save_cv = FALSE, force = FALSE)
#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])    

print(paste0("GBM model built")) 

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

##Make a submission file .csv / .zip
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#Start h2o from command line
system(paste0("java -Xmx6G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
#Small pause
Sys.sleep(3)
#Connect R to h2o
h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)

#R data.table to h2o.ai
h2oWallmartTest <- as.h2o(h2oServer, testWithWeather)

#Load h2o Model
wallmartGBMModel <- h2o.loadModel(h2oServer, path = file.path(dataDirectory, "GBMFullModel"))

#Factor columns as h2o factors
for (columnName in c("station_nbr", "item_nbr", "store_nbr", "year", "month")){
  h2oWallmartTest[, columnName] <- as.factor(h2oWallmartTest[, columnName])
  h2oWallmartTest[, columnName] <- as.factor(h2oWallmartTest[, columnName])
}

#Regression Prediction 
predictionGBM <- as.data.frame(h2o.predict(wallmartGBMModel, newdata = h2oWallmartTest))[, 1]
#Round negative values to zero
predictionGBM[predictionGBM < 0] <- 0

#Clear Server
h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])   

#Shutdown h20 server
h2o.shutdown(h2oServer, prompt = FALSE)

print(paste0("GBM predictions ready"))

variablePredictionsIds <- paste(testWithWeather$store_nbr, testWithWeather$item_nbr, testWithWeather$date, sep = "_")
variablePredictions <- predictionGBM
constantPredictionsIds <- paste(testConstant$store_nbr, testConstant$item_nbr, testConstant$date, sep = "_")
constantPredictions <- rep(0, nrow(testConstant))

sampleSubmissionFile$id <- c(variablePredictionsIds, constantPredictionsIds)
sampleSubmissionFile$units <- c(variablePredictions, constantPredictions)

#Write File
write.csv(sampleSubmissionFile, file = "GBMWeatherNoNaNo5.csv", row.names = FALSE)
system('zip GBMWeatherNoNaNo5.zip GBMWeatherNoNaNo5.csv')
 