weatherCorrelations <- function(col2correlate, weatherDT){
  
  #Libraries
  require("h2o")
  require("ggplot2")
  require("leaps")
  
  #Linear Feature Selection------------
  #Exclude codsums and dates
  validColumns <- names(weatherDT)[c(-which(names(weatherDT) == "date"), 
                                     -which(names(weatherDT) == "station_nbr"), 
                                     -which(names(weatherDT) == "codesum"))]
  
  #Valid rows  
  validRowsTrain <- which(complete.cases(weatherDT))
  
  linMatrixData <- as.data.frame(weatherDT)[validRowsTrain, validColumns]
  
  for (columnName in validColumns[c(-which(validColumns == "year"), 
                                    -which(validColumns == "month"))]){
    linMatrixData[, columnName] <- as.numeric(linMatrixData[, columnName])
  }
  
  for (columnName in validColumns[c(which(validColumns == "year"), 
                                    which(validColumns == "month"))]){
    linMatrixData[, columnName] <- as.factor(linMatrixData[, columnName])
  }
    
  #Transform Data to a matrix
  linMatrixData <- model.matrix(~ . , data = linMatrixData)
  #Find best linear combination of features
  linearBestModels <- regsubsets(x = linMatrixData[, c(-1, -which(colnames(linMatrixData) == col2correlate))],
                                 y = as.numeric(linMatrixData[, col2correlate]), 
                                 method = "forward", nvmax = 80)
  
  #Plot the best number of predictors
  bestMods <- summary(linearBestModels)
  bestNumberOfPredictors <- which.min(bestMods$cp)
  plot(bestMods$cp, xlab="Number of Variables", ylab="CP Error", main ="Best Predictors")
  points(bestNumberOfPredictors, bestMods$cp[bestNumberOfPredictors], pch=20, col="red")
  #Save Plot
  dev.print(file = paste0("ImportanceGLM", col2correlate), device = png, width = 1200)
  
  #Name of the most predictive rankings
  predictors1 <- as.data.frame(bestMods$which)
  predictors <- names(sort(apply(predictors1[, -1], 2, sum), decreasing = TRUE)[1:bestNumberOfPredictors])  
    
  rm(linMatrixData, linearBestModels, bestMods, bestNumberOfPredictors, predictors1)
  
  ##GBM Variable Imporance 
  #Define Valid Data
  weatherValid <- as.data.frame(weatherDT)[validRowsTrain, validColumns]
  
  ##Start h2o from command line
  system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII &"))
  #Small pause
  Sys.sleep(3)
  #Connect R to h2o
  h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)
  #R data.table to h2o.ai
  h2oWeatherNoNAs <- as.h2o(h2oServer, weatherValid)
  #Factor columns as h2o factors
  for (columnName in c("year", "month")){
    h2oWeatherNoNAs[, columnName] <- as.factor(h2oWeatherNoNAs[, columnName])
    h2oWeatherNoNAs[, columnName] <- as.factor(h2oWeatherNoNAs[, columnName])
  }
  #Cross Validation
  weatherGBMCV <-  h2o.gbm(x = validColumns[-which(validColumns == col2correlate)], y = col2correlate,
                           data = h2oWeatherNoNAs,
                           nfolds = 5,
                           distribution = "gaussian",
                           interaction.depth = c(4, 7, 10),
                           shrinkage = 0.003,                           
                           n.trees = 350,
                           importance = TRUE,                           
                           grid.parallelism = numCores)
  
  
  GBMImportanceDf <- as.data.frame(weatherGBMCV@model[[1]]@model$varimp$Percent.Influence)
  GBMImportanceDf$variables <- rownames(weatherGBMCV@model[[1]]@model$varimp)
  names(GBMImportanceDf) <- c("PercentInfluence", "variables")
  print(ggplot(data = GBMImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity"))
  #Small pause
  Sys.sleep(3)
  #Save Plot
  dev.print(file = paste0("ImportanceGBM", col2correlate), device = png, width = 1200)
  
  
  gbmPredictors <- GBMImportanceDf$variables[GBMImportanceDf$PercentInfluence > 0]
  
  #Clear Server
  h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])     
  #Shutdown h20 server
  h2o.shutdown(h2oServer, prompt = FALSE)

  return(list(predictors, gbmPredictors))
}