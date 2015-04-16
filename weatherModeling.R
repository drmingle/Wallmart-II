weatherModeling <- function(weatherDT, col2Predict){
  
  #Libraries
  require("h2o")
  require("ggplot2")
  
  #Functions
  source(file.path(workingDirectory, "normalOutliersFinder.R"))
  
  #Detect available cores
  numCores <- detectCores()
  
  weatherNAs <- is.na(weatherDT)
  columnsWithNAs <- apply(weatherNAs, 2, function(weatherCol){
    return(ifelse(sum(weatherCol) > 0, TRUE, FALSE))
  })
  
  weatherWithoutNAs <- cbind(weatherDT[, col2Predict, with = FALSE],
                             weatherDT[, names(columnsWithNAs)[!columnsWithNAs], with = FALSE])
  
  validColumns <- names(weatherWithoutNAs)[c(-which(names(weatherWithoutNAs) == col2Predict),
                                             -which(names(weatherWithoutNAs) == "date"), 
                                             -which(names(weatherWithoutNAs) == "station_nbr"), 
                                             -which(names(weatherWithoutNAs) == "codesum"))]
  
  validIdx <- which(!is.na(as.data.frame(weatherWithoutNAs[, col2Predict, with = FALSE])[, 1]))  
  targetNAIdx <- which(is.na(as.data.frame(weatherWithoutNAs[, col2Predict, with = FALSE])[, 1]))
  
  ##Start h2o from command line
  system(paste0("java -Xmx5G -jar ", h2o.jarLoc, " -port 54333 -name WallmartII -single_precision &"))
  #Small pause
  Sys.sleep(3)
  #Connect R to h2o
  h2oServer <- h2o.init(ip = "localhost", port = 54333, nthreads = -1)
  #R data.table to h2o.ai
  h2oWeatherNoNAs <- as.h2o(h2oServer, weatherWithoutNAs)
  #Cross Validation
  weatherGBMCV <-  h2o.gbm(x = validColumns, y = col2Predict,
                           data = h2oWeatherNoNAs[validIdx, ],
                           nfolds = 5,
                           distribution = "gaussian",
                           interaction.depth = c(7, 10),
                           shrinkage = c(0.001, 0.003),                           
                           n.trees = 300,
                           importance = TRUE,                           
                           grid.parallelism = numCores)
  
  #Save importance plot
  GBMImportanceDf <- as.data.frame(weatherGBMCV@model[[1]]@model$varimp$Percent.Influence)
  GBMImportanceDf$variables <- rownames(weatherGBMCV@model[[1]]@model$varimp)
  names(GBMImportanceDf) <- c("PercentInfluence", "variables")
  ggplot(data = GBMImportanceDf, aes(x = variables, y = PercentInfluence, fill = variables)) + geom_bar(stat = "identity")
  
  #Best Hyperparameters
  bestInteraction.depth <- weatherGBMCV@model[[1]]@model$params$interaction.depth
  bestShrinkage <- weatherGBMCV@model[[1]]@model$params$shrinkage
  
  #h2o.ai GBM Modelling    
  weatherGBM <- h2o.gbm(x = validColumns, y = col2Predict,
                        data = h2oWeatherNoNAs[validIdx, ],
                        distribution = "gaussian",
                        interaction.depth = bestInteraction.depth,
                        shrinkage = bestShrinkage,                           
                        n.trees = 4000)
  
  #Regression Prediction 
  weatherTargetPrediction <- as.data.frame(h2o.predict(weatherGBM, newdata = h2oWeatherNoNAs[targetNAIdx, ]))[, 1]
  
  weatherPredictedColumn <- as.data.frame(weatherDT)[, col2Predict]
  weatherPredictedColumn[targetNAIdx] <- signif(weatherTargetPrediction, digits = 5)
  
  #Clear Server
  h2o.rm(object = h2oServer, keys = h2o.ls(h2oServer)[, 1])     
  #Shutdown h20 server
  h2o.shutdown(h2oServer, prompt = FALSE)
  
  return(weatherPredictedColumn)
}