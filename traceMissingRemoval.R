traceMissingRemoval <- function(weatherDatatable){ 
  
  #Libraries
  require("zoo")
  
  #Transformation to data frame
  weatherDatatable <- as.data.frame(weatherDatatable)
  
  #Define monotonous columns
  monotonous <- apply(weatherDatatable, 2, function(vector){
    return(!length(unique(vector)) > 1)
  })
  
  #Define date objects
  datesFormated <- as.Date(weatherDatatable[, "date"], format = "%Y-%m-%d")
    
  for (colName in names(weatherDatatable)[union(c(-1, -2, -13), -which(monotonous == TRUE))]){
    #Strip white spaces
    weatherDatatable[, colName] <-  gsub(" ", "", weatherDatatable[, colName])
    #Transform missing values and tracing values 
    tracesIdxs <- weatherDatatable[, colName] == "T"
    missingIdxs <- weatherDatatable[, colName] == "M"
    #Trace transformation
    tracesValue <- as.numeric(sort(unique(weatherDatatable[!(tracesIdxs | missingIdxs), colName]))[2]) / 2
    
    if (sum(tracesIdxs) > 0){
      weatherDatatable[tracesIdxs, colName]  <- tracesValue      
    }
    
    #Missing values to NAs    
    if (sum(missingIdxs) > 0){
      weatherDatatable[missingIdxs, colName] <- NA      
    }      
    
    #Transformation to numeric data
    weatherDatatable[, colName] <- as.numeric(weatherDatatable[, colName])
        
    #Make a zoo object
    zooColumn <- zoo(weatherDatatable[, colName], order.by = datesFormated)
    
    if (sum(is.na(zooColumn))){
      zooColumn <- na.spline(zooColumn)
    }
    
    #Return results as part of the data frame
    weatherDatatable[, colName] <- as.numeric(zooColumn)
    print(paste0(colName, " column processed"))
  }  
 
  return(weatherDatatable)
  
}