arimaPrediction <- function(trainDt, testDt){
  
  #This function calculates the Arima autocorrelation baseline prediction for test data to use as a baseline
  #and a reference for available items in the store
  require("data.table")
  require("zoo")
  
  uniqueItems <- unique(trainDt$item_nbr)
  uniqueStores <- unique(trainDt$store_nbr)
  
  itemsGrid <- expand.grid(.item = uniqueItems,
                           .store = uniqueStores)
  
  #Search all combinations and model them
  #Arima Model
  availabilityDt <- apply(itemsGrid, 1, function(combination){
    
    item <- combination[[1]]
    store <- combination[[2]]
    
    itemStoreDataTableTrain <- trainDt[store_nbr == store & item_nbr == item, .(date, units)]
    itemStoreDataTableTest <- testDt[store_nbr == store & item_nbr == item, .(date)]
    itemStoreDataTableTest$units <- rep(NA, length(itemStoreDataTableTest))
    
    fullTimeSeries <- rbind(itemStoreDataTableTrain, itemStoreDataTableTest)  
    
    fullTimeSeries <- fullTimeSeries[order(fullTimeSeries$date)]
    
    #Determine availability of items
    if (var(itemStoreDataTableTrain$units) == 0){
      
      fullTimeSeries$units[is.na(fullTimeSeries[, units])] <- 0
      
    }else{
      
      datesFormated <- as.Date(fullTimeSeries$date, format = "%Y-%m-%d")
      zooTs <- zoo(fullTimeSeries$units, order.by = datesFormated)
      zooTs <- na.fill(zooTs, "extend")      
      fullTimeSeries$units <- as.numeric(zooTs)
      
    }
    
    return(fullTimeSeries)
  })
  
  availabilityDt <- do.call(rbind, availabilityDt)
  return(availabilityDt)
  
}

  
