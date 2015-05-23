availabilityModeling <- function(trainDt, testDt){
  
  #This function calculates the Arima autocorrelation baseline prediction for test data to use as a baseline
  #and a reference for available items in the store
  require("data.table")
  require("zoo")
  require("prospectr")  
  
  desiredFilterWindow <- 5
  
  itemsGrid <- unique(rbind(trainDt[, .(store_nbr, item_nbr)], testDt[, .(store_nbr, item_nbr)]))
  
  #Search all combinations and model them
  #Arima Model
  availabilityDt <- apply(itemsGrid, 1, function(combination, windowValue){
    
    item <- combination[[2]]
    store <- combination[[1]]
    
    itemStoreDataTableTrain <- trainDt[store_nbr == store & item_nbr == item, .(date, units)]
    itemStoreDataTableTest <- testDt[store_nbr == store & item_nbr == item, .(date)]
    itemStoreDataTableTest$units <- rep(NA, length(itemStoreDataTableTest))
    
    fullTimeSeries <- rbind(itemStoreDataTableTrain, itemStoreDataTableTest)  
    
    fullTimeSeries <- fullTimeSeries[order(fullTimeSeries$date)]
    
    #Determine availability of items    
    datesFormated <- as.Date(fullTimeSeries$date, format = "%Y-%m-%d")
    zooTs <- zoo(fullTimeSeries$units, order.by = datesFormated)
    zooTs <- na.fill(zooTs, "extend")      
    fullTimeSeries$unitsProjected <- as.numeric(zooTs)
    filteredVector <- movav(fullTimeSeries[, unitsProjected], w = windowValue)
    fullTimeSeries$unitsProjected <- c(rep(filteredVector[1], (windowValue - 1) / 2),
                                       filteredVector, 
                                       rep(filteredVector[length(filteredVector)], (windowValue - 1) / 2))
    fullTimeSeries$availableUnitsLogical[fullTimeSeries$unitsProjected > 0] <- TRUE
    fullTimeSeries$availableUnitsLogical[fullTimeSeries$unitsProjected == 0] <- FALSE
    
    fullTimeSeries$store_nbr <- store
    fullTimeSeries$item_nbr <- item
    
    print(combination)
    
    return(fullTimeSeries)
  }, windowValue = desiredFilterWindow)
  
  availabilityDt <- do.call(rbind, availabilityDt)
  
  availabilityDt <- availabilityDt[, .(store_nbr, item_nbr, date, unitsProjected, availableUnitsLogical)]
  
  return(availabilityDt)
  
}

  
