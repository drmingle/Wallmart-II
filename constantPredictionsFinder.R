constantPredictionsFinder <- function(trainDt){
  
  #This function finds the items that aren't available ever in a given store
  require("data.table")
  
  uniqueItems <- unique(trainDt$item_nbr)
  uniqueStores <- unique(trainDt$store_nbr)
  
  itemsGrid <- expand.grid(.item = uniqueItems,
                           .store = uniqueStores)
  
  #Search all combinations and model them
  #Arima Model
  availabilityDt <- apply(itemsGrid, 1, function(combination){
    
    item <- combination[[1]]
    store <- combination[[2]]
    
    itemStoreDataTableTrainUnits <- trainDt[store_nbr == store & item_nbr == item, .(units)]
    
    #Determine availability of items
    if(var(itemStoreDataTableTrainUnits) == 0){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  combinationsDt <- cbind(itemsGrid, availabilityDt)
  setnames(combinationsDt, ".item", "item_nbr")
  setnames(combinationsDt, ".store", "store_nbr")
  
  return(as.data.table(combinationsDt))
  
}


