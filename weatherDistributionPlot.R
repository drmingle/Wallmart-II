weatherDistributionPlot <- function(weatherDf){
  
  originalTmax <- ggplot(data = weatherDf, aes(x = tmax)) + geom_bar() 
  originalTmin <- ggplot(data = weatherDf, aes(x = tmin)) + geom_bar() 
  originalTavg <- ggplot(data = weatherDf, aes(x = tavg)) + geom_bar() 
  originalDepart <- ggplot(data = weatherDf, aes(x = depart)) + geom_bar() 
  originalDewpoint <- ggplot(data = weatherDf, aes(x = dewpoint)) + geom_bar() 
  originalWetbulb <- ggplot(data = weatherDf, aes(x = wetbulb)) + geom_bar() 
  originalHeat <- ggplot(data = weatherDf, aes(x = heat)) + geom_bar() 
  originalCool <- ggplot(data = weatherDf, aes(x = cool)) + geom_bar() 
  originalSunrise <- ggplot(data = weatherDf, aes(x = sunrise)) + geom_bar() 
  originalSnowfall <- ggplot(data = weatherDf, aes(x = snowfall)) + geom_bar() 
  originalPreciptotal <- ggplot(data = weatherDf, aes(x = preciptotal)) + geom_bar() 
  originalStnpressure <- ggplot(data = weatherDf, aes(x = stnpressure)) + geom_bar() 
  originalSealevel <- ggplot(data = weatherDf, aes(x = sealevel)) + geom_bar() 
  originalResultspeed <- ggplot(data = weatherDf, aes(x = resultspeed)) + geom_bar() 
  originalResultdir <- ggplot(data = weatherDf, aes(x = resultdir)) + geom_bar() 
  originalAvgspeed <- ggplot(data = weatherDf, aes(x = avgspeed)) + geom_bar()
  
  multiplot(originalTmax, originalTmin, originalTavg, originalDepart, 
            originalDewpoint, originalWetbulb, originalHeat, originalCool, originalSunrise, 
            originalSnowfall, originalPreciptotal, originalStnpressure, originalSealevel, 
            originalResultspeed, originalResultdir, originalAvgspeed, cols = 3)
  
  originalTmax <- ggplot(data = originalWeather, aes(x = tmax)) + geom_bar() 
  originalTmin <- ggplot(data = originalWeather, aes(x = tmin)) + geom_bar() 
  originalTavg <- ggplot(data = originalWeather, aes(x = tavg)) + geom_bar() 
  originalDepart <- ggplot(data = originalWeather, aes(x = depart)) + geom_bar() 
  originalDewpoint <- ggplot(data = originalWeather, aes(x = dewpoint)) + geom_bar() 
  originalWetbulb <- ggplot(data = originalWeather, aes(x = wetbulb)) + geom_bar() 
  originalHeat <- ggplot(data = originalWeather, aes(x = heat)) + geom_bar() 
  originalCool <- ggplot(data = originalWeather, aes(x = cool)) + geom_bar() 
  originalSunrise <- ggplot(data = originalWeather, aes(x = sunrise)) + geom_bar() 
  originalSnowfall <- ggplot(data = originalWeather, aes(x = snowfall)) + geom_bar() 
  originalPreciptotal <- ggplot(data = originalWeather, aes(x = preciptotal)) + geom_bar() 
  originalStnpressure <- ggplot(data = originalWeather, aes(x = stnpressure)) + geom_bar() 
  originalSealevel <- ggplot(data = originalWeather, aes(x = sealevel)) + geom_bar() 
  originalResultspeed <- ggplot(data = originalWeather, aes(x = resultspeed)) + geom_bar() 
  originalResultdir <- ggplot(data = originalWeather, aes(x = resultdir)) + geom_bar() 
  originalAvgspeed <- ggplot(data = originalWeather, aes(x = avgspeed)) + geom_bar()
  
  multiplot(originalTmax, originalTmin, originalTavg, originalDepart, 
            originalDewpoint, originalWetbulb, originalHeat, originalCool, originalSunrise, 
            originalSnowfall, originalPreciptotal, originalStnpressure, originalSealevel, 
            originalResultspeed, originalResultdir, originalAvgspeed, cols = 3)
  
}
