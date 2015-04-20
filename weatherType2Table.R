weatherType2Table <- function(weatherSting){
  
  types2check <- c("TS", "GR", "RA", "DZ", "SN", "SG", "GS", "PL", "FG", "BR", "UP", "HZ", "FU", "DU",
                   "SS", "SQ", "FZ", "MI", "PR", "BC", "BL", "VC")
  logicalString <- sapply(types2check, function(type){
    return(grepl(type, weatherSting))
  })
  return(logicalString)
} 