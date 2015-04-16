weatherType2Table <- function(weatherSting){
  
  types2check <- c("FC", "TS", "GR", "RA", "DZ", "SN", "SG", "GS", "PL", "IC", "FG+", "FG", "BR", "UP", "HZ", "FU", 
                     "VA", "DU", "DS", "PO", "SA", "SS", "PY", "SQ", "DR", "SH", "FZ", "MI", "PR", "BC", "BL", "VC")
  logicalString <- sapply(types2check, function(type){
    return(grepl(type, weatherSting))
  })
  return(logicalString)
} 