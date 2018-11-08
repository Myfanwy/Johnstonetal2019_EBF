

firstlastOneFish <- function(x) {
  
  x = x[order(x$DateTimePST), ]
  
  FirstRow = x[which.min(x$DateTimePST), ]  # subset to the first departure
  LastRow = x[which.max(x$DateTimePST), ] # subset to the last arrival

  data.frame(
    TagID = x$TagID[1],
    
    FirstStation = FirstRow$Station,
    LastStation = LastRow$Station,
    
   # ttime = (as.numeric(LastRow$arrival) - as.numeric(FirstRow$departure))/(60*60*24),
    
    reachdistance = FirstRow$Rkm - LastRow$Rkm,
    
    stringsAsFactors = FALSE
  )
  
}


FirstLast <- function(df) {
  
  do.call(rbind, lapply(split(df, df$TagID), firstlastOneFish))
  
}
