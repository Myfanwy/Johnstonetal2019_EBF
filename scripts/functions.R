#############
# functions #--------------------------------#
#############

#--------------------------------------------#
# for converting parm estimates from log-odds to probability:
logistic <- function (x) 
{
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
}

#--------------------------------------------#
# for finding final detection locations:
firstlastOneFish <- function(x, dtc2 = datetimecol) {
  
  #testing
#   x = chn
#   dtc2 = "DateTimePST"
  
  x$DateTimeCol = x[[dtc2]]
  x = x[order(x$DateTimeCol), ]
  
  FirstRow = x[which.min(x$DateTimeCol), ]  # subset to the first departure
  LastRow = x[which.max(x$DateTimeCol), ] # subset to the last arrival/departure combo

  data.frame(
    TagID = x$TagID[1],
    FirstStation = FirstRow$Station,
    FirstDetection = FirstRow$DateTimeCol,
    LastStation = LastRow$Station,
    LastDetection = LastRow$DateTimeCol,
    
       stringsAsFactors = FALSE
  )
}

FirstLast <- function(df, datetimecol = "DateTimeUTC") {
  do.call(rbind, lapply(split(df, df$TagID), firstlastOneFish, dtc2 = datetimecol))
}


# fishpaths fxn source: (soon to be deprecated) fishtrackr package: github.com/Myfanwy/fishtrackr
#--------------------------------------------#
fishpaths <- function(x, TagID_col, Station_col, Datetime_col = "DateTimeUTC", 
    Threshold = 60 * 60) 
{
    f1 <- split(x, list(TagID_col, Station_col))
    f1 <- f1[sapply(f1, nrow) > 0]
    tmp = lapply(f1, splitFishStationVisits, dtc2 = Datetime_col, 
        TimeThreshold = Threshold)
    fishpaths = do.call(rbind, tmp)
}

splitFishStationVisits <- function(d, TimeThreshold = Threshold, rowFunc = redRowFun, 
    dtc2 = Datetime_col) 
{
    d = d[order(d[[dtc2]]), ]
    g = cumsum(c(0, diff(d[[dtc2]])) > TimeThreshold)
    ans = by(d, g, rowFunc, dtc1 = dtc2)
    do.call(rbind, ans)
}

redRowFun <- function(d, dtc1) 
{
    r = as.POSIXct(range(d[[dtc1]]))
    data.frame(d[1, ], arrival = r[1], departure = r[2], stringsAsFactors = FALSE)
}


#--------------------------------------------#
# join with bard

join_with_bard <- function(ybdf, barddf) {
  
  barddf = bardne; ybdf = ybne # testing
  barddf = data.frame(barddf); ybdf = data.frame(ybdf)
  str(barddf)  ; str(ybdf)
  names(barddf)
  names(ybdf)
  barddf = select(barddf, TagID, DateTagged, DateTimeUTC = DetectDate, Station, Rkm = RiverKm, Receiver)
  ybdf = select(ybdf, TagID, DateTagged, DateTimeUTC, Station, Rkm, Receiver, TagGroup, CodeSpace)
  
  }


# convenience functions
len <- function(x){length(unique(x))}
