#-------------------------------------------------------#
# M. Johnston
# Functions for categorizing exit status 
# Fri Dec 27 13:17:16 2019 ------------------------------


first_last <- function(detdf, tagidcol = "TagID", datetimecol = "DateTimeUTC", stationcol = "Station") {

  
  # test:
  # detdf = chn_dets
  # tagidcol = "TagID"
  # datetimecol = "DateTimePST"
  # stationcol = "GroupedStn"
  f1 <- split(detdf, detdf[[tagidcol]])
  tmp <- lapply(f1, first_last_1fish, dtc2 = datetimecol, stnc2 = stationcol)
  fldf = do.call(rbind, tmp)
                 
  return(fldf) }
                 
first_last_1fish <- function(x, 
                             dtc2 = datetimecol, 
                             tagc = tagidcol,
                             stnc2 = stationcol) {

     x = x[order(x[[dtc2]]), ] # order by DateTime

     return(data.frame(

       TagID = as.numeric(unique(x[[tagc]])),

       first_det = min(x[[dtc2]]),

       first_stn = x[[stnc2]][x[[dtc2]] == min(x[[dtc2]])],

       last_det = max(x[[dtc2]]),

       last_stn = x[[stnc2]][x[[dtc2]] == max(x[[dtc2]])],

       stringsAsFactors = FALSE)
     )
}
