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
