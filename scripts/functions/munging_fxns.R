#--------------------------------------------#
# Data munging functions (those not included with tagtales)
# Myfanwy Johnston
# Wed Dec 18 10:48:40 2019 ------------------------------

# required packages:
library(lubridate)
library(dplyr)
library(data.table)

#--------------------------------------------#
# Vemco-tidying functions
#--------------------------------------------#
# parse receiver column:
parse_receiver_col = function(df, reccol = "Receiver") {
  names(df)[names(df) == reccol] <- "SepRec" # rename the old combined tagid col
  out <- as.data.frame(do.call
                       (rbind, strsplit(as.character(df$SepRec),'-')),
                       stringsAsFactors = FALSE)

  colnames(out) <- c("freq", "Receiver")
  final <- cbind(df, out)
  drops <- c("freq", "SepRec")
  final <- final[ , !names(final) %in% drops]
  final$Receiver <- as.integer(final$Receiver)
  return(final)
}

# parse tagid col:
parse_tagid_col = function(df, tagcol = "TagID") {
  names(df)[names(df) == tagcol] <- "SepTagID" # rename the old combined tagid col
  out <- as.data.frame(do.call(rbind, strsplit(as.character(df$SepTagID),'-')),
                       stringsAsFactors = FALSE)

  colnames(out) <- c("freq", "CodeSpace", "TagID")
  final <- cbind(df, out)
  drops <- c("freq", "SepTagID")
  final <- final[ , !names(final) %in% drops]
  final$CodeSpace <- as.integer(final$CodeSpace)
  final$TagID <- as.integer(final$TagID)
  return(final)
}

#--------------------------------------------#
# Functions to format exported tables:
#--------------------------------------------#
# All TagIDs
format_tags = function(tags_df) {
  tags_df %>% 
    mutate(DateTagged = as.Date(DateTagged)) -> tags_dff
  return(tags_dff)
}

# Deployments
format_deps = function(deps_df) {
  deps_df %>%
    select(StationAbbOld,
           Station,
           Receiver,
           Start = DeploymentStart,
           End = DeploymentEnd) %>%
    mutate(
      Start = force_tz(ymd_hms(Start), "Pacific/Pitcairn"),
      End = force_tz(ymd_hms(End), "Pacific/Pitcairn")
    ) -> deps_dff
  
  deps_dff = deps_dff[!is.na(deps_dff$End),] # filter out the ragged last cell
  return(deps_dff)
}

# Detections; depends on alltags df
format_dets = function(dets_df) {
  dets_df %>%
    filter(TagID %in% alltags$TagID) %>% # filter down to just our fish
    mutate(DateTimePST = ymd_hms(DateTimePST)) %>%
    arrange(DateTimePST) -> dets_dff
  
  dets_dff$DateTimePST <- force_tz(dets_dff$DateTimePST, "Pacific/Pitcairn")

  return(dets_dff)
}

#--------------------------------------------#
# functions to check exported tables

