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

# bard
format_bard_deps = function(bard_deps_df) {
  bard_deps_df %>% 
  select(Station = Location,
         Start,
         End = Stop, 
         rkms = RiverKm) %>% 
    mutate(Start = ymd_hms(Start),
           End = ymd_hms(End)) -> bard_deps_df
  
  bard_deps_df = bard_deps_df[!is.na(bard_deps_df$End), ] # get rid of ragged ends
  
  return(bard_deps_df)
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

format_bard_dets = function(bard_dets_df) {
  bard_dets_df %>%
    mutate(DateTimePST = with_tz(DetectDate, "Pacific/Pitcairn")) %>% 
    filter(TagID %in% alltags$TagID) %>% # filter down to just our fish
    mutate(DateTagged = as.Date(DateTagged)) %>%
    select(TagID, DateTimePST, Receiver, Station, DateTagged, rkms = RiverKm) %>% 
    arrange(DateTimePST) -> bard_dets_dff
    
    bard_dets_dff <- data.frame(bard_dets_dff)
  
  return(bard_dets_dff)
}

#--------------------------------------------#
# functions to check exported tables

# check for duplicated detections within receivers
rm_dup_dets_within_recs = function(dets_df) {
    d = dets_df
    i = duplicated(d[, c("TagID", "Receiver", "DateTimePST")])
    d2 = d[!i,]
    stopifnot(nrow(d) - nrow(d2) == sum(i)) # check that it removed the correct # of rows
    stopifnot(sum(duplicated(d2[, c("TagID", "Receiver", "DateTimePST")])) == 0) # final check for duplicated detections within tags at individual receivers
    return(d2)
}


# subsets dataframe to desired study period
subset_to_study_period <- function(df, start, end) {
  dff = df[df$DateTimePST > start & df$DateTimePST < end, ]
  return(dff)
}

get_dup_deps <- function(dep_df)
{
   tt = tapply(dep_df$Station, dep_df$Receiver, function(x) length(unique(x)) > 1)
   as.numeric(names(tt)[tt])
}

# associate correct receivers with correct station names
#--------------------------------------------#
get_stations <- function(detections_df, deployments_df) {
  x = as.data.table(detections_df)
  x[ , end := DateTimePST]
  y = as.data.table(deployments_df)
  setkey(y, Receiver, Start, End)
  result = foverlaps(x, y, by.x = c('Receiver', 'DateTimePST', 'end'), type = 'within')
  result[ , end := NULL]
  result <- as.data.frame(result)
  if(sum(is.na(result$Start)) > 0){
    warning("warning: the resulting dataframe contains NAs in the joining columns - check for orphan detections")
    }
  return(result)
}

# Group receivers at gated locations
group_stations <- function(dets_df) {
  dets_df %>% 
  mutate(GroupedStn = case_when(
    StationAbbOld == "BCE" ~ "BCN",
    StationAbbOld == "BCW" ~ "BCN",
    StationAbbOld == "BCE2" ~ "BCS",
    StationAbbOld == "BCW2" ~ "BCS",
    StationAbbOld == "Abv_rstr" ~ "YBRSTR",
    TRUE ~ Station
  )) -> dets_dff
  
  return(dets_dff)
}

# discard simultaneous detections at grouped locations
discard_simuls <- function(dets_df) {
  
simuls = dets_df %>% 
  group_by(TagID, GroupedStn) %>% 
  filter(duplicated(DateTimePST)) %>% 
  ungroup() %>% 
  filter(GroupedStn %in% c("BCS", "BCN", "YBRSTR")) 

d3 = anti_join(dets_df, simuls) # filter out simultaneous detections within tags
stopifnot(nrow(dets_df) - nrow(d3) == nrow(simuls))
return(d3)

}

handle_redeployed_tag <- function(dets_df){
  
  # redeployed tag
  recap = dets_df %>% 
    filter(TagID == 45451) %>% 
    arrange(DateTimePST)
  
  # isolate tag's 2nd deployment dets
  recap = filter(recap, DateTimePST >= alltags$DateTagged[alltags$TagID == 55555]) 

# remove detections from re-deployed fish
  d4 = anti_join(dets_df, recap)
  recap$TagID = 55555 # assign new tagID
  d5 = bind_rows(d4, recap) # back we go
  return(d5)
}

check_dets_against_tagdates = function(dets_df) {
  
dets_df %>% 
  left_join(select(alltags, TagID, DateTagged)) %>% 
  group_by(TagID) %>% 
  filter(DateTimePST < as.POSIXct(DateTagged)) %>% 
  ungroup() -> falsedets

falsedets %>% 
  group_by(TagID) %>% 
  summarise(DateTagged = unique(DateTagged),
            mindet = min(DateTimePST),
            maxdet = max(DateTimePST)) -> falsedetcheck

print(falsedetcheck)

falsedets %>% 
  group_by(TagID) %>% 
  tally() -> falsetally

print(falsetally)

d6 <- anti_join(dets_df, falsedets)

stopifnot(nrow(dets_df) - nrow(d6) == nrow(falsedets))

return(d6)

}

#--------------------------------------------#
# add in river kilometer info for grouped stn; requires stns has been loaded
add_rkms = function(dets_df) {
  dets_dff = left_join(dets_df, stns)
  return(dets_dff)
}
