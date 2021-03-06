#--------------------------------------------#
# BARD tidying functions
#--------------------------------------------#
# required packages:
library(lubridate)
library(dplyr)
library(data.table)

#deployments
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

# detections
format_bard_dets = function(bard_dets_df) {
  bard_dets_df %>%
    mutate(DateTimePST = force_tz(ymd_hms(DateTimePST), "Pacific/Pitcairn"),
           RiverKm = as.numeric(RiverKm)) %>% 
    filter(TagID %in% alltags$TagID) %>% # filter down to just our fish
    select(TagID, DateTimePST, Receiver, Station = DetectionLocation, rkms = RiverKm,
           Deployment_start, Deployment_end) %>% 
    arrange(DateTimePST) -> bard_dets_dff
    
    bard_dets_dff <- data.frame(bard_dets_dff)
  
  return(bard_dets_dff)
}

#nullrecs = unique(bard_dets1$Receiver[is.na(bard_dets1$rkms)])
# bc = sort(unique(bard_dets1$Station))
# bc[grep("YB", bc)] #YB names

rm_redundant_yb_dets = function(bard_dets_df) {
  
  ybdups = c(
    "YB_AbvLisbonWr",
    "YB_AbvRotScrwTrp",
    "YB_AbvSwanston",
    "YB_BCE",
    "YB_BCE2",
    "YB_BCW",
    "YB_BCW2",
    "YB_CacheCk",
    "YB_KnaggsRnch",
    "YB_LeveeMarker",
    "YB_LisbonWr",
    "YB_RotScrwTrp",
    "YB_Swanston",
    "YB_ToeDrain_Base",
    "YB_WallaceWr"
  )
  
  bard_dets_df %>% 
    filter(!(Station %in% ybdups)) -> bard_dets_dff
  return(bard_dets_dff)
  
}


BARD_group_stns_AND_rm_simuls = function(bard_dets_df) {

bard_dets_df %>%
    group_by(rkms) %>%
    mutate(nstns = len(Station),
           GroupedStn = Station[1]) %>% # just pulls the first station name at that river kilometer
    filter(!duplicated(GroupedStn)) %>%
    arrange(desc(nstns)) %>%
    select(GroupedStn, rkms, nstns) -> bd_grouped # locations that need to be grouped or removed
  
  test = left_join(bard_dets_df, bd_grouped)
  # remove simultaneous dets at grouped stns
  test2 = test %>%
    group_by(TagID, GroupedStn) %>%
    filter(!duplicated(DateTimePST)) %>%
    ungroup()
  
  return(test2)
  
}
  

BARD_fix_NULL_values = function(bard_dets_df) {
  
  # remove stations where we have no rkms: 109544
  rm_SPcontrol = bard_dets_df[bard_dets_df$Receiver == 109544 & bard_dets_df$Station == "NULL", ]
  bard_dets_df = anti_join(bard_dets_df, rm_SPcontrol)
  
  # manually assign correct stations/rkms to tagids with NULL values:
  
  #56483
  bard_dets_df$rkms[(bard_dets_df$TagID == 56483 | bard_dets_df$TagID == 56490) & 
                      bard_dets_df$Receiver == 101256] <- 102.75
  bard_dets_df$Station[(bard_dets_df$TagID == 56483 | bard_dets_df$TagID == 56490) & 
                         bard_dets_df$Receiver == 101256 & bard_dets_df$Station == "NULL"] <- "Santa_Clara_Shoals1_N"
  
  #2880:
  bard_dets_df$rkms[(bard_dets_df$TagID == 2880) & bard_dets_df$Receiver == 104440 &
                      bard_dets_df$Station == "NULL"] <- 37.451
  bard_dets_df$Station[(bard_dets_df$TagID == 2880) & bard_dets_df$Receiver == 104440 &
                      bard_dets_df$Station == "NULL"] <- "SF9_SW"
  bard_dets_df$rkms[(bard_dets_df$TagID == 2880) & bard_dets_df$Receiver == 104441 &
                      bard_dets_df$Station == "NULL"] <- 38.151
  bard_dets_df$Station[(bard_dets_df$TagID == 2880) & bard_dets_df$Receiver == 104440 &
                      bard_dets_df$Station == "NULL"] <- "SF9_NE"
  
  return(bard_dets_df)
  
}

#--------------------------------------------#
# join with bard

join_with_bard <- function(dets_df, bard_dets_df) {
  
  bard_dets_df  = select(bard_dets_df, 
                       TagID,
                       DateTimePST,
                       Receiver,
                       GroupedStn,
                       rkms)
  
dets_df = dets_df[ , c("TagID", "DateTimePST", "Receiver", "GroupedStn", "rkms")]

dets_dff = bind_rows(dets_df, bard_dets_df)

cs_index = select(alltags, TagID, CodeSpace)

dets_dff = left_join(dets_dff, cs_index) # get back codespace
  
return(dets_dff)

}
