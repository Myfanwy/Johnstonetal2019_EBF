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
    mutate(DateTimePST = with_tz(ymd_hms(DetectDate), "Pacific/Pitcairn"),
           RiverKm = as.numeric(RiverKm)) %>% 
    filter(TagID %in% alltags$TagID) %>% # filter down to just our fish
    select(TagID, DateTimePST, Receiver, Station, rkms = RiverKm) %>% 
    arrange(DateTimePST) -> bard_dets_dff
    
    bard_dets_dff <- data.frame(bard_dets_dff)
  
  return(bard_dets_dff)
}



rm_redundant_yb_dets = function(bard_dets_df) {
  
  ybdups = c("YB_AbvLisbonWr", "YB_AbvRotScrwTrp", "YB_AbvSwanston", "YB_BCE", 
"YB_BCE2", "YB_BCW", "YB_BCW2", "YB_CacheCk", "YB_KnaggsRnch", 
"YB_LeveeMarker", "YB_LisbonWr", "YB_RotScrwTrp", "YB_Swanston", 
"YB_ToeDrain_Base", "YB_WallaceWr")
  
  bard_dets_df %>% 
    filter(!(Station %in% ybdups)) -> bard_dets_dff
  return(bard_dets_dff)
  
}


BARD_group_stns_AND_rm_simuls = function(bard_dets_df) {

bard_dets_df %>%
    group_by(rkms) %>%
    mutate(nstns = len(Station),
           GroupedStn = Station[1]) %>%
    filter(!duplicated(GroupedStn)) %>%
    arrange(desc(nstns)) %>%
    select(GroupedStn, rkms, nstns) -> bd_grouped # 154 locations that need to be grouped or removed
  
  test = left_join(bard_dets_df, bd_grouped)
  # remove simultaneous dets at grouped stns
  test2 = test %>%
    group_by(TagID, GroupedStn) %>%
    filter(!duplicated(DateTimePST)) %>%
    ungroup()
  return(test2)
}

# Need to add NULL stations here:
# nr = wst_dets[wst_dets$GroupedStn == "NULL", ]
# # have receiver & GroupedStn, need rkms
# nr = nr[!duplicated(nr$Receiver), c("Receiver") ]
# nr = filter(nr, Receiver != 104440)
# nr %>% 
#   mutate(GroupedStn = 
#            case_when(Receiver == 104441 ~ "SF9_NE", # rkms in bard_deps
#                           Receiver == 101256 ~ "Santa_Clara_Shoals1_N", 
#                           Receiver == 109544 ~ "SR_SRWTPD_Old",
#                           TRUE ~ "YBBI80" #rkms in deps
#                           )  
#   ) -> nr

# pp = filter(wst_dets, !duplicated(GroupedStn)) %>% 
#   filter(GroupedStn %in% nr$GroupedStn) %>% 
#   select(GroupedStn, rkms)
# 
# px = filter(bard_deps, !duplicated(Station)) %>% 
#   filter(Station %in% nr$GroupedStn) %>% 
#   select(GroupedStn = Station, rkms) %>% 
#   mutate(rkms = as.numeric(rkms)) %>% 
#   bind_rows(pp) %>% 
#   filter(!duplicated(GroupedStn))
# 
# nrr = left_join(nr, px)
  

BARD_fix_NULL_values = function(bard_dets_df) {
  
  # remove stations where we have no rkms
  rm_SPcontrol = bard_dets_df[bard_dets_df$Receiver == 104440 & bard_dets_df$GroupedStn == "NULL", ]
  bard_dets_df = anti_join(bard_dets_df, rm_SPcontrol)
  
  # replace matching recs with true NAs:
  bard_dets_df$GroupedStn[bard_dets_df$GroupedStn == "NULL"] <- NA
  bard_dets_df$Station[bard_dets_df$Station == "NULL"] <- NA
  
  # make rkms key to bring in groupedstn names
  nrr = structure(
    list(
      Receiver = c(104441L, 101256L, 109544L),
      GroupedStn = c("SF9_SE",
                     "Santa_Clara_Shoals1_N", "SR_SRWTPD_E"),
      rkms = c(38.151, 102.75,
               191.514)
    ),
    class = "data.frame",
    row.names = c(NA,-3L)
  )
  
  bard_dets_dff = left_join(bard_dets_df, nrr)
  
  return(bard_dets_dff)
  
}

#--------------------------------------------#
# join with bard

join_with_bard <- function(dets_df, bard_dets_df) {
  
  bard_dets_df  = select(bard_dets_df, 
                       TagID,
                       DateTimePST,
                       Receiver,
                       GroupedStn = grp_name,
                       rkms)
  
dets_df = dets_df[ , c("TagID", "DateTimePST", "Receiver", "GroupedStn", "rkms")]

dets_dff = bind_rows(dets_df, bard_dets_df)

cs_index = select(alltags, TagID, CodeSpace)

dets_dff = left_join(dets_dff, cs_index) # get back codespace
  
return(dets_dff)

  }