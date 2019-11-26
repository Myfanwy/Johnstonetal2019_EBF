#--------------------------------------------#
#  chn
source("scripts/setup.R")

fl <- FirstLast(chn, dtc2 = "DateTimeUTC")
chnidx <- select(chn, TagID, TagGroup) %>% 
  filter(!duplicated(TagID))

fl <- left_join(fl, chnidx)

fl <- fl %>% 
  filter(TagGroup != "fca_2012") %>%  #filter out 2012 fish; not included in analyses
  group_by(TagID) %>% 
  mutate(ExitStatus = case_when(LastStation %in% exitstations ~ 1 , 
                            TRUE ~ 0)) %>% 
  ungroup() %>% 
  mutate(Detyear = 
           case_when(
    TagGroup == "fca_2013" ~ 2013,
    TagGroup == "fca_2014" ~ 2014,
    TagGroup == "fca_2015" ~ 2015,
    TagGroup == "fca_2016" ~ 2016,
    TRUE ~ 2017
                    ), 
        TagDetyear = Detyear,
        Species = "chn") %>% 
  select(TagID, Species, Detyear, TagDetyear, ExitStatus)

chn_exits_final <- fl

saveRDS(chn_exits_final, "data/chn_exits_final.rds")

#-------------------------------------------------------#
# check to see if any of the fish that didn't exit were recorded on the core array somewhere outside the bypass

# categorizing final locations: in/out of Bypass
# cache slough complex
cacheslough <- c("BCE", "BCW", "BCE2", "BCW2", "BC_joint", "BC_joint2",
                 "YB_BCE"  ,      "YB_BCE2"  , "YB_BCW"  , "YB_BCW2"  )

# putah creek
putahcrk <- c("PutahCrk_mace"  ,  "PutahCrk_AbvChk" , "Putah_creek", "PutahCrk_spawn")

# locations in bypass
bypass <- c("YB_CacheCk", "YB_ToeDrain_Base" , "YB_WallaceWr" ,"YB_AbvLisbonWr", stations[1:26],
            "Lisbon_RT")
bypass <- bypass[!(bypass %in% putahcrk)]

bard <- readRDS("data/BARD_query_fcatags.rds") # query that Matt Pagel ran on 8/30/2018

ne <- filter(chn_exits_final, ExitStatus == 0) # no-exit

bard <- filter(bard, TagID %in% ne$TagID)

len(bard$TagID) # 22 yolo fish detected in BARD database

bard <- bard %>% 
  group_by(TagID) %>% 
  filter(!duplicated(DetectDate)) %>% 
  ungroup() %>% 
  rename(DateTimeUTC = DetectDate, Rkm = RiverKm)

bardp <- fishpaths(bard, "TagID", "Station", "DateTimeUTC")

bardfl <- FirstLast(bard)
bardfl <- filter(bardfl, !(LastStation %in% bypass), !(LastStation %in% cacheslough))
bardfl # 2 fish with "no exit" were detected on BARD array; check on their last detections in YB array - were they before or after the BARD detections?

chnp <- fishpaths(chn, "TagID", "Station", "DateTimeUTC")

ne_paths <- filter(chnp, TagID %in% c(31567, 37845)) %>% # 31567 last detected at I80; 37845 last detected at cache creek
  arrange(TagID, DateTimePST)


bardp %>% 
  filter(TagID %in% c(31567, 37845)) %>% 
  arrange(TagID, DateTimeUTC) %>% 
  View()

#  the riovista detection happened in the middle of its path; for the other fish, made it all the way to the Feather before turning around and heading BACK up the bypass; final detection at Knaggs in December; chn_exits_final ready to model.