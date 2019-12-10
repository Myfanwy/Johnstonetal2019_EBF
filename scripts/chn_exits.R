#--------------------------------------------#
#  chn
source("scripts/setup.R")

fl <- first_last(chn, "TagID", "DateTimeUTC", "Station")
# detdf = chn
# tagidcol = "TagID"
# datetimecol = "DateTimeUTC"
# stationcol = "Station"
# 
# detdf$Station = detdf[[stationcol]]
# detdf$datetimecol = detdf[[datetimecol]]
# detdf$TagID = detdf[[tagidcol]]
# 
# newdf <-  do.call(rbind, lapply(split(detdf, detdf$TagID), function(x) {
# 
#      x = x[order(x$datetimecol), ]
# 
#      return(data.frame(
# 
#        TagID = as.numeric(unique(x$TagID)),
# 
#        first_det = min(x$datetimecol),
# 
#        first_stn = x$Station[x$datetimecol == min(x$datetimecol)],
# 
#        last_det = max(x$datetimecol),
# 
#        last_stn = x$Station[x$datetimecol == max(x$datetimecol)],
# 
#        stringsAsFactors = FALSE)
#      )
#    }
#   ))

chnidx <- select(chn, TagID, TagGroup) %>% 
  filter(!duplicated(TagID))

fl <- left_join(fl, chnidx)

fl <- fl %>% 
  filter(TagGroup != "fca_2012") %>%  #filter out 2012 fish; not included in analyses
  group_by(TagID) %>% 
  mutate(ExitStatus = case_when(last_stn %in% exitstations ~ 1 , 
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

head(fl)

sheds <- structure(list(TagID = c(13728, 13729, 20168, 20164, 2600, 2625, 
2619, 9986, 9973), DateTimePST_truncated = c("2015-01-01 0:00:00", 
"2014-12-04 15:23:06", "2014-11-16 21:26:10", "2014-12-23 4:04:55", 
"2017-10-16 20:51:12", "2017-12-26 13:06:10", "2017-11-20 7:42:04", 
"2018-12-03 6:37:13", "2018-12-02 10:30:08"), GroupedStn_truncated = c("BCN", 
"YBR22", "YBALW", "YBAWW", "YBRSTR", "YBAAG4", "YBBLW", "YBAWW", 
"YBBWW")), row.names = c(NA, -9L), class = "data.frame")

table(sheds$GroupedStn_truncated)
sheds

recovs = c(9970, 37835, 37845, 2639, 2596, 2593, 2603, 33937, 45433, 45429, 
45451, 31565, 13721, 33940)

sum(sheds$TagID %in% recovs)
intersect(sheds$TagID, recovs)

recovfps <- filter(chn, TagID %in% recovs)

recovfps %>% 
  filter(TagID %in% c(33940, 37835,37845)) %>% 
  ggplot(aes(x = DateTimePST, y = reorder(Station,Rkm))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~TagID)

flrecov <- first_last(recovfps)
flrecov$ExitStatus <- ifelse(flrecov$last_stn %in% c("BC_joint2","BC_joint", "Base_TD"), 1, 2)
flrecov$ExitStatus[flrecov$TagID == 37845] <- 1 # WW rescue

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

ne <- filter(chn_exits_final, ExitStatus == 0) # 64/215 fish were no-exit; these are the fish we have to check against the BARD database, to see if they were picked up anywhere outside the bypass.  Want to join their tracks with the bard tracks.

ybne <- filter(chn, TagID %in% ne$TagID)
bardne <- filter(bard, TagID %in% ne$TagID)

len(bardne$TagID) # 22 ne yolo fish detected in BARD database

# join together


bardp <- fishpaths(bard, "TagID", "Station", "DateTimeUTC")

bardfl <- FirstLast(bard)
bardfl <- filter(bardfl, !(LastStation %in% bypass), !(LastStation %in% cacheslough))
bardfl # 2 fish with "no exit" were detected on BARD array; check on their last detections in YB array - were they before or after the BARD detections?

chnp <- fishpaths(chn, "TagID", "Station", "DateTimeUTC")

ne_paths <- filter(chnp, TagID %in% c(31567, 37845)) %>% # 31567 last detected at I80; 37845 last detected at cache creek
  arrange(TagID, DateTimePST)

chn %>% 
  filter(TagID == 37845) %>% 
  ggplot(aes(x = DateTimePST, y = reorder(Station, Rkm))) +
  geom_point(alpha = 0.5)

FirstLast(chn[chn$TagID == 37845, ], "DateTimePST")


bardp %>% 
  filter(TagID %in% c(31567, 37845)) %>% 
  arrange(TagID, DateTimeUTC) %>% 
  View()

#  the riovista detection happened in the middle of its path; for the other fish, made it all the way to the Feather before turning around and heading BACK up the bypass; final detection at Knaggs in December; chn_exits_final ready to model.