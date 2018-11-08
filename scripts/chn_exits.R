#--------------------------------------------#
# functions: chn
# 
# For a fish in x year: 
#     last detection location at BC/BaseTD?
#           If yes, exited in year x
#           If no:
#              detected elsewhere (outside bypass)?
#                   If yes, exit in year x
#                   If no, no exit in year x 
#
# repeat for all years.

source("scripts/setup.R")


head(chn)

#chn <- fishpaths(chn, chn$TagID, chn$Station)

chn %>% 
  group_by(TagID) %>% 
  summarise(ndups = length(duplicated(DateTimeUTC)))

fl <- FirstLast(chn)

filter(fl, TagID == 20152)

chnidx <- select(chn, TagID, TagGroup) %>% 
  filter(!duplicated(TagID))

fl <- left_join(fl, chnidx)
vet(fl)

fl <- fl %>% 
  group_by(TagID) %>% 
  mutate(exited = case_when(LastStation %in% exitstations ~ 1 ,
                            TRUE ~ 0))

final_st_summary <- fl %>% 
  group_by(TagGroup, exited) %>% 
  tally()


