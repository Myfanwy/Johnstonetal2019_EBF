source("scripts/setup.R")

# white sturgeon:
wst <- wst %>% 
  mutate(tag_detyear = case_when(lubridate::year(DateTagged) == 2012 ~ 2011,
                                 TRUE ~ 2013))
len(wst$TagID) # 92 detected white sturgeon

# master list of wst tagids:
wsttable = alltags %>% 
  filter(Sp == "wst") %>% select(TagID) %>% #92 tagged white sturgeon; all fish tagged were detected
  left_join(select(filter(wst, !duplicated(TagID)), TagID, tag_detyear)) 

# make exit status dataframe to be filled in
wsttags = alltags %>% filter(Sp == "wst") %>% pull(TagID)
detyear = c(2011:2017)
wst_exits = expand.grid(TagID = wsttags, Detyear = detyear, stringsAsFactors = FALSE)

wst1206 <- wst %>% 
  filter(CodeSpace == 1206, !duplicated(TagID)) %>% pull(TagID)

wst1825days <- wst %>% 
  filter(tag_detyear == 2011, !(TagID %in% wst1206)) %>% 
  filter(!duplicated(TagID)) %>% 
  pull(TagID)

wst1206_outyears = filter(wst_exits, TagID %in% wst1206, Detyear >= 2013)
wst1825_outyears = filter(wst_exits, TagID %in% wst1825days, Detyear < 2014 )
outyears = rbind(wst1206_outyears, wst1825days)

wst_exits = anti_join(wst_exits, outyears)
wst_exits = full_join(wst_exits, wsttable, by ="TagID") # add detyear back in


#--------------------------------------------#
# functions: wst
# 
# For a fish with tag_detyear x: 
#   Is that fish detected again in >= x + 1 ?
#       If yes, change exit status in tagyear x and all years in which it was detected except for the last year to 1
#       If no:
#         last detection location in tagyear x at BC/BaseTD?
#             If yes, exited in year x
#             If no, no exit in year x or any years > x
# repeat for x + 1 year.


test <- wst %>% 
  filter(TagID == 25618)

# start in detyear fish was tagged in
unique(test$tag_detyear) # 2013

test_tag_detyear = unique(test$tag_detyear) # beginning year
test_years_detected = unique(test$Detyear) # vector of years detected
test_years_detected = test_years_detected[!(test_years_detected %in% test_tag_detyear)] # filter out year they were tagged

tag_exit_status <- data.frame(TagID = 25618, tag_detyear = test_tag_detyear, years_detected = test_years_detected,
                              )
  


returns <- wst %>% 
 # filter(CodeSpace != 1206) %>% 
  group_by(TagID) %>% 
  mutate(latest_detyear = max(Detyear)) %>% 
  arrange(DateTimePST) %>% 
  mutate(returned = ifelse(Detyear < max(Detyear) & Detyear >= tag_detyear, 1, 0)) %>% # all fish's last detyears should be 0
  ungroup() %>% 
  group_by(TagID, Detyear) %>% 
  summarise(returnstatus = unique(returned)) %>% 
  filter(returnstatus > 0)

wsttable <- full_join(returns, wsttable) %>% rename(exitstatus = returnstatus)
wsttable
