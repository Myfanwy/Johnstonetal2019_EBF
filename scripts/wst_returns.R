source("scripts/setup.R")
# white sturgeon:

# add detection year to the white sturgeon detection df
wst <- wst %>% 
  mutate(TagDetyear = case_when(lubridate::year(DateTagged) == 2012 ~ 2011,
                                 TRUE ~ 2013))
len(wst$TagID) # 92 detected white sturgeon

# wst tagids:
wsttable = alltags %>% 
  filter(Sp == "wst") %>% select(TagID) %>% # 92 tagged white sturgeon; all fish tagged were detected
  left_join(select(filter(wst, !duplicated(TagID)), TagID, TagDetyear)) 

# make exit status dataframe to be filled in
wsttags = alltags %>% filter(Sp == "wst") %>% pull(TagID)
detyear = c(2011:2017)
wst_exits = expand.grid(TagID = wsttags, Detyear = detyear, stringsAsFactors = FALSE)

# subset years that the 2-year tags can't return in
wst1206 <- wst %>% 
  filter(CodeSpace == 1206, !duplicated(TagID)) %>% pull(TagID)
wst1206_outyears = filter(wst_exits, TagID %in% wst1206, Detyear >= 2013)

# subest years that the 5-year tags from 2011 can't return in
wst1825days <- wst %>% 
  filter(TagDetyear == 2011, !(TagID %in% wst1206)) %>% 
  filter(!duplicated(TagID)) %>% 
  pull(TagID)
wst1825_outyears = filter(wst_exits, TagID %in% wst1825days, Detyear < 2014 )

# make into df to anti-join
outyears = rbind(wst1206_outyears, wst1825days)

# get rid of the years that expired tags can't return in
wst_exits = anti_join(wst_exits, outyears)
wst_exits = full_join(wst_exits, wsttable, by ="TagID") # add detyear back in

# filter to only the years greater than or equal to a fish's tag year, arrange, and add return status column
wst_exits <- wst_exits %>% 
  group_by(TagID) %>% 
  filter(Detyear >= TagDetyear) %>% 
  ungroup() %>% 
  arrange(TagID, Detyear) 

#--------------------------------------------#

# Make table of detections per fish per year
dettable <- as.data.frame.matrix(table(wst$TagID, wst$Detyear))
write.csv(dettable, file = "data/wst_returns.csv", quote = FALSE)

# Check 2011 fish's last locations:
wst_paths <- fishpaths(wst, wst$TagID, wst$Station)
wst_paths <- wst_paths %>% 
  arrange(TagID, DateTimeUTC) %>% 
  select(TagID, DateTimeUTC, Station, Detyear, DateTagged)

filter(wst_paths, TagID == 2841) # iterated through each tagID and verified that final detection was at BC_joint or Base_TD; if so, 1, if not, 0. Saved in wst_returns_handchecked.xlsx

#--------------------------------------------#
wst_returns_handchecked <- readxl::read_excel("data/wst_returns_handchecked.xlsx", na = "NA")

wst_returns_hc_tidy <- tidyr::gather(wst_returns_handchecked, key = "Detyear", value = "ExitStatus", -TagID) %>% 
  mutate(Detyear = as.integer(Detyear))

wst_exits_final <- left_join(wst_exits, wst_returns_hc_tidy) %>% 
  filter(!is.na(ExitStatus)) %>% 
  mutate(Species = "wst") %>% 
  select(TagID, Species, Detyear, TagDetyear, ExitStatus)

saveRDS(wst_exits_final, "data/wst_exits_final.rds")
