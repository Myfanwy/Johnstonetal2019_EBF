#--------------------------------------------#
# Summary stats for results section of manuscript
#--------------------------------------------#

source("scripts/setup.R")
options(digits = 4)


chn_tags %>% 
  filter(!(TagID %in% c(31570, 13720 ,13723))) %>% # late-fall fish; exclude
  left_join(., select(alltags, TagID, TagGroup), by = "TagID") %>% 
  filter(TagGroup != "fca_2012", TagGroup != "fca_2018") %>% 
  group_by(TagGroup) %>% 
  summarise(nfish = len(TagID),
            fl_low = min(FL),
            fl_hi = max(FL),
            fl_mean = mean(FL, na.rm = TRUE),
            sd_mean = sd(FL, na.rm = TRUE),
            nmales = sum(Sex == "M"),
            nfemales = sum(Sex == "F"),
            nunk = sum(Sex == "U")
            ) 

wst_tags %>% 
  group_by(TagGroup) %>% 
    left_join(., select(alltags, TagID, TagGroup, FL, Sex), by = "TagID") %>% 
  summarise(nfish = len(TagID),
            fl_low = min(FL),
            fl_hi = max(FL),
            fl_mean = mean(FL, na.rm = TRUE),
            sd_mean = sd(FL, na.rm = TRUE),
            nmales = sum(Sex == "M"),
            nfemales = sum(Sex == "F"),
            nunk = sum(Sex == "U")
            ) 


source("scripts/functions/munging_fxns.R")
library(tagtales)
chn_rs <- tag_tales(chn_dets, chn_dets$TagID, chn_dets$GroupedStn, "DateTimePST")

chn_rs <- chn_rs %>% 
  group_by(TagID) %>% 
  arrange(arrival) %>% 
  mutate(firstarr = min(arrival),
         lastdep = max(departure),
         residence = as.numeric(as.duration(interval(start = firstarr, end = lastdep)), "days"))  %>% 
  ungroup() %>% 
  arrange(TagID, arrival)

summary(chn_rs$residence) # median residence = ~25 days

# number of returning fish per year

wst_exits <- readRDS("data_clean/model_data/wst_exits_modeldata.rds")
table(wst_exits$Detyear)
table(wst_exits$Detyear, wst_exits$exit_status)

chn_exits <- readRDS("data_clean/model_data/chn_exits_modeldata.rds")
table(chn_exits$Detyear)
table(chn_exits$Detyear, chn_exits$exit_status)

# number of fish detected at Putah Creek Spawning
dets9 <- get_det_year(dets9, "DateTimePST")
dets9 %>% 
  filter(GroupedStn == "PCSG") %>% 
  group_by(Detyear) %>% 
  summarise(nfish = len(TagID),
            mindate = min(DateTimePST),
            maxdate = max(DateTimePST))

# number of chn exits by year
chn_exits_final %>% 
  group_by(Detyear, ExitStatus) %>% 
  tally() %>% 
  filter(ExitStatus == 1)

# residence/detection window of white sturgeon

wst %>% 
  group_by(Detyear) %>% 
  summarise(firstdet = min(DateTimePST), loc = Station[DateTimePST == min(DateTimePST)],
            lastdet = max(DateTimePST), loc2 = Station[DateTimePST == max(DateTimePST)])

