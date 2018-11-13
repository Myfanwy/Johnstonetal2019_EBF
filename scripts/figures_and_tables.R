#--------------------------------------------#
# Summary stats for results section of manuscript
#--------------------------------------------#

source("scripts/setup.R")
options(digits = 4)


fca_tags %>% 
  left_join(., select(alltags, TagID, TagGroup), by = "TagID") %>% 
  filter(TagGroup != "fca_2012") %>% 
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
  summarise(nfish = len(TagID),
            fl_low = min(FL),
            fl_hi = max(FL),
            fl_mean = mean(FL, na.rm = TRUE),
            sd_mean = sd(FL, na.rm = TRUE),
            nmales = sum(Sex == "M"),
            nfemales = sum(Sex == "F"),
            nunk = sum(Sex == "U")
            ) 

# number of returning fish per year

wst_exits <- readRDS("data/wst_exits_final.rds")
wst_exits %>% 
  group_by(Detyear) %>% 
  summarise(nreturn = len(TagID),
            nexits = sum(ExitStatus))
