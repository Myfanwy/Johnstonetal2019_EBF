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

alltags %>% 
  filter(Sp == "wst") %>% 
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

source("scripts/functions/FirstLast.R")

chn_rs <- fishpaths(chn, chn$TagID, chn$Station)

chn_rs <- chn_rs %>% 
  group_by(TagID) %>% 
  arrange(arrival) %>% 
  mutate(firstarr = min(arrival),
         lastdep = max(departure),
         residence = as.numeric(as.duration(interval(start = firstarr, end = lastdep)), "days"))  %>% 
  ungroup() %>% 
  arrange(TagID, arrival)

summary(chn_rs)
