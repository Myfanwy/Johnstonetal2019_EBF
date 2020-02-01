#--------------------------------------------#
# M. Johnston
# Reviewer questions
# Fri Jan 31 11:10:42 2020 ------------------------------

library(dplyr)
library(ggplot2)
# DATA and functions
dat = readRDS("data_clean/detection_data/1_all_detections.rds")
ctags = readRDS("data_raw/tag_data_raw/chn_tags_raw.rds")
cexits = readRDS("data_clean/model_data/chn_exits_modeldata.rds")
source("scripts/functions/munging_fxns.R")
source("scripts/functions/convenience_fxns.R")

#--------------------------------------------#
# 1. how many of the chn in Detyear 2016 were just downstream of the Fremont Weir prior to the high flow events?
#--------------------------------------------#

# stations just downstream of Fremont Weir:
dsfw = c("PCSG", "YBKR", "YBBWW", "YBAWW", "KLRCN", "KLRC", "YBTP", 
"YBDP", "CBD")

dat %>% 
  filter(TagID %in% ctags$TagID) -> cdets

cdets <- get_det_year(cdets, "DateTimePST")

cdets %>% 
  filter(Detyear == 2016) %>% 
  filter(GroupedStn %in% dsfw) %>% 
  filter(!duplicated(TagID)) %>% 
  pull(TagID) -> d16

plot_track(cdets, d16[10])

cdets %>% 
  filter(GroupedStn %in% c("YBBCD", "PCSG")) %>% 
  group_by(Detyear) %>% 
  summarise(nperyr = len(TagID))


table(cexits$exit_status, cexits$Detyear)

fr = readRDS("data_clean/cdec_data/fremont_riverstage2012-2018.rds")
fr %>% 
  mutate(overtopping = ifelse(value >=32.0, "yes", "no")) %>% 
  select(datetime = actual_datetime, value = value, year, month, overtopping) -> fr

fr$ot_id = rleidv(fr, cols = c("overtopping"))
View(fr)
fr <- fr[fr$overtopping == "yes", ]

plot(fr$datetime, fr$value)
plot(fr$datetime, fr$ot_id)

fr %>% 
  group_by(ot_id) %>% 
  summarise(start = min(datetime),
            end = max(datetime)) -> overtops

overtops = get_det_year(overtops, "start")

overtops %>% 
  group_by(Detyear) %>% 
  tally()

overtops$duration = as.numeric(as.duration(interval(start = overtops$start, end = overtops$end)), "hours")

long_ots = filter(overtops, duration >= 1.0) # consider only overtopping events that were at least an hour in duration
long_ots$ot_days = as.numeric(as.duration(interval(start = long_ots$start, end = long_ots$end)), "days")

ggplot(long_ots, aes(x = factor(Detyear), y = ot_days)) +
  geom_point(aes(color = factor(Detyear)))

# If all the Chinook salmon that did not exit downstream had in fact past upstream of the Fremont Weir, and then had not been detected at upstream receivers, how much would this have changed the probability of exit for the species?

filter(cexits, Detyear == 2014 | Detyear == 2016) %>% 
  filter(exit_status == 1) %>% 
  mutate(month_det = month(last_det)) %>% 
  filter(month_det == 12)
