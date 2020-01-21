#-------------------------------------------------------#
# Step 2 of analysis
# M. Johnston
# Fri Dec 27 13:08:02 2019 ------------------------------

# goal: categorize all fish, each year, into one of three "exit" statuses:

# 3) Tag shed in Bypass
#    - occurs when a tagged fish is recovered by hand somewhere after being released, and their detection history shows their final recorded detection somewhere in the Yolo Bypass array.  This applied to three fish in the study, all recovered at Wallace Weir fyke trap and identified either by their floy tags or their fork lengths + detection histories: 

known_sheds = c(37835 , # this fish was recovered in fyke and identified by its floy tag in 2015
                20161 , # captured in WW fyke and identified by fork length/detection history consistent with capture time
                20164  # captured in WW fyke and identified by fork length/detection history consistent with capture time
                )

# 2) Exited
# - occurs when a fish's last detection location is at BCN, BCS, the base of the Toe drain, OR in the BARD array, AND that fish is not detected again within the Yolo Bypass array in the same detection year (i.e., white sturgeon have a clear final exit in a given detection year).  

# 1) Shed/Mortality/Did not Exit the Yolo Bypass
# - all other fish in the study fall into this category.

# BEGIN
#--------------------------------------------#
source("scripts/setup.R") # loads detections cleaned in scripts/analysis/1_munge-raw-data.R, subsets down to wst/chn dets

# White Sturgeon
#--------------------------------------------#
# Split into detection years, get last location in each detection year.
#--------------------------------------------#
wst_dets %>% 
  group_by(TagID, Detyear) %>% 
  filter(!duplicated(GroupedStn)) %>% 
  ungroup() %>% 
  select(TagID, Detyear, GroupedStn) -> yb_peryear

head(yb_peryear)

syd = split(yb_peryear, list(yb_peryear$TagID, yb_peryear$Detyear))
syd <- syd[sapply(syd, nrow) > 0]

# apply to a single fish
yb_enter_onefish <- function(df) {
  
  df = as.data.frame(df)
  
  dfnew = data.frame(TagID = as.numeric(unique(df$TagID)),
                   Detyear = as.numeric(unique(df$Detyear)),
                   ybstatus = ifelse(sum(df$GroupedStn %in% ybrecs) > 0, 1, 0) # if they entered YB that year, 1, else 0
                   )
return(dfnew)
}

ybchk = lapply(syd, yb_enter_onefish)
ybchk = do.call(rbind, ybchk)

tail(arrange(ybchk, TagID), 25) # if they weren't detected at all that year, they wouldn't be in the detections. Of those detected, should only analyze exits for fish that have a 1 in that year.

head(wst_dets) # have to remove the detections for the fish where ybstatus = 0



wp = split(wst_dets, wst_dets$Detyear) 


# apply first_last to each detection year
wfl = list()
for(i in 1:8) {
  wfl[i] = lapply(wp[i], first_last, "TagID", "DateTimePST", "GroupedStn")
}
names(wfl) = names(wp)
wfl = data.frame(rbindlist(wfl, use.names = TRUE, idcol = "Detyear"), stringsAsFactors = FALSE)

wpfl = wfl %>% 
  group_by(Detyear, TagID) %>% 
  mutate(exit_status = case_when(last_stn %in% ybrecs ~ 1, # 1 = did not exit/mortality/shed tag
                                 TRUE ~ 2)) %>% # 2 = exited
  mutate(exit_status = ifelse(TagID %in% known_sheds, 3, exit_status)) %>% # 3 = shed tag
  ungroup()

table(wpfl$exit_status) # how many of these fish never entered the yolo bypass in a given year tho?

ybrecs

ex = readRDS("data_clean/wst_exits_final.rds")
head(ex)
str(ex)
table(ex$ExitStatus)

ex[ex$ExitStatus == 0, ] # did not exit
ex[ex$ExitStatus == 1, ] # exited

wpfl[wpfl$exit_status == 1, ] # 56483 and 56494 were
wpfl[wpfl$TagID == 56483, ]
saveRDS(wst_exits_final, "data/wst_exits_final.rds")


# CHINOOK
chn_fl = first_last(chn_dets, "TagID", "DateTimePST", "GroupedStn")


