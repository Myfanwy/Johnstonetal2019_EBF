#-------------------------------------------------------#
# M. Johnston
# Functions for categorizing exit status 
# Mon Dec 30 09:49:44 2019 ------------------------------

# wst tagids:
mk_wst_tbl = function() {
  wsttable = alltags %>% 
  filter(Sp == "wst") %>% 
  mutate(DetyearTagged = year(DateTagged) - 1,
         YearTagged = year(DateTagged)) %>% 
  select(TagID, YearTagged, DetyearTagged) %>% 
  left_join(select(filter(wst_dets, !duplicated(TagID)), TagID)) 
  return(wsttable)
  }

# make exit status dataframe to be filled in
mk_exit_tbl = function() {
  wsttags = alltags %>% filter(Sp == "wst") %>% pull(TagID)
  detyear = c(2011:2017)
  wst_exits = expand.grid(TagID = wsttags, Detyear = detyear, stringsAsFactors = FALSE)
  return(wst_exits)
}


# calculate end tag date for each wst tag:

add_tag_end_col = function(wst_tags_df) {
  
  wst_tags_dff = wst_tags_df %>% 
    mutate(EstTagEnd = DateTagged + duration(EstTagLife_days, "days"))
  
  return(wst_tags_dff)
  
}

# visualize tag life:
wst_taglife_viz <- function(tagdf) {
  
pp = ggplot(tagdf, aes(x = DateTagged, y = factor(TagID))) +
 geom_point(aes(color = factor(CodeSpace)), show.legend = FALSE) +
 geom_segment(aes(x = DateTagged, xend = EstTagEnd, 
                  y = factor(TagID), 
                  yend = factor(TagID)),
              size = 0.2) +
  geom_point(aes(x = EstTagEnd, color = factor(CodeSpace)), show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Tag life from Tagging Date to estimated tag life end",
       y = "TagID",
       subtitle = "colors indicate different codespaces")

pp
}

rm_post_tag_end_dets <- function(dets_df){
  dets_df %>% 
    group_by(TagID) %>% 
    arrange(DateTimePST) %>% 
    filter(DateTimePST <= EstTagEnd) %>% 
    ungroup() -> dets_dff
  
  return(dets_dff)
  
}
