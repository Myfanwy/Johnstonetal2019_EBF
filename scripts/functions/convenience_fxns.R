#############
# Convenience Functions #--------------------------------#
#############
library(ggplot2)
#--------------------------------------------#
# for converting parm estimates from log-odds to probability:
logistic <- function (x) 
{
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
}


## ht == headtail
ht <- function(d, n=6) {
  
  rbind(head(d, n), tail(d, n))
  
}

# typing shortcuts
len <- function(x){length(unique(x))}
csn <- function(x){colSums(is.na(x))}
rsn <- function(x){rowSums(is.na(x))}


# plot a fish track:
plot_track = function(dets_df, tagid) {
  d = subset(dets_df, TagID == tagid)
  ggplot(d, aes(x = DateTimePST, y = reorder(GroupedStn, rkms))) +
    geom_jitter(alpha = 0.25, width = 0.2) +
    theme_bw()
}
