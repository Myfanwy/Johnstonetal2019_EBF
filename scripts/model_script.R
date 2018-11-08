#-------------------------------------------------------#
# model script

library(rethinking)

wst_exits <- readRDS("data/wst_exits_final.rds")
chn_exits <- readRDS("data/chn_exits_final.rds")

exits = rbind(wst_exits, chn_exits)
exits
sum(duplicated(exits$TagID[exits$Species=="wst"])) # many white sturgeon were returners in any given year
exits$Bchn <- ifelse(exits$Species == "chn", 1, 0)
exits$Nfish_total = ifelse(exits$Species == "chn", 215, 229)


#-------------------------------------------------------#
# Basic binomial model: treats each fish as iid
dlist = list(Nfish_total = exits$Nfish_total, ExitStatus = exits$ExitStatus, Bchn = exits$Bchn)

m1 <- rethinking::map(
  alist(
    ExitStatus ~ dbinom(1, p),
    logit(p) <- a + bChn*Bchn,
    a ~ dnorm(0,10),
    bChn ~ dnorm(0, 10)
  ),
  data = dlist)

precis(m1, prob = 0.95)
plot(precis(m1))

post <- extract.samples(m1)
p.exit.salmon <- logistic(post$a + post$bChn)
p.exit.wst <- logistic(post$a)

plot(NULL, xlim = c(0, 1), ylim = c(0, 35))
dens(p.exit.salmon, col = "red", add = TRUE)
dens(p.exit.wst, col = "darkblue", add = TRUE)
abline(v=0, lty = 2, add = TRUE)

diff.exit <- p.exit.wst - p.exit.salmon

quantile(diff.exit, c(0.025, 0.5, 0.975))  # means that the median estimate of exit for salmon is about 32% that of a sturgeon, with a 95% CI of between 25 % and 39%.
dens(diff.esc)

#-------------------------------------------------------#
# plotting raw data

library(ggplot2)
library(dplyr)

exits %>% 
  group_by(Species) %>% 
  summarise(propexits = sum(ExitStatus)/unique(Nfish_total)) %>% 
  ggplot() +
  geom_bar(aes(x = Species, y = propexits, fill = Species), stat = "identity") +
  coord_flip() 
