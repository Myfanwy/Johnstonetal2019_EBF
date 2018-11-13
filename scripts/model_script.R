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
dlist = list(Nfish_total = exits$Nfish_total, ExitStatus = exits$ExitStatus, Bchn = exits$Bchn,
             TagID = coerce_index(exits$TagID), Detyear = coerce_index(exits$Detyear))

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

# Random effects model: TagID and Year

m2 <- map2stan(
  alist(
    #likelihood
    ExitStatus ~ dbinom( 1, p ),
    # linear model
    logit(p) <- a + bChn*Bchn + b_fish[TagID] + b_detyear[Detyear],
    # adaptive priors
    b_fish[TagID] ~ dnorm(0,sigma_fish),
    b_detyear[Detyear] ~ dnorm(0, sigma_detyear),
    # fixed priors
    a ~ dnorm(0, 1),
    bChn ~ dnorm(0, 10),
    sigma_fish ~ dnorm(5, 2),
    sigma_detyear ~ dexp(1)
  ),
  data = dlist, warmup=2000 , iter=1e5 , cores=2 , chains = 1)

precis(m2, prob = 0.95)
plot(precis(m2, pars = c("bChn", "b_detyear", "sigma_fish", "sigma_detyear"), depth = 2))
logistic(-2.61)
