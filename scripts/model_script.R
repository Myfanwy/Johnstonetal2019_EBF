#-------------------------------------------------------#
# model script
library(rstanarm)
options(digits = 3)
wst_exits <- readRDS("data/wst_exits_final.rds")
chn_exits <- readRDS("data/chn_exits_final.rds")

exits = rbind(wst_exits, chn_exits)
exits
sum(duplicated(exits$TagID[exits$Species=="wst"])) # many white sturgeon were returners in any given year
exits$Bchn <- ifelse(exits$Species == "chn", 1, 0)
exits$Nfish_total = ifelse(exits$Species == "chn", 215, 229)


#-------------------------------------------------------#

dlist = list(Nfish_total = exits$Nfish_total, ExitStatus = exits$ExitStatus, Bchn = exits$Bchn,
             TagID = coerce_index(exits$TagID), Detyear = coerce_index(exits$Detyear))


#-------------------------------------------------------#

m2 = stan_glmer(ExitStatus ~ Bchn + (1|Detyear),
                data = exits, family = "binomial", adapt_delta=0.99,
                prior = hs())

summary(m2, pars = c("alpha", "Bchn", "Sigma[Detyear:(Intercept),(Intercept)]"), 
        digits = 2, probs = c(0.0275, 0.50, 0.975))

print(m2, digits = 3)
save(m2, file = "stan_models/model_script_results_m2.rda")
prior_summary(m2)

# convert estimates to probability scale
options(digits = 2)
logistic(c(3.35, 2.56, 4.34)) # intercept median and se, lower and upper ci
logistic(c(-2.75, -3.65, -1.96)) # b_chn median and se, lower and upper ci
logistic(c(0.27, 0.02, 1.79)) # sigma detyear median, lower and upper ci

# median probability of a chn exiting
logistic(3.35 -2.77)
# lower ci
logistic(3.35 - 3.65)
# upper ci
logistic(3.35 -1.96)

# difference in prob of exiting between species
post <- as.data.frame(m2)
p.exit.wst <- logistic(post$`(Intercept)`)
p.exit.chn <- logistic(post$`(Intercept)` + post$Bchn)
diff.exit = p.exit.wst - p.exit.chn
quantile(diff.exit)

#--------------------------------------------#
# plot
library(ggplot2)
plot(m2)
#--------------------------------------------#
post <- as.data.frame(m2)
str(post)

rethinking::dens(post$`(Intercept)`)
rethinking::dens(post$`(Intercept)` + post$Bchn, add = TRUE)
names(post) <- c("alpha", "Bchn", "2011","2012", "2013", "2014", "2015", "2016", "2017", "Sigma_detyear")
post$Chn <- logistic(post$alpha + post$Bchn)
post$alpha_p <- logistic(post$alpha)
post <- post[ , c("alpha_p", "Chn")]
post2 <- tidyr::gather(post, key = "parameter", value = "value")
head(post2)

ggplot(post2) +
  geom_density(aes(x = value, fill = parameter, group = parameter)) +
  theme_minimal()
