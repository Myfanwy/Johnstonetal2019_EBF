#-------------------------------------------------------#
# model script
library(rstanarm)
library(ggplot2)
options(digits = 2)

#--------------------------------------------#
# data

wst_exits <- readRDS("data/wst_exits_final.rds")
chn_exits <- readRDS("data/chn_exits_final.rds")

exits = rbind(wst_exits, chn_exits)
exits
sum(duplicated(exits$TagID[exits$Species=="wst"])) # many white sturgeon were returners in any given year
exits$Bchn <- ifelse(exits$Species == "chn", 1, 0)
exits$Nfish_total = ifelse(exits$Species == "chn", 215, 229) # 215 chinook obs, 229 wst (92 ind fish)

#--------------------------------------------#
# First Model - random effects on TagID and Detyear

if(!file.exists("stan_models/model_script_results.rda")) { 
  
  m1 = stan_glmer(ExitStatus ~ Bchn + (1|TagID) + (1|Detyear),
                data = exits, family = "binomial", adapt_delta=0.99,
                prior = hs())
    save(m1, file = "stan_models/model_script_results.rda")
  } else {
    load(file = "stan_models/model_script_results.rda")
  }

summary(m1, pars = c("alpha", "Bchn", "Sigma[Detyear:(Intercept),(Intercept)]", "Sigma[TagID:(Intercept),(Intercept)]"), 
        digits = 2, probs = c(0.0275, 0.50, 0.975)) # variance on TagID includes 0

#---------------------------------------------#
# Final Model: random effects on detection year only

if(!file.exists("stan_models/model_script_results_m2.rda")) {

    m2 = stan_glmer(ExitStatus ~ Bchn + (1|Detyear),
                data = exits, family = "binomial", adapt_delta=0.99,
                prior = hs())
    save(m2, file = "stan_models/model_script_results_m2.rda")
    
} else {
  load("stan_models/model_script_results_m2.rda")
}

summary(m2, pars = c("alpha", "Bchn", "Sigma[Detyear:(Intercept),(Intercept)]"), 
        digits = 2, probs = c(0.0275, 0.50, 0.975))
prior_summary(m2)

#--------------------------------------------#
# working with estimates

# convert estimates to probability scale
logistic(c(3.38, 2.56, 4.34)) # intercept median and se, lower and upper ci
logistic(c(-2.51, -3.40, -1.74)) # b_chn median and se, lower and upper ci

# median probability of a chn exiting
logistic(3.38 -2.51)
# lower ci
logistic(3.38 - 3.40)
# upper ci
logistic(3.35 -1.74)

# difference in prob of exiting between species
post <- as.data.frame(m2)
p.exit.wst <- logistic(post$`(Intercept)`)
p.exit.chn <- logistic(post$`(Intercept)` + post$Bchn)
diff.exit = p.exit.wst - p.exit.chn
quantile(diff.exit)

#--------------------------------------------#
#--------------------------------------------#
# work with posterior distribution

post <- as.data.frame(m2) # posterior probability; 4000 samples for each par
str(post)

names(post) <- c("alpha", "Bchn", "2011","2012", "2013", "2014", "2015", "2016", "2017", "Sigma_detyear")
post$Chn <- logistic(post$alpha + post$Bchn)
post$alpha_p <- logistic(post$alpha)
post <- post[ , c("alpha_p", "Chn")]
post2 <- tidyr::gather(post, key = "parameter", value = "value")
head(post2)

ggplot(post2) +
  geom_density(aes(x = value, fill = parameter, group = parameter)) +
  theme_minimal()
