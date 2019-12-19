#-------------------------------------------------------#
# model script
library(rstanarm)
library(ggplot2)
library(bayesplot)
options(digits = 2)

#--------------------------------------------#
# data

wst_exits <- readRDS("data/wst_exits_final.rds")
chn_exits <- readRDS("data/chn_exits_final.rds")

exits = rbind(wst_exits, chn_exits)

exits$ExitStatus = ifelse(exits$ExitStatus == 0, "did_not_exit", "exited")

exits$ExitStatus = ifelse(exits$TagID %in% c(13728, 13729, 20168, 
                                                 20164, 2600, 2625, 
                                                 2619, 9986, 9973), "shed_mort", exits$ExitStatus)

exits$ExitStatus = ifelse(exits$TagID %in% c(33940, 37835,37845), "shed", exits$ExitStatus)

table(exits$ExitStatus)

sum(duplicated(exits$TagID[exits$Species=="wst"])) # many white sturgeon were returners in any given year
exits$Bchn <- ifelse(exits$Species == "chn", 1, 0)
table(exits$Species)

exits$Nfish_total = ifelse(exits$Species == "chn", 215, 229) # 215 chinook obs, 229 wst (92 ind fish)

exits$ExitStatus = as.factor(exits$ExitStatus)
#--------------------------------------------#
# First Model - random effects on TagID and Detyear

if(!file.exists("stan_models/model_script_results.rda")) { 
  
    m1 = stan_polr(ExitStatus ~ Bchn + factor(Detyear), method = "logistic",
                   data = exits, adapt_delta=0.99, prior = NULL)
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

#---------------------------------------------#
# Final Model: random effects on detection year only
library(rstan)

mod = stan_model("stan_models/categorical.stan")

x = model.matrix(~ Bchn, exits)
detYear = model.matrix(~ factor(Detyear) - 1, exits)
data = list(N = nrow(exits),
            K = length(unique(exits$ExitStatus)),
            D = ncol(x),
            y = as.integer(factor(exits$ExitStatus)),
            x = x,
            M = ncol(detYear),
            detYear = detYear)

fit = sampling(mod, data)

#--------------------------------------------#
# Working with the output from the Stan model
samples = as.data.frame(fit)

base_probs = samples[,grep("base_prob", colnames(samples))]
colnames(base_probs) = levels(exits$ExitStatus)
sapply(base_probs, quantile, p = c(0.025, 0.5, 0.975))

bchn_probs = samples[,grep("Bchn_prob", colnames(samples))]
colnames(bchn_probs) = levels(exits$ExitStatus)
sapply(bchn_probs, quantile, p = c(0.025, 0.5, 0.975))

# compare to observed proportions
table(exits$ExitStatus, exits$Bchn) / nrow(exits)

# Diff between sp
quantile(base_probs$exited - bchn_probs$exited, p = c(0.025, 0.5, 0.975))

#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#


summary(m2, pars = c("alpha", "Bchn", "Sigma[Detyear:(Intercept),(Intercept)]"), 
        digits = 2, probs = c(0.0275, 0.50, 0.975))

msum <- summary(m2, probs = c(0.50, 0.0275, 0.975), digits = 2)

msum <- as.data.frame(unlist(msum))
msum[, 1:8] <- round(msum[, 1:8], 2)

readr::write_csv(msum[, c(4:7)], "results/model_estimates.csv")
prior_summary(m2)

# Inverse Logit of estimates for Table 3:
logistic(c(3.38, 2.56, 4.34)) # intercept
logistic(c(-2.51, -3.40, -1.74)) # chinook
logistic(c(0.50, 0.02, 2.07)) # sigma detyear
#--------------------------------------------#
# working with estimates
source("scripts/setup.R")

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
hist(diff.exit)

#--------------------------------------------#
#--------------------------------------------#
# work with posterior distribution

post <- as.data.frame(m2) # posterior probability; 4000 samples for each par
str(post)

names(post) <- c("alpha", "Bchn", "2011","2012", "2013", "2014", "2015", "2016", "2017", "Sigma_detyear")

# plotting marginal distributions:
post$Chn <- logistic(post$alpha + post$Bchn)
post$alpha_p <- logistic(post$alpha)
post <- post[ , c("alpha_p", "Chn")]
post2 <- tidyr::gather(post, key = "parameter", value = "value")
head(post2)

post2 %>% 
  filter(value > 0.49, value < 0.99) %>% 
ggplot() +
  geom_density(aes(x = value, fill = parameter, group = parameter), alpha = 0.7) +
  labs(x = "Marginal posterior probability of exiting Yolo Bypass", y = "Density") +
  scale_fill_manual(values = c("alpha_p" = "white", "Chn" = "grey50"), 
                    labels = c("White sturgeon     ", "fall-run Chinook salmon          ")) +
  geom_rug(aes(x = value), alpha = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 15),
        axis.text = element_text(size = 14),
    legend.position = c(0.25, 0.78),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 13),
        legend.spacing = unit(14, "points")) +
  guides(fill = guide_legend(title = "",
                             keywidth = 0.5, keyheight = 0.5, default.unit = "inch",
                             stroke = 1))

ggsave("Fig2_posteriordistributions.png", height = 6, width = 10)
