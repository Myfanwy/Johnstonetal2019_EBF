#-------------------------------------------------------#
# model script
library(rstan)
library(ggplot2)
options(digits = 3)

#--------------------------------------------#
# data

wst_exits <- readRDS("data_clean/model_data/wst_exits_modeldata.rds")
chn_exits <- readRDS("data_clean/model_data/chn_exits_modeldata.rds")

exits = dplyr::bind_rows(wst_exits, chn_exits)

table(exits$exit_status)

sum(duplicated(exits$TagID[exits$Sp=="wst"])) # many white sturgeon were returners in any given year
exits$Bchn <- ifelse(exits$Sp == "chn", 1, 0)
table(exits$Sp)
table(exits$Bchn)

exits$Nfish_total = ifelse(exits$Sp == "chn", 215, 182) # 215 chinook obs, 229 wst (92 ind fish)

exits$exit_status = as.factor(exits$exit_status)


#---------------------------------------------#
# Final Model: random effects on detection year only

mod = stan_model("stan_models/categorical.stan")

x = model.matrix(~ Bchn, exits)
detYear = model.matrix(~ factor(Detyear) - 1, exits)
data = list(N = nrow(exits),
            K = length(unique(exits$exit_status)),
            D = ncol(x),
            y = as.integer(factor(exits$exit_status)),
            x = x,
            M = ncol(detYear),
            detYear = detYear)

fit = sampling(mod, data)
saveRDS(fit, "results/fit.rds")
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
