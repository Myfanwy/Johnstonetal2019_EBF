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
colnames(base_probs) = levels(exits$exit_status)
sapply(base_probs, quantile, p = c(0.025, 0.5, 0.975))

bchn_probs = samples[,grep("Bchn_prob", colnames(samples))]
colnames(bchn_probs) = levels(exits$exit_status)
sapply(bchn_probs, quantile, p = c(0.025, 0.5, 0.975))

# compare to observed proportions
table(exits$exit_status, exits$Bchn) / nrow(exits)

# Diff between sp
quantile(base_probs$`2` - bchn_probs$`2`, p = c(0.025, 0.5, 0.975))

#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#

