#-------------------------------------------------------#
# model script
library(rstan)
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

set.seed(1234)
fit = sampling(mod, data)
saveRDS(fit, "results/fit.rds")

#-------------------------------------------------------#