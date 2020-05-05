#-------------------------------------------------------#
# model script
library(rstan)
options(digits = 3)

#--------------------------------------------#
# data
wst_exits <- readRDS("data_clean/model_data/wst_exits_modeldata.rds") # generated in steps 0-3
chn_exits <- readRDS("data_clean/model_data/chn_exits_modeldata.rds") # generaated in steps 0-3

exits = dplyr::bind_rows(wst_exits, chn_exits)
exits$Bchn <- ifelse(exits$Sp == "chn", 1, 0) #Bchn predictor

exits$Nfish_total = ifelse(exits$Sp == "chn", 215, 182) 
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

fit = sampling(mod, data, seed = 1234)

if(!dir.exists("results"))
    dir.create("results")

saveRDS(fit, "results/fit_r1.rds")

#-------------------------------------------------------#
# Candidate Models
#---------------------------------------------#
# check year:sp interaction: random effects on detection year:sp; this model was only written/fit to verify that year effects did not contribute consistent explanatory power to the variance in proportion of Chinook salmon exiting from year to year
mod = stan_model("stan_models/categorical.stan", save_dso = FALSE)

x = model.matrix(~ Bchn, exits)
detYear = model.matrix(~ factor(Detyear):Bchn - 1, exits)
data = list(N = nrow(exits),
            K = length(unique(exits$exit_status)),
            D = ncol(x),
            y = as.integer(factor(exits$exit_status)),
            x = x,
            M = ncol(detYear),
            detYear = detYear)

fit2 = sampling(mod, data, seed = 1234)
saveRDS(fit2, "results/interaction_fit_r1.rds")

#-------------------------------------------------------#
# Random effects on TagID
#--------------------------------------------#
mod = stan_model("stan_models/categorical.stan")

x = model.matrix(~ Bchn, exits)
detYear = model.matrix(~ factor(Detyear) + factor(TagID) - 1, exits)

data = list(N = nrow(exits),
            K = length(unique(exits$exit_status)),
            D = ncol(x),
            y = as.integer(factor(exits$exit_status)),
            x = x,
            M = ncol(detYear),
            detYear = detYear)


fit2 = sampling(mod, data, seed = 1234)
saveRDS(fit, "results/TagID_fit_r1.rds")
