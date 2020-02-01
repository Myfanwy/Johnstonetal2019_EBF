#--------------------------------------------#
# M. Johnston
# Working with the output from the Stan model
# Mon Jan 27 21:39:25 2020 ------------------------------

fit = readRDS("results/fit.rds")
wst_exits <- readRDS("data_clean/model_data/wst_exits_modeldata.rds")
chn_exits <- readRDS("data_clean/model_data/chn_exits_modeldata.rds")
exits = dplyr::bind_rows(wst_exits, chn_exits)
exits$exit_status = as.factor(exits$exit_status)
exits$Bchn <- ifelse(exits$Sp == "chn", 1, 0)

samples = as.data.frame(fit)

base_probs = samples[,grep("base_prob", colnames(samples))]

colnames(base_probs) = c("did_not_exit", "exited", "confirmed_shed")
sapply(base_probs, quantile, p = c(0.025, 0.5, 0.975))

bchn_probs = samples[,grep("Bchn_prob", colnames(samples))]
colnames(bchn_probs) = c("did_not_exit", "exited", "confirmed_shed")
sapply(bchn_probs, quantile, p = c(0.025, 0.5, 0.975))

#--------------------------------------------#
betas_probs = samples[, grep("beta[*,2]", colnames(samples))] # how to extract effects on category 2?
sapply(betas_probs, quantile, p = c(0.025, 0.5, 0.975))

# compare to observed proportions
table(exits$exit_status, exits$Bchn) / nrow(exits)

# Diff between sp
quantile(base_probs$exited - bchn_probs$exited, p = c(0.025, 0.5, 0.975))

#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#
library(dplyr)
library(ggplot2)

base_probs %>% 
  tidyr::gather(key = "exit_status", value = "value") %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(group = exit_status, color = exit_status))

bchn_probs %>% 
  tidyr::gather(key = "exit_status", value = "value") %>% 
  ggplot(aes(x = value)) +
  geom_density(aes(group = exit_status, color = exit_status))

table(exits$exit_status, exits$Sp)
