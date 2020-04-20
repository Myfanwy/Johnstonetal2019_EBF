#--------------------------------------------#
# M. Johnston
# Working with the output from the Stan model
# Mon Jan 27 21:39:25 2020 ------------------------------

#
#--------------------------------------------#
## Compare models
library(loo)
mods = sapply(list.files("results/orig_results", pattern = "rds", full.names = TRUE),
              readRDS,
              simplify = FALSE)
loos = lapply(mods, function(x) loo(extract_log_lik(x)))
loo_compare(loos[[1]], loos[[2]], loos[[3]])

#--------------------------------------------#

fit = readRDS("results/orig_results/fit.rds")
wst_exits <- readRDS("data_clean/model_data/wst_exits_modeldata.rds")
chn_exits <- readRDS("data_clean/model_data/chn_exits_modeldata.rds")
exits = dplyr::bind_rows(wst_exits, chn_exits)
exits$exit_status = as.factor(exits$exit_status)
exits$Bchn <- ifelse(exits$Sp == "chn", 1, 0)

samples = as.data.frame(fit)

# White Sturgeon
base_probs = samples[,grep("base_prob", colnames(samples))]
colnames(base_probs) = c("did_not_exit", "exited", "confirmed_shed")
sapply(base_probs, quantile, p = c(0.025, 0.5, 0.975))

# Chinook Salmon
bchn_probs = samples[,grep("Bchn_prob", colnames(samples))]
colnames(bchn_probs) = c("did_not_exit", "exited", "confirmed_shed")
sapply(bchn_probs, quantile, p = c(0.025, 0.5, 0.975))

# Diff between Spp.
quantile(base_probs$exited - bchn_probs$exited, p = c(0.025, 0.5, 0.975))

# by-detyear proportion of Chn exits:
table(exits$exit_status[exits$Bchn == 1], exits$Detyear[exits$Bchn == 1])

#--------------------------------------------#
#--------------------------------------------#
#--------------------------------------------#
# Visualize results
library(ggplot2)

# plotting marginal distributions: (FIGURE 2)
post = data.frame(cbind(wst = base_probs$exited, chn = bchn_probs$exited))
post2 <- tidyr::gather(post, key = "parameter", value = "value")

ggplot(post2) +
  geom_density(aes(x = value, 
                   fill = parameter, 
                   group = parameter), 
               alpha = 0.7) +
  labs(x = "Marginal posterior probability of exiting Yolo Bypass", 
       y = "Density") +
  scale_fill_manual(
    values = c("wst" = "white", "chn" = "grey50"),
    labels = c("fall-run Chinook Salmon        ", "White Sturgeon     ")
  ) +
  geom_rug(aes(x = value), alpha = 0.5) +
  fishpals::theme_pub() +
  theme(legend.key = element_rect(size = 20, fill = "white"),
        legend.position = c(0.35, 0.85),
        legend.direction = "horizontal",
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.spacing.y = unit(1, "cm"),
        legend.text = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13)) +
  guides(fill = guide_legend(title = NULL
  )) +
  scale_x_continuous(expand = c(0.02, 0.0))

#ggsave("Fig2_posteriordistributions.png", height = 6, width = 10)

