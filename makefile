#  run 'make' from the root directory's terminal to automatically produce the data intermediates and model fits from all the
# dependencies:


## Data

# 0_query-raw-data.r isn't under make because it requires the original .sqlite db, which is not in this repo

data_clean/detection_data/1_yb_detections.rds: scripts/analysis/1_munge-raw-data.R 
	Rscript $<

data_clean/detection_data/2_all_detections.rds: scripts/analysis/2_munge-BARD-data.R data_clean/detection_data/1_yb_detections.rds
	Rscript $<

data_clean/model_data/chn_exits_modeldata.rds data_clean/model_data/wst_exits_modeldata.rds: scripts/analysis/3_categorize-exit-status.R
	Rscript $<

# Model fits

results/fit_r1.rds results/interaction_fit_r1.rds results/TagID_fit_r1.rds: scripts/analysis/4_model-data.R stan_models/categorical.stan
	Rscript $<