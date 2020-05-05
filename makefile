#  run 'make' from the root directory's terminal to automatically produce the data intermediates and model fits from all the
# dependencies:

RAW_DATA_SHARED = data_clean/deployment_data/stns.rds data_raw/tag_data_raw/alltags_raw.rds data_raw/tag_data_raw/chn_tags_raw.rds

RAW_DATA_DETS = data_raw/deployment_data_raw/deps_data_raw.rds data_raw/detection_data_raw/dets_data_raw.rds data_clean/detection_data/FDA_screen2012-2018.rds 

RAW_DATA_BARD = data_clean/bard_data/bard_dets.rds data_clean/detection_data/1_yb_detections.rds

FXNS = scripts/functions/munging_fxns.R scripts/functions/convenience_fxns.R scripts/functions/munging_bard_fxns.R

MODEL_DATA = data_clean/model_data/chn_exits_modeldata.rds data_clean/model_data/wst_exits_modeldata.rds

RESULTS = results/fit_r1.rds results/interaction_fit_r1.rds results/TagID_fit_r1.rds

## Data
# 0_query-raw-data.r isn't under make because it requires the original .sqlite db, which is not in this repo

data_clean/detection_data/1_yb_detections.rds: scripts/analysis/1_munge-raw-data.R \
                                               $(RAW_DATA_SHARED) $(RAW_DATA_DETS) $(FXNS)
	Rscript $<

data_clean/detection_data/2_all_detections.rds: scripts/analysis/2_munge-BARD-data.R $(RAW_DATA_SHARED) $(RAW_DATA_BARD) $(FXNS)
	Rscript $<

$(MODEL_DATA) &: scripts/analysis/3_categorize-exit-status.R scripts/analysis/setup.R data_clean/detection_data/2_all_detections.rds data_raw/tag_data_raw/alltags_raw.rds
	Rscript $<

# Model fits

$(RESULTS) &: scripts/analysis/4_model-data.R stan_models/categorical.stan $(MODEL_DATA)
	Rscript $<

make clean:
	rm $(MODEL_DATA) $(RESULTS)


make results: $(RESULTS)
