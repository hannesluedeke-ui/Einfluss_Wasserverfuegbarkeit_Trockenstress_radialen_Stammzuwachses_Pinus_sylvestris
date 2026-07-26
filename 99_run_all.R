# =========================================================
# 99_run_all.R
# Führt alle Hauptschritte in sinnvoller Reihenfolge aus
# =========================================================

source("01_dendro_prep.R")
source("02_meteo_prep.R")
source("03_merge_and_define_events.R")
source("04_explorative_plots.R")
source("05_linear_models.R")
source("06_logistic_models_and_lags.R")
source("07_final_figures.R")


