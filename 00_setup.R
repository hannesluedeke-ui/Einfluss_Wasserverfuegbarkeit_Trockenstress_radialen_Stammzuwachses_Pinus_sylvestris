# =========================================================
# 00_setup.R
# Pakete laden und Projektpfade prüfen
# =========================================================

required_packages <- c("readr", "dplyr", "lubridate", "ggplot2", "tidyr")

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

dir.create("./data_out", showWarnings = FALSE)

 if(!file.exists("./Daten/Pfynwald_dendro.tab")) {
  stop("Datei nicht gefunden: ./Daten/Pfynwald_dendro.tab")
}

if (!file.exists("./Daten/Pfynwald_meteo.tab")) {
  stop("Datei nicht gefunden: ./Daten/Pfynwald_meteo.tab")
}



