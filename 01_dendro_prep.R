# =========================================================
# 01_dendro_prep.R
# Dendrometerdaten einlesen, Referenzbäume auswählen,
# Tageswerte für GI und SRC berechnen
# =========================================================

source("./Skripte/00_setup.R")

# ---------- Dendrometerdaten einlesen ----------
dendro <- read_tsv(
  "./Daten/Pfynwald_dendro.tab",
  skip = 43,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

dendro_control <- dendro %>%
  select(
    `Date/Time`,
    contains("control"),
    contains("control stopplot")
  )

# ---------- GI auswählen ----------
dendro_gi <- dendro_control[, c(
  "Date/Time",
  "Tree GI [µm] (at tree 110, control plot)",
  "Tree GI [µm] (at tree 124, control plot)",
  "Tree GI [µm] (at tree 125, control plot)",
  "Tree GI [µm] (at tree 109, control stopplot)"
)]

colnames(dendro_gi) <- c("datetime", "GI_110", "GI_124", "GI_125", "GI_109")

dendro_gi <- dendro_gi[-c(1:893), ]
rownames(dendro_gi) <- NULL

dendro_gi$datetime <- as.POSIXct(dendro_gi$datetime, tz = "UTC")
dendro_gi$GI_110 <- as.numeric(dendro_gi$GI_110)
dendro_gi$GI_124 <- as.numeric(dendro_gi$GI_124)
dendro_gi$GI_125 <- as.numeric(dendro_gi$GI_125)
dendro_gi$GI_109 <- as.numeric(dendro_gi$GI_109)

dendro_gi$date <- as.Date(dendro_gi$datetime)

# ---------- SRC auswählen ----------
dendro_src <- dendro_control[, c(
  "Date/Time",
  "Tree SRC [µm] (at tree 110, control plot)",
  "Tree SRC [µm] (at tree 124, control plot)",
  "Tree SRC [µm] (at tree 125, control plot)",
  "Tree SRC [µm] (at tree 109, control plot)"
)]

colnames(dendro_src) <- c("datetime", "SRC_110", "SRC_124", "SRC_125", "SRC_109")

dendro_src <- dendro_src[-c(1:893), ]
rownames(dendro_src) <- NULL

dendro_src$datetime <- as.POSIXct(dendro_src$datetime, tz = "UTC")
dendro_src$SRC_110 <- as.numeric(dendro_src$SRC_110)
dendro_src$SRC_124 <- as.numeric(dendro_src$SRC_124)
dendro_src$SRC_125 <- as.numeric(dendro_src$SRC_125)
dendro_src$SRC_109 <- as.numeric(dendro_src$SRC_109)

dendro_src$date <- as.Date(dendro_src$datetime)

# ---------- Tageswerte GI ----------
dendro_daily_trees <- dendro_gi %>%
  group_by(date) %>%
  summarise(
    GI_110 = mean(GI_110, na.rm = TRUE),
    GI_124 = mean(GI_124, na.rm = TRUE),
    GI_125 = mean(GI_125, na.rm = TRUE),
    GI_109 = mean(GI_109, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

dendro_daily_trees$inc_110 <- c(NA, diff(dendro_daily_trees$GI_110))
dendro_daily_trees$inc_124 <- c(NA, diff(dendro_daily_trees$GI_124))
dendro_daily_trees$inc_125 <- c(NA, diff(dendro_daily_trees$GI_125))
dendro_daily_trees$inc_109 <- c(NA, diff(dendro_daily_trees$GI_109))

dendro_daily_trees$GI_daily_inc_mean <- rowMeans(
  dendro_daily_trees[, c("inc_110", "inc_124", "inc_125", "inc_109")],
  na.rm = TRUE
)

dendro_daily_trees$GI_mean <- rowMeans(
  dendro_daily_trees[, c("GI_110", "GI_124", "GI_125", "GI_109")],
  na.rm = TRUE
)


# ---------- Tageswerte SRC ----------
dendro_src_daily_trees <- dendro_src %>%
  group_by(date) %>%
  summarise(
    SRC_110 = mean(SRC_110, na.rm = TRUE),
    SRC_124 = mean(SRC_124, na.rm = TRUE),
    SRC_125 = mean(SRC_125, na.rm = TRUE),
    SRC_109 = mean(SRC_109, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    SRC_mean = rowMeans(
      across(c(SRC_110, SRC_124, SRC_125, SRC_109)),
      na.rm = TRUE
    )
  )

dendro_src_daily <- dendro_src_daily_trees[, c("date", "SRC_mean")]

saveRDS(dendro_gi, "./data_out/dendro_gi.rds")
saveRDS(dendro_src, "./data_out/dendro_src.rds")
saveRDS(dendro_daily_trees, "./data_out/dendro_daily_trees.rds")
saveRDS(dendro_src_daily, "./data_out/dendro_src_daily.rds")



