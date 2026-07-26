# =========================================================
# 06_logistic_models_and_lags.R
# Logistische Modelle für Wachstumstage und  Lag-Suche
# =========================================================

source("./Skripte/00_setup.R")

analysis_growing <- readRDS("./data_out/analysis_growing.rds")

# Logistisches Grundmodell
model_logit_0 <- glm(
  growth_day ~ temp_max + vpd_max + precip_sum,
  data = analysis_growing,
  family = binomial
)

summary(model_logit_0)

# Zusätzliches Modell mit kontinuierlicher Trockenperiode
model_logit_dry_streak <- glm(
  growth_day ~ dry_streak_before + temp_max + vpd_max + precip_sum,
  data = analysis_growing,
  family = binomial
)

summary(model_logit_dry_streak)

# Zusätzliches Modell mit binärer 5-Tage-Dürre
model_logit_drought5 <- glm(
  growth_day ~ drought_5d_before + temp_max + vpd_max + precip_sum,
  data = analysis_growing,
  family = binomial
)

summary(model_logit_drought5)

# Vergleich der Modellgüte
AIC(model_logit_0, model_logit_dry_streak, model_logit_drought5)
BIC(model_logit_0, model_logit_dry_streak, model_logit_drought5)

# Odds Ratios für die neuen Modelle
exp(coef(model_logit_dry_streak))
exp(cbind(OddsRatio = coef(model_logit_dry_streak), confint(model_logit_dry_streak)))

exp(coef(model_logit_drought5))
exp(cbind(OddsRatio = coef(model_logit_drought5), confint(model_logit_drought5)))

# Funktion für Lag-Modelle
fit_lag_logit <- function(data, lag_days) {
  
  df <- data %>%
    arrange(date) %>%
    mutate(
      temp_max_lag   = dplyr::lag(temp_max, lag_days),
      vpd_max_lag    = dplyr::lag(vpd_max, lag_days),
      precip_sum_lag = dplyr::lag(precip_sum, lag_days)
    )
  
  model <- glm(
    growth_day ~ temp_max_lag + vpd_max_lag + precip_sum_lag,
    data = df,
    family = binomial
  )
  
  data.frame(
    lag = lag_days,
    n = nobs(model),
    aic = AIC(model),
    bic = BIC(model),
    coef_temp = coef(model)["temp_max_lag"],
    coef_vpd = coef(model)["vpd_max_lag"],
    coef_precip = coef(model)["precip_sum_lag"],
    p_temp = coef(summary(model))["temp_max_lag", "Pr(>|z|)"],
    p_vpd = coef(summary(model))["vpd_max_lag", "Pr(>|z|)"],
    p_precip = coef(summary(model))["precip_sum_lag", "Pr(>|z|)"]
  )
}

logit_lag_results <- do.call(
  rbind,
  lapply(0:14, function(x) fit_lag_logit(analysis_growing, x))
)

logit_lag_results <- logit_lag_results[order(logit_lag_results$aic), ]
print(logit_lag_results)

best_lag <- logit_lag_results$lag[1]
message("Bester Lag nach AIC: ", best_lag)

best_logit <- analysis_growing %>%
  arrange(date) %>%
  mutate(
    temp_max_best   = dplyr::lag(temp_max, best_lag),
    vpd_max_best    = dplyr::lag(vpd_max, best_lag),
    precip_sum_best = dplyr::lag(precip_sum, best_lag)
  )

model_logit_best <- glm(
  growth_day ~ temp_max_best + vpd_max_best + precip_sum_best,
  data = best_logit,
  family = binomial
)

summary(model_logit_best)

# Odds Ratios
exp(coef(model_logit_best))
exp(cbind(OddsRatio = coef(model_logit_best), confint(model_logit_best)))

# -----------------------------------------
# Zusätzliche Gütemaße für das beste logistische Modell
# -----------------------------------------

# Nur vollständige Fälle des besten Modells verwenden
best_logit_eval <- best_logit %>%
  filter(
    !is.na(growth_day),
    !is.na(temp_max_best),
    !is.na(vpd_max_best),
    !is.na(precip_sum_best)
  )

# Nullmodell auf demselben Datensatz
model_null <- glm(growth_day ~ 1, data = best_logit_eval, family = binomial)

# Bestes Modell ebenfalls auf demselben Datensatz
model_logit_best_eval <- glm(
  growth_day ~ temp_max_best + vpd_max_best + precip_sum_best,
  data = best_logit_eval,
  family = binomial
)

# AIC und BIC
AIC(model_logit_best_eval)
BIC(model_logit_best_eval)

# Null- und Residual Deviance
model_logit_best_eval$null.deviance
model_logit_best_eval$deviance

# Likelihood-Ratio-Test gegen Nullmodell
anova(model_null, model_logit_best_eval, test = "Chisq")

# McFadden Pseudo-R²
1 - (model_logit_best_eval$deviance / model_logit_best_eval$null.deviance)

# Vorhergesagte Wahrscheinlichkeiten
best_logit_eval$pred_prob <- predict(model_logit_best_eval, type = "response")

# Brier Score
mean((best_logit_eval$growth_day - best_logit_eval$pred_prob)^2)


# Speichern
saveRDS(logit_lag_results, "./data_out/logit_lag_results.rds")
saveRDS(best_logit, "./data_out/best_logit.rds")
saveRDS(model_logit_best, "./data_out/model_logit_best.rds")
saveRDS(model_logit_dry_streak, "./data_out/model_logit_dry_streak.rds")
saveRDS(model_logit_drought5, "./data_out/model_logit_drought5.rds")
saveRDS(best_logit_eval, "./data_out/best_logit_eval.rds")
saveRDS(model_logit_best_eval, "./data_out/model_logit_best_eval.rds")

