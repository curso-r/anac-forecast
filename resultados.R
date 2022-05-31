library(modeltime)
library(timetk)
library(tidyverse)

files <- c("results/01-baseline.rds", "results/02-regressao.rds", "results/03-ets.rds",
           "results/04-arima.rds")

results <- files %>%
  set_names() %>%
  map(readRDS)

# modelo globais
anac <- readRDS("data-raw/anac.rds")
splits <- anac %>%
  arrange(DATA) %>%
  time_series_split(
    DATA,
    assess     = "12 months",
    cumulative = TRUE
  )
xgb <- readRDS("results/05-xgboost.rds")
calib_tbl <- xgb %>%
  modeltime_calibrate(
    new_data = rsample::testing(splits),
    id       = "AEROPORTO_DE_ORIGEM_UF"
  )
acc_xgb <- calib_tbl %>%
  modeltime_accuracy(acc_by_id = TRUE) %>%
  mutate(arq = "05-xgboost")

# tft
acc_tft <- readRDS("results/06-tft.rds") %>%
  mutate(arq = "06-tft", .model_id = 1)

acc_por_estado <- results %>%
  map_dfr(extract_nested_test_accuracy, .id = "arq") %>%
  bind_rows(acc_xgb, acc_tft)

acc <- acc_por_estado %>%
  group_by(arq, .model_id) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE)))



