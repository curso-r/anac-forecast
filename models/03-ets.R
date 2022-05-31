library(tidymodels)
library(timetk)
library(modeltime)

nested_anac <- readRDS("data-raw/nested_anac.rds")

rec <- recipe(PASSAGEIROS_PAGOS ~ DATA, data = extract_nested_train_split(nested_anac))

# model 1 : ETS ajustado automatico

ets <- modeltime::exp_smoothing() %>%
  set_engine("ets")
ets_auto <- workflow() %>%
  add_model(ets) %>%
  add_recipe(rec)

# model 2: theta method

theta <- modeltime::exp_smoothing() %>%
  set_engine("theta")

theta <- workflow() %>%
  add_model(theta) %>%
  add_recipe(rec)

nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_anac,
  ets_auto,
  theta
)

saveRDS(nested_modeltime_tbl, "results/03-ets.rds")

# mostrar predicoes
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "SP") %>%
  plot_modeltime_forecast()

nested_modeltime_tbl %>%
  extract_nested_modeltime_table(1) %>%
  purrr::pluck(".model")

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(.model_id) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE)))

# # A tibble: 2 × 7
# .model_id    mae  mape  mase smape   rmse   rsq
# <int>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1         1 12560.  6.56 0.779  6.41 15178. 0.603
# 2         2 18654.  7.84 0.937  7.69 23306. 0.201

