library(tidymodels)
library(timetk)
library(modeltime)

nested_anac <- readRDS("data-raw/nested_anac.rds")

rec <- recipe(PASSAGEIROS_PAGOS ~ DATA, data = extract_nested_train_split(nested_anac))

# model 1 : Auto arima sem nada

arima <- modeltime::arima_reg() %>%
  set_engine("auto_arima")
arima_auto <- workflow() %>%
  add_model(arima) %>%
  add_recipe(rec)

# model 2: Arima + tendencia

rec <- recipe(PASSAGEIROS_PAGOS ~ DATA, data = extract_nested_train_split(nested_anac)) %>%
  step_mutate(
    tendencia = as.numeric(difftime(DATA, lubridate::ymd("2000-01-01"), units = "days"))
  )
arima <- modeltime::arima_reg() %>%
  set_engine("auto_arima")

arima_tendencia <- workflow() %>%
  add_model(arima) %>%
  add_recipe(rec)


# model 3 tbats

rec <- recipe(PASSAGEIROS_PAGOS ~ DATA, data = extract_nested_train_split(nested_anac))

tbats <- modeltime::seasonal_reg() %>%
  set_engine("tbats")
tbats_auto <- workflow() %>%
  add_model(tbats) %>%
  add_recipe(rec)

nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_anac,
  arima_auto,
  arima_tendencia,
  tbats_auto,
  control = control_nested_fit(allow_par = TRUE)
)

saveRDS(nested_modeltime_tbl, "results/04-arima.rds")

# mostrar predicoes
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "RJ") %>%
  plot_modeltime_forecast()

nested_modeltime_tbl %>%
  extract_nested_modeltime_table(2) %>%
  purrr::pluck(".model")

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>% view() %>%
  group_by(.model_id) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE)))

# # A tibble: 3 × 7
# .model_id    mae  mape  mase smape   rmse   rsq
# <int>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1         1 19397.  8.21 1.05   8.55 21954. 0.632
# 2         2 16870.  6.86 0.845  6.96 19257. 0.685
# 3         3 13730.  6.06 0.745  6.06 15977. 0.544

