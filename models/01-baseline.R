library(timetk)
library(modeltime)
library(tidymodels)

nested_anac <- readRDS("data-raw/nested_anac.rds")

# modelos baseline
rec <- recipe(PASSAGEIROS_PAGOS ~ DATA, data = extract_nested_train_split(nested_anac))
naive <- modeltime::naive_reg() %>%
  set_engine("naive")

naive <- workflow() %>%
  add_model(naive) %>%
  add_recipe(rec)

snaive <- modeltime::naive_reg(seasonal_period = 12) %>%
  set_engine("snaive")
snaive <- workflow() %>%
  add_model(snaive) %>%
  add_recipe(rec)

nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_anac,
  naive,
  snaive
)

saveRDS(nested_modeltime_tbl, "results/01-baseline.rds")

# mostrar predicoes
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "AM") %>%
  plot_modeltime_forecast()

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(.model_desc) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE)))


# # A tibble: 2 × 7
# .model_desc    mae  mape  mase smape   rmse     rsq
# <chr>        <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
# 1 "NAIVE"     22643. 10.1  1.20   9.67 27618. NaN
# 2 "SNAIVE "   16141.  7.05 0.876  7.12 18989.   0.480
