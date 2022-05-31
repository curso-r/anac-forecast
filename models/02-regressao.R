library(tidymodels)
library(timetk)
library(modeltime)

anac <- readRDS("data-raw/anac.rds")
anac %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "SP") %>%
  plot_time_series(DATA, PASSAGEIROS_PAGOS)

nested_anac <- readRDS("data-raw/nested_anac.rds")

rec <- recipe(PASSAGEIROS_PAGOS ~ ANO + MES + DATA, data = extract_nested_train_split(nested_anac)) %>%
  step_num2factor(MES, levels = readr::date_names_lang("pt")$mon) %>%
  step_mutate(
    tendencia = as.numeric(difftime(DATA, lubridate::ymd("2000-01-01"), units = "days"))
  ) %>%
  step_rm(DATA, ANO)
linear <- parsnip::linear_reg() %>%
  set_engine("lm")

# model 1 : regressao simples com tendencia + mes
reg_simples <- workflow() %>%
  add_model(linear) %>%
  add_recipe(rec)

# model2 : regressao com interacao entre tendencia e mes
rec_inter <- rec %>%
  step_interact(terms = ~tendencia:MES)
reg_inter <- workflow() %>%
  add_model(linear) %>%
  add_recipe(rec_inter)

# model3 : regressao adicionando smoothing p/ a tendencia
rec_spline <- rec %>%
  step_bs(tendencia)
reg_spline <- workflow() %>%
  add_model(linear) %>%
  add_recipe(rec_spline)

# model4: regressao com termo qudrático
rec_poly <- rec %>%
  step_poly(tendencia, degree = 2)
reg_poly <- workflow() %>%
  add_model(linear) %>%
  add_recipe(rec_poly)

# modelo 5: regressao com grau 3
rec_poly3 <- rec %>%
  step_poly(tendencia, degree = 3)
reg_poly3 <- workflow() %>%
  add_model(linear) %>%
  add_recipe(rec_poly3)


nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_anac,
  reg_simples,
  reg_inter,
  reg_spline,
  reg_poly,
  reg_poly3
)

saveRDS(nested_modeltime_tbl, "results/02-regressao.rds")

# mostrar predicoes
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "SP") %>%
  plot_modeltime_forecast()

nested_modeltime_tbl %>%
  extract_nested_modeltime_table(2) %>%
  purrr::pluck(".model")

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  group_by(.model_desc, .model_id) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE)))

# # A tibble: 5 × 8
# # Groups:   .model_desc [1]
# .model_desc .model_id    mae  mape  mase smape   rmse   rsq
# <chr>           <int>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 LM                  1 62343.  25.3  3.11  22.0 63490. 0.699
# 2 LM                  2 62308.  25.1  3.12  21.8 63581. 0.693
# 3 LM                  3 42867.  15.9  2.02  18.0 48301. 0.229
# 4 LM                  4 32817.  12.1  1.49  11.3 35018. 0.665
# 5 LM                  5 42867.  15.9  2.02  18.0 48301. 0.229

