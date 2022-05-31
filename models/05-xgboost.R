library(modeltime)
library(timetk)
library(tidymodels)

anac <- readRDS("data-raw/anac.rds")

splits <- anac %>%
  arrange(DATA) %>%
  time_series_split(
    DATA,
    assess     = "12 months",
    cumulative = TRUE
  )

rec_obj <- recipe(PASSAGEIROS_PAGOS ~ ., training(splits)) %>%
  step_timeseries_signature(DATA) %>%
  step_rm(DATA, CARGA_PAGA_KG) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

glimpse(juice(prep(rec_obj)))

model_tbl <- expand.grid(
  learn_rate = c(0.0001, 0.001, 0.01, 0.1),
  mtry = c(3,5,7),
  trees = c(100, 500, 700),
  tree_depth = c(3,5)
) %>%
  create_model_grid(
    f_model_spec = boost_tree,
    engine_name  = "xgboost",
    mode         = "regression"
  )

model_wfset <- workflow_set(
  preproc = list(rec_obj),
  models = model_tbl$.models,
  cross = TRUE
)

model_parallel_tbl <- model_wfset %>%
  modeltime_fit_workflowset(
    data    = training(splits),
    control = control_fit_workflowset(
      verbose   = TRUE,
      allow_par = TRUE
    )
  )

calib_tbl <- model_parallel_tbl %>%
  modeltime_calibrate(
    new_data = testing(splits),
    id       = "AEROPORTO_DE_ORIGEM_UF"
  )

saveRDS(model_parallel_tbl, "results/05-xgboost.rds")

calib_tbl %>%
  modeltime_accuracy(acc_by_id = TRUE) %>%
  group_by(.model_id) %>%
  summarise(across(mae:rsq, ~mean(.x, na.rm = TRUE))) %>%
  arrange(mae)

forecasts <- model_parallel_tbl %>%
  filter(.model_id %in% c(72, 68, 60)) %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = anac,
    keep_data   = TRUE
  )

forecasts %>%
  filter(AEROPORTO_DE_ORIGEM_UF == "SP") %>%
  plot_modeltime_forecast()
