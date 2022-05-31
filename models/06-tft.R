library(tidyverse)
library(timetk)
library(tft)
library(tidymodels)

anac <- readRDS("data-raw/anac.rds")
anac <- anac %>%
  distinct(DATA) %>%
  mutate(DATA2 = lubridate::ymd("2020-01-01") + lubridate::days(seq_along(DATA))) %>%
  right_join(anac)
split <- time_series_split(anac, DATA, assess = "12 months", cumulative = TRUE)
split2 <- time_series_split(training(split), DATA, assess = "12 months", cumulative = TRUE)

train <- training(split2)
valid <- testing(split2)
test <- testing(split)

rec <- recipe(PASSAGEIROS_PAGOS ~ ., data = train) %>%
  step_mutate(
    date_time_since_begining = as.numeric(difftime(
      time1 = DATA,
      time2 = lubridate::ymd(min(anac$DATA)),
      units = "weeks"
    )),
    date_month = as.factor(lubridate::month(DATA))
  ) %>%
  step_rm(DATA) %>%
  step_normalize(all_numeric_predictors())

spec <- tft_dataset_spec(rec, train) %>%
  spec_covariate_index(DATA2) %>%
  spec_covariate_key(AEROPORTO_DE_ORIGEM_UF) %>%
  spec_covariate_known(starts_with("date_")) %>%
  spec_time_splits(lookback = 48, horizon = 12) %>%
  prep()

model <- temporal_fusion_transformer(
  spec,
  hidden_state_size = 8,
  learn_rate = 1e-3,
  dropout = 0.6,
  num_attention_heads = 1,
  num_lstm_layers = 1
)

fitted <- model %>%
  fit(
    transform(spec),
    valid_data = transform(spec, new_data = valid),
    epochs = 10,
    verbose = TRUE,
    dataloader_options = list(batch_size = 64),
    callbacks = list(
      luz::luz_callback_keep_best_model(monitor = "valid_loss")
    )
  )

preds <- predict(fitted, past_data = bind_rows(train, valid), new_data = test)
preds <- preds %>%
  bind_cols(test)

metrics <- yardstick::metric_set(mae, mape, mase, smape, rmse, rsq)

acc <- preds %>%
  group_by(AEROPORTO_DE_ORIGEM_UF) %>%
  arrange(DATA) %>%
  metrics(PASSAGEIROS_PAGOS, .pred) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

saveRDS(acc, "results/06-tft.rds")

acc %>%
  summarise(across(mae:rsq, mean))

# # A tibble: 1 × 6
# mae  mape  mase smape   rmse    rsq
# <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
# 1 28700.  12.0  1.43  11.4 33715. 0.0215


