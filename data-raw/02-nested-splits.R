anac <- readRDS("data-raw/anac.rds")

nested_anac <- anac %>%
  modeltime::extend_timeseries(AEROPORTO_DE_ORIGEM_UF, .date_var = DATA, 12) %>%
  modeltime::nest_timeseries(AEROPORTO_DE_ORIGEM_UF,.length_future = 12, .length_actual = 160) %>%
  modeltime::split_nested_timeseries(.length_test = 12)

saveRDS(nested_anac, "data-raw/nested_anac.rds")
