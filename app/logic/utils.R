
#### Function 1: Get new preds ####
#' @export
get_dat = function(state){

  box::use(
    magrittr[`%>%`],
    dplyr[mutate, filter, reframe, bind_rows, left_join],
    tidyr[unnest],
    lubridate[ymd, today, year, yday, days],
    glue[glue],
    sf[st_drop_geometry],
    #terra[rast],
    #downloader[download],
    arrow[read_parquet]
  )

  # dat_cols = read_parquet('app/data/dat_cols.parquet')
  # elev = read_parquet('app/data/elevation.parquet')
  # daylight = read_parquet('app/data/iowa_daylight.parquet')

  # Also population, income for completeness

  # # GRIDMET
  # features = c('tmmx') # 'srad', 'th', 'vs' - leave out for now, will likely run into scaling issues due to file size
  #
  # # later collect by timezone, for now just use UTC for mvp
  # download(
  #   url = glue('http://www.northwestknowledge.net/metdata/data/{feature}_{yr}.nc'),
  #   # destfile = glue('{dat_dir}/{feature}/{feature}_{yr}.nc'),
  #   mode = 'wb'
  # )

  # cal = data.frame(date = seq.Date(from = today(), to = today() + months(1)))
  cal = data.frame(date = seq.Date(from = glue('{year(today())}-1-1'),
                                   to = glue('{year(today())}-12-31'))
                   ) %>%
    mutate(
      julian_day = yday(date)
    )

  dat = read_parquet(glue('app/data/by_state/{state}_preds.parquet')) %>%
    left_join(cal) %>%
    mutate(
      pred = (pred - min(pred)) / (max(pred) - min(pred))
    )

  return(dat)

  # dat = grid %>%
  #   st_drop_geometry() %>%
  #   filter(state %in% states) %>%
  #   reframe(date = cal$date, .by = c(key, state)) %>%
  #   unnest(date) %>%
  #   mutate(
  #     key = as.character(key),
  #     julian_day = yday(date),
  #     year = year(today())
  #   ) %>%
  #   # Join pre-prepared features
  #   left_join(elev) %>%
  #   left_join(daylight)

  # Explicitly generate (empty) columns for all other model features

  # return(
  #   bind_rows(dat_cols, dat)
  #   )
}


#### Function 2: Get new preds ####
#' @export
get_preds = function(dat){

  box::use(
    magrittr[`%>%`],
    dplyr[select, any_of, arrange, mutate],
    lubridate[year],
    stats[predict],
    bundle[unbundle],
    xgboost[...],
    parsnip[...],
    workflows[...]
  )

  drop_list = c('site', 'collection_date', 'longitude', 'latitude', 'species', 'imputed_days', 'exclude', 'date_added_utc', 'has_aphid_data_14d', 'has_aphid_data_21d', 'demog_infilled', 'survey_year', 'date_combined_utc')

  mod = unbundle(readRDS('app/data/mod.rds'))

  preds = dat %>%
    select(-any_of(drop_list)) %>%
    arrange(date) %>%
    mutate(
      #key = as.character(key),
      year = year(date)
      ) %>%
    mutate(
      pred = predict(object = mod, ., type="raw")
    ) %>%
    select(key, pred, date, prism_ppt, prism_tmean)

  return(preds)
}
