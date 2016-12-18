# example of ideal model flow

load_tse(file, time_col, value_col, value_name) {
  # load dset
  
  # select columns
  
  # create time columns
  
  # set availability
  
  #
  data <- 0
  
  # set tse attributes
  
}

restore_tse(file){
  stopifnot(is.tse(data))
}

is.tse(data) {
  # check class attributes and columns
  t1 <- inherits(data, 'tse') &
    all(c('local_time', 'year', 'month', 
          'wday', 'holiday', 'period', 'is_available') %in% colnames(data))
  
  # check that dataset is sorted according to time, and in SGT
  
  # check that the availability flag is correctly set
  
}

s_ts <- loadFromFile('sg-energy.csv', time_col = 1, value_col = 4, out_name = 'x1') %>%
  clean(outlier_c = 10) %>%
  clean_period(method = 'round', interval = 48)

weather_ts <- loadFromFile('weather.csv', time_col = 1, value_col = 2:4, 
                           out_name = c('T', 'dp', 'rh')) %>%
  clean(outlier_c = 10, value_col = c('T', 'dp', 'rh')) %>%
  clean_period()

##########################################################
# Model side
##########################################################


ts <- ts_merge(s_ts, weather_ts) %>% split_decorate(70/100)

lm_trend <- ts %>% select_train %>% Lmfactory('x1 ~ local_time', out = 'x1.trend')

dcm <- compose_out(add_operator(lm_trend), DcmFactory('x1'), )

model interface (no time shifting):
  train(ts, value_vars)
  predict(ts, out_vars)
  predict_1row(ts, in_vars)

model interface (non-continuous time shifting):
  train(ts, value_vars)
  predict(ts, out_vars, seed = values or flag?) # iterative
  predict_1row(ts, in_vars)
  fit_1step(ts, fitflag)
  fit_step(ts, nstep, fitflag)
  
# how to deal with missing values: from the dataset, from certain algorithm: eg diff

##########################################################
# View side
##########################################################