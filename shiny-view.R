source(file.path('..', 'core', 'daily_cluster_model.R'), chdir = TRUE)
source(file.path('..', 'core', 'shuya_SVR.R'), chdir = TRUE)

view_col_selection <- function(tse, input_id = 'SC_choose_cols'){
  # input check
  if(!is.tse(tse)) return(NULL)
  cols <- c(get_value_cols(tse), 'available')
  checkboxGroupInput(input_id, 
                       label = "Which time-series?", 
                       choices = structure(as.list(cols), names = cols),
                       selected = NULL)
}


algos <- list()

algos$none <- list(
  ui = function(input, tsev) {
    p('Nothing to see here')
  },
  build = function(train_data, input) {
    NULL
  })

algos$del <- list(
  ui = function(input, tsev) {
    checkboxGroupInput('SE_col_sel', 'Column selection',
                      choices = get_value_cols(tsev$tse))
  },
  build = function(input, tsev) {
    NULL
  })

algos$split <- list(
  ui = function(input, tsev) {
    div(
      h3('Description'),
      h3('Parameters'),
      textInput("SE_map_out", label = "Mapping out"),
      sliderInput('SE_split_ratio', 'Split ratio', 0, 100, 
                  default_split_ratio)
    )
  },
  build = function(input, tsev) {
    tsev$tse <- tsev$tse %>% split_tse(input$SE_split_ratio/100, input$SE_map_out)
    tsev <- add_algo_meta(tsev, algo_meta('Create train column', 
                                          split_ui(input$SE_split_ratio, input$SE_map_out), NULL, NULL))
    tsev
  })

algos$daily_profiles <- list(
  ui = function(input, tsev) {
    div(
      h3('Description'), 
      p(
        'This algorithm compute 1 daily profile for each day group',
        'and uses it for forecasting. Days can be grouped in different',
        'ways depending on the position in the week, and whether it is a public holiday'
      ),
      h3('Parameters'),
      fluidRow(
        column(width = 4, selectInput("SE_map_x", label = "Map x", 
                                      choices = get_value_cols(tsev$tse))),
        column(width = 4, selectInput("SE_map_train", label = "Map train", 
                                      choices = get_value_cols(tsev$tse))),
        column(width = 4, textInput("SE_map_out", label = "Map out"))
      ),
      radioButtons("SE_cluster", label = 'Day clustering',
                   choices = list("7 groups: Monday to Sunday" = 'time.wday', 
                                  "2 groups: worked (Mon-Fri) and non-worked (weekend+ph)" = 'worked',
                                  "3 groups: worked, weekend and ph" = 'w_we_ph',
                                  "3 groups: worked, Saturdays, Sunday+ph" = 'wd_s_sph'), 
                   selected = 'time.wday')
    ) # END div
  },
  build = function(input, tsev) {
    store_algo(tsev, function (tse) { daily_cluster_model(tse, input$SE_cluster, input$SE_map_x) },
                'Daily profiles model', input$SE_map_x, input$SE_map_out, input$SE_map_train)
  }
)

algos$ddcm <- list(
  ui = function(input, tsev) {
    div(
      h3('Description'),
      p(
        'This algorithm compute 1 daily profile for each day group',
        'and uses it for forecasting. Days can be grouped in different',
        'ways depending on the position in the week, and whether it is a public holiday'
      ),
      h3('Parameters'),
      radioButtons("algo_ui_cluster", label = 'Day clustering',
                   choices = list("7 groups: Monday to Sunday" = 'time.wday', 
                                  "2 groups: worked (Mon-Fri) and non-worked (weekend+ph)" = 'worked',
                                  "3 groups: worked, weekend and ph" = 'w_we_ph'), 
                   selected = 'wday')
    ) # END div
  },
  build = function(input, tsev) {
    tsev
  }
)

algos$exponential_smoothing <- list(
  ui = function(input, tsev) {
    # set the default choice of the component select
    choice <- input$SE_components

    # HTML UI
    tagList(
      h3('Description'),
      p(
        'Exponential smoothing is one of the simplest',
        'time series prediction and smoothing method.',
        'It can take into account a trend and a seasonality in the data.'
      ),
      p(
        'However it cannot take into consideration other factors such as weekdays',
        'and weather data'),
      h3('Parameters'),
      
      # mapping parameters
      fluidRow(
        column(width = 4, selectInput("SE_map_x", label = "Map x", 
                                      choices = get_value_cols(tsev$tse))),
        column(width = 4, selectInput("SE_map_train", label = "Map train", 
                                      choices = get_value_cols(tsev$tse))),
        column(width = 4, textInput("SE_map_out", label = "Map out"))
      ),
      
      # model parameters
      fluidRow(
        column(width = 6, 
          checkboxGroupInput('SE_components', 'Components', 
                              choices = list('Trend' = 1, 'Seasonal' = 2), selected = choice)),
        column(width = 6,
          numericInput('SE_alpha', 'Alpha-value', 0.3, min = 0, max = 1, step = 0.1),
          if(1 %in% choice) numericInput('SE_beta', 'Beta-value (trend)', 0.1, min = 0, max = 1, step = 0.1),
          if(2 %in% choice) tagList(
            numericInput('SE_gamma', 'Gamma-value (seasonality)', 0.1, min = 0, max = 1, step = 0.1),
            numericInput('SE_frequency', 'Frequency', 24, min = 1, max = dim(tsev$tse)[1], step = 1))
          )
      ),
      
      # how many steps ahead prediction
      numericInput('SE_n_step', 'N-steps', 1, min = 1, max = dim(tsev$tse)[1], step = 1, width = '25%')
    )
  },
  build = function(input, tsev) {
    f <- 1
    if(!is.null(input$SE_frequency)) f <- input$SE_frequency
    alpha = NULL
    beta = FALSE
    gamma = FALSE
    if(1 %in% input$SE_components) beta = input$SE_beta
    if(2 %in% input$SE_components) gamma = input$SE_gamma
    tsev$tse %>% print
    tsev$tse %>% tail %>% print
    tsev$tse %>%
      filter_(input$SE_map_train) %>% dim %>% print
    # model training
    t1 <- system.time(
      m <- tsev$tse %>%
        filter_(input$SE_map_train) %>% # select only train data
        as.data.frame %>%
        select_(input$SE_map_x) %>% # select the column to be smoothed 
        ts(frequency = f) %>%
        HoltWinters(alpha = alpha, beta = beta, gamma = gamma)
    )
    # prediction / fitting
    t2 <- system.time({
      n_start <- if(1 %in% input$SE_components) 2 else 1
      if(2 %in% input$SE_components) n_start <- f
      n_start %>% print
      dim(m$fitted) %>% print
      sum(!tsev$tse[,input$SE_map_train]) %>% print
      tsev$tse[, input$SE_map_out] <- c(tsev$tse[1:n_start,input$SE_map_x], # start values
                                        m$fitted[, 1], # fitted values
                                        predict(m, n.ahead = sum(!tsev$tse[,input$SE_map_train]))) # for the test set
    })
    
    # compute perf indicators
    perf <- performance_indicators(tsev$tse, 
                                   input$SE_map_x, input$SE_map_out, 
                                   input$SE_map_train, t1[[3]], t2[[3]], dim(tsev$tse)[1])
    
    # update dataset with algorithm metadata
    add_algo_meta(tsev, 
                  algo_meta('Exponential smoothing', NULL, 
                            t(structure(c(perf, recursive = TRUE), 
                                        names = names(perf))), NULL))
  }
)


ui_variable_lag <- function(input, tsev, var_name) {
  map_id <- paste0('SE_map_', var_name) # SE_map_X
  D_id <- paste0('SE_', var_name, '_D') # SE_X_D
  label_map <- paste('Map', var_name) # Map X
  tagList(
    column(width = 3, 
           selectInput(map_id, label = label_map, 
                       choices = get_value_cols(tsev$tse))),
    column(width = 1, 
           numericInput(D_id, label = "Lag", 
                        4, min = 0, max = 100, step = 1))
    )
}

algos$shuya_svm <- list(
  ui = function(input, tsev) {
    # set the default choice of the component select
    choice <- input$SE_components
    n_predictor <- 3
    if(!is.null(input$SE_n_predictor)) n_predictor <- input$SE_n_predictor
    # HTML UI
    tagList(
      h3('Description'),
      p(
        'Support Vector Machine (SVM) is one of the most popular tool',
        'for prediction and classification. It can model non-linearities,',
        'dependance on past values and external factors like weather.'
      ),
      p(
        'We can select the variables that SVM will perform onto. Usually these',
        'variables include weather and the load that we want to predict. For each',
        'of those variables we can select lag orders. This is when we expect not only',
        'the current value of the variable to influence the output (load) but also its past values'
      ),
      p(
        'Since this algorithm makes use of previous values,',
        'It will have different performance according to how far',
        'in the future it is predicting. Currently we are outputting',
        'the forecasts 1,2,3,4,12 and 24 steps in the future'
      ),
      h3('Parameters'),
      
      # mapping parameters
      fluidRow(
        ui_variable_lag(input, tsev, 'X'),

        column(width = 4, 
          numericInput('SE_gamma', 'Gamma-value', 0.5, min = 0, max = 1, step = 0.1),
          numericInput('SE_cost', 'Cost', 500, min = 0, max = 2000, step = 100)),
        
        column(width = 4,
          textInput("SE_map_out", label = "Map out"),
          selectInput("SE_map_train", label = "Map train", 
                           choices = get_value_cols(tsev$tse)))
      ),
      numericInput('SE_n_predictor', 'Number of predictor variables', 
                   n_predictor, min = 0, max = length(get_value_cols(tsev$tse)), step = 1),
      
      div(class = 'panel panel-default',
        div(class = 'panel-body',
          {
          if(n_predictor == 0) 
            div()
          else {
            all_predictor_ui <- div()
            # mapping of predictor variables
            for(i in 1:ceiling(n_predictor/3)) { # number of rows: 3 variables per row
              r <- fluidRow()
              for(j in (3*(i-1)+1):min(3*i, n_predictor))
                r <- tagAppendChild(r, ui_variable_lag(input, tsev, paste0('Y', j))) # build each row column by column
              all_predictor_ui <- tagAppendChild(all_predictor_ui, r) # concatenate the rows together
            }
          all_predictor_ui
          }
          }
        ) # panel body
      ) # panel
  )
  },
  build = function(input, tsev) {
    # mapping of input columns
    var_list <- get_multiple(input, c('SE_X_D', paste0('SE_', 'Y', 1:input$SE_n_predictor, '_D'))) # variable D orders
    var_list %>% print
    orders <- lapply(var_list, FUN = function(x) 1:x)
    orders %>% print
    names(orders) <- get_multiple(input, c('SE_map_X', paste0('SE_map_', 'Y', 1:input$SE_n_predictor))) # variable names
    orders %>% print
    #orders <- structure(list(1:input$SE_X_D, 1:input$SE_Y1_D, 1:input$SE_Y2_D, 1:input$SE_Y3_D), 
    #  names = c(input$SE_map_X, input$SE_map_Y1, input$SE_map_Y2, input$SE_map_Y3))
    
    store_forecasting(tsev, function (tse) { 
      shuya_svm(tse, input$SE_gamma, input$SE_cost, input$SE_map_X,
                .orders_list = orders) },
               'SVM model', input$SE_map_X, input$SE_map_out, input$SE_map_train)
  }
)

get_multiple <- function(reav, js) {
  r <- list()
  for(j in js) {
    r <- c(r, reav[[j]])
  }
  r
}