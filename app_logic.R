# controller part
# the controller holds the app logic

#------------------------------------------------------------------------------#
# tse_visu class

#' tse_visu is the data structure that holds the T.S. Handyman app state
#' 
#' @description tse_visu is composed of 2 part: one is a tse object which stores
#' a bunch of time-series which share the same time indexation. The other is
#' a list of algo_meta objects that describes algorithms that have been applied
#' to the tse objects.
#' The objective of the T.S. Handyman is to visualize data and validate
#' algorithms. There is a close interplay between the algorithms and the
#' visualizations because on one hand algorithms outputs are often time-series
#' that can be visualized as well and on the other hand because algorithms
#' transforming the time-series can help understand them and make the
#' visualization much more telling.
tse_visu <- function(tse, algo_meta) {
  stopifnot(is.tse(tse))
  
  structure(list(tse = tse, algo_meta = algo_meta),
            class = c('tse_visu'),
            is_null = FALSE)
}

#' tsev_null is a special value for an object of class tse_visu representing
#' an error state.
#' 
#' @description Sometimes the application can reach a state of error for example
#' if data loading has failed or if an algorithm returns a corrupt state, or
#' before any data has been loaded. This is the way the app handle errors.
tsev_null <- structure(list(tse = NULL, algo_meta = NULL),
                      class = c('tse_visu'),
                      is_null = TRUE)

#' Function that checks if the state of the app is valid
#' 
#' @description All functions operating should maintain the app state as valid.
#' That means that before each function call is.tse_visu should evaluate to
#' TRUE and after each state change the state should also evaluate to TRUE. If
#' there is an exception or error and a function cannot execute normally, the
#' function should output a NULL state, represented by tsev_null, which is
#' considered a valid state.
#' @param tsev A tse_visu object
#' @return Boolean value. TRUE if the app state is valid FALSE if the app state
#' is invalid.
is.tse_visu <- function(tsev) {
  if(!inherits(tsev, 'tse_visu')) return(FALSE)
  
  if(is_null_tsev(tsev)) return(TRUE)
  
  is.tse(tsev$tse)
}

#' Checks if the app state is null, convenience function
#' @return TRUE if the app state is the null state
is_null_tsev <- function(tsev) {
  attributes(tsev)$is_null
}

#------------------------------------------------------------------------------#
# Algo meta class

#' Constructor for an object of class algo_meta. 
#' 
#' @description Stores meta information about an algorithm that has been 
#' applied to a time-series strand. This information is bound to be displayed
#' in a Shiny app context and particularly under the management of Mr T.S.
#' Handyman. 
#' As such it contains basic information such as a name and a timestamp. It also
#' contains some information about the algorithms. One custom UI: a tagList, a
#' list of data.frames, a list of ggplot2 objects, an object with a print
#' function. Currently implemented: list of data.frames, list of ggplot2 
#' objects.
#' 
#' @param custom_ui A tagList, some HTML or a character vector that can be
#'  directly displayed
#' @param show_table A list of data.frame
#' @param show_graph A list of ggplot2 objects
#' @param show_summary Any object with a print function
#' @return Object of class algo_meta
algo_meta <- function(name, custom_ui, show_table, show_graph, show_summary,
                      timestamp = Sys.time()) {
  structure(list(name = name, 
                 timestamp = timestamp, 
                 custom_ui = custom_ui,
                 show_table = show_table,
                 show_graph = show_graph,
                 show_summary = show_summary), 
            class = 'algo_meta')
}

name <- function(ameta) { ameta$name }

name_vec <- function(algo_meta) { sapply(algo_meta, FUN = name) }

#' Checks if an algo_meta is valid
#' @return boolean 
is.algo_meta <- function(obj) {
  # class
  if(!inherits(obj, 'algo_meta')) return(FALSE)
  # name is not null
  if(is.null(obj$name)) return(FALSE)
  # timestamp is not null
  if(is.null(obj$timestamp)) return(FALSE)
  
  return(TRUE)
}

# algo is just a tse_fun that returns a tse_fun
# the wrapper take an algo
# wrapper that takes an algo, compute the perfs
# algo_meta <- function(perf, name, mapping, timestamp, train_ratio) {
#   structure(list(perf = perf, mapping = mapping, 
#                  timestamp = timestamp, train_ratio = train_ratio), 
#             name = name, 
#             class = 'algo_meta')
# }



################################################################################
# Models and validation

# previous version - no longer in use
store_algo2 <- function(tsev, algo, ratio, name, ...) {
  mapping <- construct_mapping(...)
  mapping$train = 'train'
  r <- run_model(tsev$tse %>% split_tse(ratio, 'train'), algo, mapping)
  tse <- r$tse
  tsev$algo_meta <- c(tsev$algo_meta, 
                      algo_meta(r$perf, name, mapping, Sys.time(), ratio))
  
  tsev
}

# function to compute metrics about an algo that does not use previous values
store_algo <- function(tsev, algo, name, colx, colout, coltrain) {
  
  # model training
  t1 <- system.time(
    m <- tsev$tse %>%
      filter_(coltrain) %>% # select only train data
      algo
  )
  
  # prediction / fitting
  t2 <- system.time(
    tsev$tse[, colout] <- predict(m, tsev$tse)
  )
  
  # compute perf indicators
  perf <- performance_indicators(tsev$tse, colx, colout, coltrain, t1[[3]],
                                 t2[[3]], dim(tsev$tse)[1])
  
  # update dataset with algorithm metadata
  add_algo_meta(tsev, 
                algo_meta(name, NULL, t(structure(c(perf, recursive = TRUE),
                                                  names = names(perf))), NULL))
}

#' function to compute metrics about a forecasting algorithm
#' 
#' @description This function runs a forecasting algorithm. Forecasting
#' algorithms have different perfomance according to how far they forecast in
#' the future, That is why this function can
#' Data stored include algorithms metrics performance: speed and
#' accuracy and mapping information. The speed information is contained in a 
#' single data.frame. The performance information is stored in 3 data.frames
#' and a graph. The first data.frame is the accuracy: MAPE+r2 for the train set
#' for each forecasting order. The second is the same of the test set. Finally
#' a third one containing only one row is there to give an overview.
#' Finally a graph sums up the testing information by plotting the MAPE in
#' function of the forecasting interval.
#' 
#' @param algo A function taking a tse and outputing a trained forecasting 
#' model.
#' @param tsev A valid app state of the class tse_visu
#' @param n How many iteration of the algorithm should be tested
#' @return A valid application state
store_forecasting <- function(tsev, algo, name, colx, colout, coltrain, 
                              n = 24) {
  
  # model training
  t1 <- system.time(
    m <- tsev$tse %>%
      filter_(coltrain) %>% # select only train data
      algo
  )
  
  # prediction / fitting
  t2 <- system.time({
    r <- predict_n(tsev$tse, m, 24)
    tsev$tse[, paste0(colout, '.f', 1)] <- r[, 'f1']
    tsev$tse[, paste0(colout, '.f', 2)] <- r[, 'f2']
    tsev$tse[, paste0(colout, '.f', 3)] <- r[, 'f3']
    tsev$tse[, paste0(colout, '.f', 4)] <- r[, 'f4']
    tsev$tse[, paste0(colout, '.f', 12)] <- r[, 'f12']
    tsev$tse[, paste0(colout, '.f', 24)] <- r[, 'f24']
  })
  
  perf <- list()
  # compute speed perf indicators
  perf$howfast <- perf_speed(tsev$tse[,coltrain], t1[[3]], t2[[3]], 
                        dim(tsev$tse)[1] * n) %>%
    # transpose so that is fits better on the screen
                        t
  
  train_flag <- tsev$tse[, coltrain]
  # compute train accuracy metrics
  perf$'howgood - training set' <- perf_accuracy(
    tsev$tse[train_flag, colx],
    r[train_flag, ])
  
  # compute test accuracy metrics
  perf$'howgood - test set' <- perf_accuracy(
    tsev$tse[!train_flag, colx],
    r[!train_flag, ])
  
  # compute summary performance indicators
  perf$'howgood - summary' <- 
    # average the performance operators for each forecasting orders
    vapply(perf$'howgood - test set', mean, 1) %>%
    # transpose so that it shows more nicely in shiny
    t
  
  # plotting object to summarize the prediction accuracy according to
  # forecasting step
  perf_graph <- list()
  testperf <- perf$'howgood - test set'
  # convert the rownames from f3 to 3, f1 to 1 etc
  testperf$howfar <- sub(x = rownames(testperf), pattern = 'f', replacement = '') %>%
    as.numeric()
  
  perf_graph$'howgood: a graph' <- 
    melt(testperf, id.vars = 'howfar') %>%
    ggplot(aes_string(x = 'howfar', y = 'value', 
                      group = 'variable', color = 'variable'))
  perf_graph$'howgood: a graph' <- 
    perf_graph$'howgood: a graph' +
    geom_line()
  
  # update dataset with algorithm metadata
  add_algo_meta(tsev, 
    algo_meta(name, NULL, perf, perf_graph, NULL))
}

#' Load tse_visu from file
#' 
#' @description Checks that the file exists and is valid
#' @return A list with 2 values: 1 status message that indicates either the
#' success of the loading or if it failed what is the reason. The second
#' element is the loaded tse_visu in case of success or a null tsev in case
#' of failure.x
restore <- function(file) {
  r <- list()
  # check input validity
  if(missing(file) | is.null(file)){
    r$message <- 'No input file'
    r$tsev <- tsev_null
    return(r)
  }
  # check file existence
  if(!file.exists(file)){
    r$message <- 'File not found'
    r$tsev <- tsev_null
    return(r)
  }
  # load the file
  var_name <- load(file)
  r$tsev <- get(var_name)
  if(!is.tse_visu(r$tsev)) { # if file loaded is not valid tsve
    r$message <- 'The file loaded is not valid'
    r$tsev <- tsev_null
  } else r$message <- 'File load success'
  
  r # return value
}

print.tse_visu <- function(tsev, ...) {
  if(is_null_tsev(tsev)) {
    cat('NULL TSEV object')
  } else { unclass(tsev) %>% print }
}

add_algo_meta <- function(tsev, meta) {
  if(length(tsev$algo_meta) > 0)
    tsev$algo_meta[[length(tsev$algo_meta)+1]] <- meta
  else tsev$algo_meta <- list(meta)
  
  tsev
}