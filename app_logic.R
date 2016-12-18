# controller part
# the controller holds the app logic

source(file.path('..', 'core', 'time-serie.R'), chdir = TRUE)
source(file.path('..', 'core', 'models.R'), chdir = TRUE)

#------------------------------------------------------
# tse_visu class

tse_visu <- function(tse, algo_meta) {
  stopifnot(is.tse(tse))
  
  structure(list(tse = tse, algo_meta = algo_meta),
            class = c('tse_visu'),
            is_null = FALSE)
}

tsev_null <- structure(list(tse = NULL, algo_meta = NULL),
                      class = c('tse_visu'),
                      is_null = TRUE)

is.tse_visu <- function(tsev) {
  if(!inherits(tsev, 'tse_visu')) return(FALSE)
  
  if(is_null_tsev(tsev)) return(TRUE)
  
  is.tse(tsev$tse)
}

is_null_tsev <- function(tsev) {
  attributes(tsev)$is_null
}

#------------------------------------------------------
# Algo meta class

algo_meta <- function(name, custom_ui, show_table, show_summary, timestamp = Sys.time()) {
  structure(list(name = name, 
                 timestamp = timestamp, 
                 custom_ui = custom_ui,
                 show_table = show_table,
                 show_summary = show_summary), 
            class = 'algo_meta')
}

name <- function(ameta) { ameta$name }

name_vec <- function(algo_meta) { sapply(algo_meta, FUN = name) }

is.algo_meta <- function(obj) {
  if(!inherits(obj, 'algo_meta')) return(FALSE)
  if(is.null(obj$name)) return(FALSE)
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



##########################################################
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
  perf <- performance_indicators(tsev$tse, colx, colout, coltrain, t1[[3]], t2[[3]], dim(tsev$tse)[1])
  
  # update dataset with algorithm metadata
  add_algo_meta(tsev, 
                algo_meta(name, NULL, t(structure(c(perf, recursive = TRUE), names = names(perf))), NULL))
}

# function to compute metrics about a forecasting algorithm
store_forecasting <- function(tsev, algo, name, colx, colout, coltrain) {
  
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
  
  # compute perf indicators
  perf <- performance_indicators(tsev$tse, colx, colout, coltrain, t1[[3]], t2[[3]], dim(tsev$tse)[1])
  
  # update dataset with algorithm metadata
  add_algo_meta(tsev, 
                algo_meta(name, NULL, t(structure(c(perf, recursive = TRUE), names = names(perf))), NULL))
}

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