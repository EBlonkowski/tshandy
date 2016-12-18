
##########################################################
# Load/import stage
# move to app logic
init_app <- function() {
  message('[init_app] hi')
  r <- restore(default_tsev)
  message(r$message)
  app_state$tsev <- r$tsev
}

load_file <- observeEvent(input$SL_input_file, {
  message('[load_file] hi')
  r <- restore(input$SL_input_file$datapath)
  message(paste('[load_file]', r$message))
  app_state$tsev <- r$tsev
})

save_file <- observeEvent(input$SL_save, {
  message('[save_file] hi')
  tsev <- app_state$tsev
  save(tsev, file = input$SL_out_file)
})

app_state <- reactiveValues()

##########################################################
# Edit stage

run_algo <- observeEvent(input$SE_run_algo, {
  message('[run_algo] hi')
  choice <- input$choice_analytics
  if(is.null(choice)) return(NULL)
  
  app_state$tsev <- algos[[choice]]$build(input, app_state$tsev)
})
##########################################################
# Correct/select stage stage
# move to app logic
cor_cut_ctl <- function(tsev, sel_cols) {
  # app state is corrupted
  if(!is.tse_visu(tsev)) stop('[cor_cut_ctl] App state is corrupted')
  # app state is null
  if(is_null_tsev(tsev)) return(NULL)
  # app state is valid and non-null
  # apply corrections and selections
  subcol(tsev$tse, sel_cols)
}

cor_cut_tse <- reactive({ 
  message('[cor_cut_tse] Hi!')
  cor_cut_ctl(app_state$tsev, input$SC_choose_cols)
})

##########################################################
# Visualization stage
output$SV_is_valid <- reactive({
  message('[SV_is_valid] Hi!')
  is.tse(cor_cut_tse()) & # check if valid time-series
    length(get_value_cols(cor_cut_tse())) > 0 # value column
})
# make sure SV_is_valid is computed -> since it is not printed
# it will not be computed unless we include this line
outputOptions(output, "SV_is_valid", suspendWhenHidden=FALSE) 

