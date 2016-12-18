
output$correction_stack_comp <- renderUI({
  div(
  h3('Algorithmic stack'),
  checkboxInput('cor_stack_use_trend', 'Use trend', value = default_trend),
  fluidRow(
    column(width = 6, checkboxInput('cor_stack_use_seasonal', 'Use seasonal', 
                                    value = default_seasonal)),
    column(width = 6, 
           numericInput('cor_n_harmonics', 'Number of harmonics', 
                        default_harmonics, min = 1, max = 20, step = 1))
  ),
  fluidRow(
    column(width = 6, checkboxInput('cor_stack_use_daily', 'Use daily', 
                                    value = default_daily)),
    column(width = 6, 
           numericInput('cor_n_clusters', 'Number of clusters', 
                        default_clusters, min = 2, max = 20, step = 1))
  ),
  checkboxInput('cor_stack_use_AR', 'Use autoregressive', value = default_ar)
  )
})