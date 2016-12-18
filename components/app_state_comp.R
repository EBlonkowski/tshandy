output$app_state_comp <- renderUI({
  tagList(
    h3('Time-series sample'),
    verbatimTextOutput('tse_sample_print'),
    h3('Metrics data structure'),
    verbatimTextOutput('meta_str_print'),
    h3('Corrected, cut time-series sample'),
    verbatimTextOutput('cor_cut_sample_print')
  )
})

output$tse_sample_print <- renderPrint({
  app_state$tsev$tse %>% print
})

output$meta_str_print <- renderPrint({
  app_state$tsev$algo_meta %>% str %>% print
})

output$cor_cut_sample_print <- renderPrint({
  cor_cut_tse() %>% print
})

