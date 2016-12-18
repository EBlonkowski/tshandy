# categorical_comp
# component to analyze distribution of a categorical time-serie

output$categorical_comp <- renderUI({
  tagList(
    plotOutput('SV_cat_hist'),
    selectInput('SV_select_level', 'Select level', choice = list('TRUE' = TRUE, 'FALSE' = FALSE)),
    plotOutput('SV_width_plot'),
    tableOutput('SV_width_table')
  )
})

output$SV_cat_hist <- renderPlot({
  cat_barplot(app_state$tsev$tse$available %>% as.factor)
})

output$SV_width_plot <- renderPlot({
  lspans <- cat_level_span(app_state$tsev$tse$available %>% as.factor, input$SV_select_level)
  plot(lspans$width, lspans$total)
})

output$SV_width_table <- renderTable({
  cat_level_span(app_state$tsev$tse$available %>% as.factor, input$SV_select_level)
})