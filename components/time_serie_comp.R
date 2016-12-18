
output$time_serie_comp <- renderUI({
  wellPanel(
    plotOutput("time_plot"),
    sliderInput("SV_interval_size",
                "Size of interval:",
                min = 0,
                max = 100,
                value = 100),
    sliderInput("SV_position",
                "Start time:",
                min = 0,
                max = 100,
                value = 0)
  )
})

output$time_plot <- renderPlot({
  tse_plot(cor_cut_tse(), 
           input$SV_position/100, 
           min(1, (input$SV_position + input$SV_interval_size)/100))
})