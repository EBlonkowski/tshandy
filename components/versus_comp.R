# the versus component allows to plot one variable against the other
# to understand how they are correlated

output$versus_comp <- renderUI({
  plotOutput('SV_versus_plot')
})

output$SV_versus_plot <- renderPlot({
  plot_versus(cor_cut_tse())

})