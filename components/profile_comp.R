# profile comp

output$profile_comp <- renderUI({
  tagList(
    plotOutput('SV_profile_plot')
  )
})

output$SV_profile_plot <- renderPlot({
  plot_profiles(cor_cut_tse())
})