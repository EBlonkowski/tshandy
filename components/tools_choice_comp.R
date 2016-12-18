
output$tools_choice_comp <- renderUI({
  div(
  selectInput("current_tool", label = "Visualization tools:", 
              choices = list("Time-serie" = "time_serie_comp",
                             'Profiles' = 'profile_comp',
                             'Versus plot' = 'versus_comp',
                             'Categorical' = 'categorical_comp',
                             'App state info' = 'app_state_comp'),
                             #'Auto-correlation' = 'acf_comp',
                             #'Cross-correlation',
                             #'Versus plot',
                             #'Histogram',
                             #'Power spectrum',
                             #'3D plot',
                             #'peak plot',
              selected = 1))
})