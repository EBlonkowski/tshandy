

output$cor_sel_sidebar_comp <- renderUI({
column(width = 4,
       ##########################################################
       # date/time selection
       div(class = 'panel panel-primary',
           div(class = 'panel-heading',
               fluidRow(
                 column(width = 3, h3(span(class="glyphicon glyphicon-calendar", `aria-hidden` = "true"))),
                 column(width = 9, h3(class = 'text-left', 'Time selection'))
               )
           ),
           
           div(class = 'panel-body',
               uiOutput('date_range'),
               
               checkboxGroupInput("weekdays", 
                                  label = "Days of the week", 
                                  choices = list("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3,
                                                 "Thursday" = 4, "Friday" = 5, "Saturday" = 6,
                                                 "Sunday" = 0),
                                  selected = 0:6),
               
               checkboxGroupInput("public_holidays", 
                                  label = "Public holidays", 
                                  choices = list("Normal day" = FALSE, "Public holiday" = TRUE),
                                  selected = c(TRUE, FALSE)),
               
               sliderInput("day_period", "Time of the day:",
                           min = 0, max = 47, value = c(0, 47)),
               
               checkboxInput("negate", label = "Negate time of the day", value = FALSE)
           )
       ),
       
       ##########################################################
       # Components decomposition
       div(class = 'panel panel-green',
           div(class = 'panel-heading',
               fluidRow(
                 column(width = 3, h3(span(class="glyphicon glyphicon-plus-sign", `aria-hidden` = "true"))),
                 column(width = 9, h3(class = 'text-left', 'Components'))
               )
           ),
           div(class = 'panel-body',
               checkboxInput("component_trend", label = "Trend", value = TRUE),
               checkboxInput("component_seasonality", label = "Seasonality", value = TRUE),
               checkboxInput("component_daily", label = "Daily component", value = TRUE),
               checkboxInput("component_ar", label = "Autoreactive component", value = TRUE),
               checkboxInput("component_residual", label = "Residual", value = TRUE)
           )
       ),
       
       
      ##########################################################
      # Show multiple time series
      div(class = 'panel panel-red',
        div(class = 'panel-heading',
          fluidRow(
            column(width = 3, h3(span(class="glyphicon glyphicon-ok-sign", `aria-hidden` = "true"))),
            column(width = 9, h3(class = 'text-left', 'Data'))
          )
        ),
        div(class = 'panel-body',
          view_col_selection(app_state$tsev$tse)
      )
    )
)
})
