
output$load_save_comp <- renderUI({
  
  tagList(
    selectInput('SL_load_choice', 'Choose loading method',
                list('From previously saved', 'From CSV', 'From SQL')),
    # the tools row at the top containing the loading, save
    div(class = 'panel panel-default',
        div(class = 'panel-body',
            fluidRow(
              column(width = 4, fileInput('SL_input_file', 'Input time-serie', 
                                          multiple = FALSE, accept = c('.RData', '.tsev'))), # data loading
              column(width = 4, textInput("SL_out_file", "Output file path", default_out_tsev)),
              column(width = 4, actionButton("SL_save", "Save", icon = icon('save', lib = 'glyphicon'))) # saving results
            ) # fluidrow
        ) # panel-body
    ) # parameters panel
  ) # taglist
})