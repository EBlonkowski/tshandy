#------------------------------------------------------------------------------#
# Edition part of the app

output$algorithms_comp <- renderUI({
  # set the default choice of the algorithm select
  choice <- input$choice_analytics
  if(is.null(choice)) choice <- 'none'

  tagList(
    
#------------------------------------------------------------------------------#
# Choice of algorithm panel
    div(class = 'panel panel-primary',
        div(class = 'panel-heading', 
            fluidRow(column(width = 1, h3(icon('cog', lib = 'glyphicon'))),
                     column(width = 11, h3('Algorithm')))
        ),
        div(class = 'panel-body',
          # select algorithm input
          selectInput("choice_analytics", label = "Algorithm choice", 
            choices = list("None" = 'none', 'Delete columns' = 'del',
                            'Split dataset' = 'split',
                            "Daily profiles" = 'daily_profiles', 
                            'Exponential smoothing' = 'exponential_smoothing',
                            'Shuya SVM' = 'shuya_svm'),
            selected = choice),
          wellPanel(
            # algorithm custom UI
            algos[[choice]]$ui(input, app_state$tsev),
            # run button
            actionButton("SE_run_algo", "Run", icon = icon('play', 
                                                           lib = 'glyphicon'))
          ) # panel body
        ) # algorithm panel
    ),

#------------------------------------------------------------------------------#
# History panel
    div(class = 'panel panel-green',
        div(class = 'panel-heading', 
            fluidRow(column(width = 1, h3(icon('search', lib = 'glyphicon'))),
                     column(width = 11, h3('History')))
        ),
        div(class = 'panel-body',
            # select metrics/metadata
            selectInput('SE_choose_meta', 'Select past algorithm:',
              choices = 
                structure(as.list(seq_along(app_state$tsev$algo_meta)),
                                  names = 
                            name_vec(app_state$tsev$algo_meta)), 
              selected = input$SE_choose_meta),
            {
            # no algorithm selected
            if(is.null(input$SE_choose_meta)) {
              # meta contains the currently selected metadata
              meta <- NULL
              p('No metadata to show')
            }
            else {
              # meta contains the currently selected metadata
              meta <- 
                app_state$tsev$algo_meta[[as.numeric(input$SE_choose_meta)]]
              # show somme basic metadata
              tagList(
                h3(input$SE_choose_meta, '-', name(meta)),
                p('Date: ', meta$timestamp),
                # show metrics custom UI
                meta$custom_ui
              )
            }
            },
            # metrics custom table
            # choice of the table
            {if(!is.null(meta))
              selectInput('SE_meta_seltable', 'Select table:',
                choices = names(meta$show_table))},
            # show the table
            tableOutput('SE_meta_table'),
            # choice of the graph
            {if(!is.null(meta))
              selectInput('SE_meta_selgraph', 'Select graph:',
                          choices = names(meta$show_graph))},
            # show graph
            plotOutput('SE_meta_graph'),
            # metrics custom R output
            verbatimTextOutput('SE_algo_summary')
        )
    )
  )
})

# metrics custom table
output$SE_meta_table <- renderTable({
  if(!is.null(input$SE_meta_seltable))
    app_state$tsev$
      algo_meta[[as.numeric(input$SE_choose_meta)]]$show_table[[
        input$SE_meta_seltable
      ]]
})

# metrics custom graph
output$SE_meta_graph <- renderPlot({
  if(!is.null(input$SE_meta_selgraph))
    app_state$tsev$
      algo_meta[[as.numeric(input$SE_choose_meta)]]$show_graph[[
        input$SE_meta_selgraph
        ]]
})

# metrics custom R output
output$SE_algo_summary <- renderPrint({
  app_state$tsev$algo_meta[[as.numeric(input$SE_choose_meta)]]$show_summary
})