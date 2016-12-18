#
# This application allows to visuzalize the singapore energy consumption data.
# The sg consumption data is the electrical consumptionof the whole of Singapore.
# It is half-hourly.
#
#
#
##########################################################

# Question: quand utiliser des wellPanel ou pas?
# 1ere solution: toujours utiliser des wellPanel: chaque element est dans un wellPanel, les controles
# qui sont relie uniquement a un graphe sont place dans le meme wellPanel de ce graphe.
# Avantage -> L'interface est unifie par l'utilisation de plein de panel en mode windows 10
# Inconvenient -> Beaucoup de gris qui fait mal aux yeux
# solution -> changer la couleur?
# 2eme solution: utiliser seulement un wellPanel quand il y a des controles a grouper, ou des graphes+controles
# a grouper. Avantage -> Le panel a un interet il permet de visuellement voir quel parametre agit sur quel graphe.
# 3eme solution -> utiliser les wellPanels seulement pour grouper les inputs. Utiliser les titles headers pour
# faire des parties qui groupent les outputs et leur inputs ensemble

library(shiny)
library(shinyRGL)



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  includeCSS("colorful-panels.css"),
  
  tags$head(
    tags$style(HTML("
                    .label-bg {
                    font-size: 100%
                    }
                    
                    "))
  ),
  
  navbarPage("Time-series handiman", selected = 'Edit',
     
##########################################################
# Load import panel        
    tabPanel('Load/import',
      uiOutput('load_save_comp')
    ),

##########################################################
# Edition panel
    tabPanel('Edit',
      uiOutput('algorithms_comp'),
      uiOutput('history_comp')
  ),

##########################################################
# Tools panel
    tabPanel('Visualization',
      # Sidebar for selection and main panel for display
      sidebarLayout(
        # sidebar
        uiOutput('cor_sel_sidebar_comp'),
        
        # main
        mainPanel(
          tabsetPanel(
            tabPanel('Tools',
              div(
              # select the tool you want to use
              uiOutput('tools_choice_comp'),
              # Panel that will show all the tools
              conditionalPanel(
                condition = 'output.SV_is_valid',
                
                  # time serie plot
                  conditionalPanel(
                    condition = "input.current_tool == 'time_serie_comp'",
                    uiOutput('time_serie_comp')
                  ), # END time serie tool panel
                
                  # profile plot
                  conditionalPanel(
                    condition = "input.current_tool == 'profile_comp'",
                    uiOutput('profile_comp')
                  ), # END profile tool panel
                  
                  # versus plot
                  conditionalPanel(
                    condition = "input.current_tool == 'versus_comp'",
                    uiOutput('versus_comp')
                  ), # END versus tool panel
                  
                  # categorical tool
                  conditionalPanel(
                    condition = "input.current_tool == 'categorical_comp'",
                    uiOutput('categorical_comp')
                  ), # END versus tool panel
                
                  # app state tool
                  conditionalPanel(
                    condition = "input.current_tool == 'app_state_comp'",
                    uiOutput('app_state_comp')
                  ) # END app state tool tool panel
              )
              ) # all tools panel
            ), # tab tools
            tabPanel('Correction',
              uiOutput('correction_stack_comp')
            )
          )
        ) # main panel
      ) # sidebar layout panel
    ) # tools panel

  ) # navbar page
)) # fluidPage, shinyUI


