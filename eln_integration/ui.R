library(shiny)

shinyUI(fluidPage(
  titlePanel("ELN - R Integration"),
  sidebarLayout(
    sidebarPanel(
      selectInput('input_funName', 'Please select a function to execute:', 
                  c("Linear Regression", "Logistic Regression", "ANOVA", "Chi-Square")),
      tags$hr(),
      
      # Depends on the function selected, a response variable might be required
      # If response ~ term: single selection list + Radio Button/Check Boxes
      # If single input: empty selection list + Text Input with dynamic label
      wellPanel( 
        # conditionalPanel(
        #   'reponseRequired === 1',
        #    selectInput('response', 'Dependent Variable', choices = "")
        # ),
        
        uiOutput("selectResponse"),
        uiOutput("inputUI")
        # We could also use ConditionalPanel
      ),
      tags$hr(),

      h4("Debug Output"),
      textOutput("debug")
            
      # # Download      
      # radioButtons('format', 'Format', c('JPEG', 'PNG', 'PDF'), inline = TRUE),
      # downloadButton('downloadData', 'Download')
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput('table')),
                  tabPanel("Plot", plotOutput('plot')),
                  tabPanel("Summary", verbatimTextOutput('summary'))
                  )
    )
  )
))