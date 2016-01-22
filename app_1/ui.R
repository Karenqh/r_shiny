library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      # TODO - show radio buttons based on the headers of the input csv file?
      # It seems we has to first import the data and save as a data frame??
      tags$hr(),
      radioButtons("colName", "Column Selected", c("Column 1" = " ")),
      
      # TODO - download the result as PDF or image?
      tags$hr(),
      # radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
      radioButtons('format', 'Format', c('JPEG', 'PNG', 'PDF'), inline = TRUE),
      downloadButton('downloadData', 'Download')
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