library(shiny)

shinyServer(function(input, output, session) {
  
  # If the input$file1 doesn't change, the saved tabe should be returned
  getTable <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    table <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                      quote=input$quote)
    
    # Update the radio buttons
    cols <- names(table)
    updateRadioButtons(session, "colName",
                       choices = cols,
                       selected = cols[1])
    
    return(table)
  })
  
  # If the colname doesn't change, the saved columns should be returned
  getData <- reactive({
    table <- getTable()
    if (is.null(table))
      return(NULL)
    
    # Use the column name selected by user via radion buttons
    colName <- input$colName
    
    table[[colName]]  # table$colname will not work here since colName is a string
  })
  
  
  
  output$table <- renderTable({
    # Display the table
    getTable()
  })
  
  output$plot <- renderPlot({
    # Barplot
    if (is.null(getData()))
      return(NULL)
    
    barplot(getData())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('my_download', sep = '.', 
            switch (input$format,
              JPEG = 'jpg', PNG = 'png', PDF = 'pdf'
            ))
    },
    content = function(file) {
      # ------ R markdown (require installation of LaTex. Will do later) -----
      # src <- normalizePath('report.Rmd')
      # # temporarily switch to the temp dir, in case you do not have write
      # # permission to the current working directory
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd')
      # 
      # library(rmarkdown)
      # out <- render('report.Rmd', switch(
      #   input$format,
      #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
      # ))
      # file.rename(out, file)
      
      # ----- Image ----------
      switch (input$format,
        JPEG = jpeg(file),
        PNG = png(file),
        PDF = pdf(file)
      )
      barplot(getData())
      dev.off()
    }
  )
  
  output$summary <- renderPrint({
    summary(getTable())
  })
  
})