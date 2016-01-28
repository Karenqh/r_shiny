library(shiny)
# options(shiny.error = browser)

#=== Code here will be loaded once when the app is launched ===
funConfig <- read.csv("input_control.csv", header=TRUE, sep = ",", quote = '"')

shinyServer(function(input, output, session) {
  #=== Code here will be loaded when a new user connects to the app ===
  
  # Read and display the data
  table <- read.csv("rock.csv", header = TRUE, sep = ",", quote = '"')
  output$table <- renderTable({
    # Display the table
    table
  })
  
  # Populate single-selection list based on the configuration file
  # The Columns of the data frame is of class 'factor' rather than 'charactor'
  updateSelectInput(session, "input_funName", choices = as.character(funConfig$displayName))
  # browser()
  
  # ============= Debug ===============
  output$debug <- renderText({
    index <- which(funConfig$displayName == input$input_funName)
    
    paste( "We are here!!",
          funConfig[index, "needResponse"] == 1,
          funConfig[index, "funName"])
  })
  #====================================
  
  # Dynamic input UI based on function selected by user  
  getFunIndex <- reactive({
    # Ideally there will always be an item selected
    # if (is.null(input$input_funName))
    #   return(0)
    
    which(funConfig$displayName == input$input_funName)
  })
  
  isResposeNeeded <- reactive({
    funConfig[getFunIndex(), "needResponse"] == 1
  })
  
  isSingleTerm <- reactive({
    funConfig[getFunIndex(), "singleTerm"] == 1
  })
  
  output$selectResponse <- renderUI({
    if (isResposeNeeded()) {
      selectInput("response", 'Dependent Variable', choices = names(table))
    } else {
      selectInput("response", 'Dependent Variable', choices = "")
    }
  })
  
  output$inputUI <- renderUI({
    if (isResposeNeeded()) {
      # Update check boxes for selecting independent variables
      allColNames <- names(table)
      
      # If the function expects a singel term, use radio buttons rather than check boxes
      if (isSingleTerm()) {
        radioButtons("terms", "Independent Variable", choices = allColNames[allColNames != input$response])        
      } else {
        checkboxGroupInput("terms", "Independent Variables", choices = allColNames[allColNames != input$response])
      }
      
    } else {
      # If the selected function requires an numeric argument (currently we only support numeric input)
      reqArgName <- funConfig[getFunIndex(), "requiredArg"]
      if (reqArgName != "") {
        numericInput("reqArg", reqArgName, value = 3)            
      } else {
        return(NULL)
      }
    }
  })
  
  
  # Get the actual function name that R recognizes
  getFunName <- reactive({
    as.character(funConfig[getFunIndex(), "funName"])
  })
  
  # Compose the formula for regression analysis
  getFormula <- reactive({
    if (length(input$terms) == 0) 
      return(NULL)
    
    as.formula(paste(input$response, " ~ ", paste(input$terms, collapse = " + ")))
  })
  
  # Get the fitted models for ANOVA
  getFittedModels <- reactive({
    if (length(input$terms) == 0) 
      return(NULL)
    
    modelList <- list()
    i <- 1
    for (term in input$terms) {
      # browser()
      modelList[[i]] <- lm(as.formula(paste(input$response, " ~ ", term)), table)
      i <- i + 1
    }
    return(modelList)
  })
  
  
  # Run the analysis and get the result object
  # lm(formula, data)
  # glm(formula, family = gaussian, data)
  # anova(Object...) where Object is one or more fitted result objects
  # chisq.test(x) where x should be a 2-d matrix/table
  # kmeans(x, centers) - let's forget about this for now
  
  hasPlot <- TRUE
  hasSummary <- TRUE
  
  
  getFirstArg <- function(funName) {
    tryCatch({
      formalArgs(funName)[1]
    }, error = function(e) {
      return(NULL)
    } )
  }
  
  getResult <- reactive({
    # Reset session flag
    hasPlot <<- TRUE
    hasSummary <<- TRUE
    
    if (funConfig[getFunIndex(), "needResponse"] == 1) {
      if (is.null(getFormula()))
        return(NULL)
      
      funName <- getFunName();
      
      # Need to update the flags for plot and summary
      
      switch (getFirstArg(funName),
        "formula" = do.call(funName, list(getFormula(), data = table)),
        "object" = { # do lm for each independent variable
          hasPlot <<- FALSE
          hasSummary <<- FALSE
          
          return(do.call(funName, getFittedModels()))
        }, 
        "x" = {
          hasPlot <<- FALSE
          hasSummary <<- FALSE
          return(do.call(funName, list(x = table[, c(input$response, input$terms)])))
        } # pass in the 2-D matrix
      )
    } 
    else {
      # If the selected function requires an numeric argument (currently we only support numeric input)
      reqArgName <- funConfig[getFunIndex(), "requiredArg"]
      if (reqArgName != "") {
        do.call(getFunName(), list(x=table, centers=input$reqArg))
        # hasSummary <<- FALSE
      } else {
        hasPlot <<- FALSE
        hasSummary <<- FALSE
        do.call(getFunName(), list(table))
      }
    }
  })
  
  # Display the plot
  output$plot <- renderPlot({
    if (is.null(getResult()) || !hasPlot)
      return(NULL)
    else
      with(table, plot(getResult()))
  })
  
    
  output$summary <- renderPrint({
    if (is.null(getResult())) {
      return(NULL)
    }

    if (hasSummary) {
      summary(getResult())
    } else {
      getResult()
    }
  })
})