library(shiny)
# options(shiny.error = browser)

#=== Code here will be loaded once when the app is launched ===
funConfig <- read.csv("input_control.csv", header=TRUE, sep = ",", quote = '"')

shinyServer(function(input, output, session) {
  #=== Code here will be loaded when a new user connects to the app ===
  
  # Read and display the data
  table <- as.data.frame(read.csv("rock.csv", header = TRUE, sep = ",", quote = '"'))
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
      checkboxGroupInput("terms", "Independent Variables", choices = allColNames[allColNames != input$response])
      
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
  
  
  updateUI <- function(needResponse) {
    # Selection of Function will direclty affect the Select Input UI
    if (needResponse) {
      output$selectResponse <- renderUI({
        selectInput("response", 'Dependent Variable', choices = names(table))
      })
      
      # Update check boxes for selecting independent variables
      allColNames <- names(table)
      output$inputUI <- renderUI({
        checkboxGroupInput("terms", "Independent Variables", 
                           choices = allColNames[allColNames != input$response])
      })
      
    } else {
      output$selectResponse <- renderUI({ return(NULL) })
      
      # If the selected function requires an numeric argument (currently we only support numeric input)
      reqArgName <- funConfig[index, "requiredArg"]
      if (reqArgName != "") {
        output$inputUI <- renderUI({
          numericInput("reqArg", reqArgName, value = 3)
        })
      } else {
        output$inputUI <- renderUI({
          return(NULL)
        })
      }
    }
  }

  
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
  
  # Run the analysis and get the result object
  # lm(formula, data)
  # glm(formula, family = gaussian, data)
  # chisq.test(x)
  # kmeans(x, centers) - let's forget about this for now
  
  hasPlot <- TRUE
  hasSummary <- TRUE
  
  getResult <- reactive({
    if (funConfig[getFunIndex(), "needResponse"] == 1) {
      if (is.null(getFormula()))
        return(NULL)
      
      do.call(getFunName(), list(getFormula(), data = table))
    } 
    else {
      # If the selected function requires an numeric argument (currently we only support numeric input)
      reqArgName <- funConfig[getFunIndex(), "requiredArg"]
      if (reqArgName != "") {
        do.call(getFunName(), list(x=table, centers=input$reqArg))
        # hasSummary <<- FALSE
      } else {
        do.call(getFunName(), list(table))
        hasPlot <<- FALSE
        hasSummary <<- FALSE
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