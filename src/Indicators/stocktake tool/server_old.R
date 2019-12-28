library(shiny)

library(datasets)

library(ggplot2) # load ggplot



# Define server logic required to plot various variables against mpg

shinyServer(function(input, output) {
  
  
  
  # Compute the forumla text in a reactive function since it is 
  
  # shared by the output$caption and output$mpgPlot functions
  
  formulaText <- reactive(function() {
    
    paste("mpg ~", input$variable)
    
  })
  
  
  
  # Return the formula text for printing as a caption
  
  output$caption <- renderText(function() {
    
    formulaText()
    
  })
  
  
  
  # Generate a plot of the requested variable against mpg and only 
  
  # include outliers if requested
  
  # ggplot version
  
  
  
  output$mpgPlot <- renderPlot(function() {
    
    # check for the input variable
    
    if (input$variable == "am") {
      
      # am
      
      mpgData <- data.frame(mpg = mtcars$mpg, var = factor(mtcars[[input$variable]], labels = c("Automatic", "Manual")))
      
    }
    
    else {
      
      # cyl and gear
      
      mpgData <- data.frame(mpg = mtcars$mpg, var = factor(mtcars[[input$variable]]))
      
    }
    
    
    
    p <- ggplot(mpgData, aes(var, mpg)) + 
      
      geom_boxplot(outlier.size = ifelse(input$outliers, 2, NA)) + 
      
      xlab(input$variable)
    
    print(p)
    
  })
  
  
  
})