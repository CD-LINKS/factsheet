library("shiny")
library("datasets")
library("ggplot2")
# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  # Compute the forumla text in a reactive expression since it is
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  # Generate a plot of the requested variable against mpg and only
  # include outliers if requested
  # ggplot version
  output$mpgPlot <- renderPlot({
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