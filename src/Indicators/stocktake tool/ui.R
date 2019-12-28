library(shiny)

# See https://psyteachr.github.io/shiny-tutorials/01-first-app.html

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Global GHG emissions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
      radioButtons("Country", "Country",
                   choices = regions_indicators,
                   selected = "World"),
      checkboxGroupInput("Scenarios", "Scenarios", 
                         choices = scens_indicators, 
                         selected = c('NDC', 'National policies', '2C_66'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)