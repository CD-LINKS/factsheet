library(shiny)

library(datasets)

library(ggplot2) # load ggplot

library(dplyr)      # %>%

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#  output$distPlot <- renderPlot({
#    # generate bins based on input$bins from ui.R
#    x    <- faithful[, 2] 
#    bins <- seq(min(x), max(x), length.out = input$bins + 1)
#    
#    # draw the histogram with the specified number of bins
#    hist(x, breaks = bins, col = 'darkgray', border = 'white')
#  })
#}

#output$distPlot <- renderPlot({
#  # create plot
#  ggplot(faithful, aes(waiting)) +
#    geom_histogram(bins = input$bins,
#                   fill = "steelblue3",
#                   colour = "grey30") +
#    xlab("What are we even plotting here?") +
#    theme_minimal()
#})

  # Figure 1
  d1 <- gather(d_selec_Kyoto_glob_stat, 7:ncol(d_selec_Kyoto_stat), key="year", value="value") %>% 
        spread(key=statistic, value=value)
  d1$year <- as.integer(d1$year)
  
  output$distPlot <- renderPlot({
    #ggplot(data=filter(d1, region=="USA")) +
    ggplot(data=filter(d1, region==input$Country, scenario %in% input$Scenarios)) +
    geom_ribbon(aes(x=year, ymin=tenp, ymax=ninetyp, fill=scenario), alpha=.15) +
    geom_line(aes(x=year, y=median, colour=scenario)) +
    #facet_wrap(~region, scales = "free_y") +
    #ylim(0,NA) +
    scale_fill_brewer(palette="Set2") +
    scale_colour_brewer(palette="Set1") +
    guides(colour = guide_legend(reverse = TRUE)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw()+
    theme_minimal()
  })
  }