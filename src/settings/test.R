library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob
library(xtable)
library(grid)
library(scales)
library(readxl)

cntr <- unique(all_indicators$region)
cntr_selec <- c("AUS", "BRA", "CAN", "CHN", "EU", "IDN", "IND", "JPN", "MEX", "ROK", "RUS", "SAF","TUR", "USA", "World")
d <- filter(all_indicators, variable=="Emissions|CO2|Energy", region %in% cntr_selec, period==2030)
d_selec <- filter(d, region%in%c("CAN", "IDN"), Category=="National policies") %>%
            spread(key=model, value=value)
