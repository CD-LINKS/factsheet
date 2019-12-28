# Data processing ---------------------------------------------------------
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

data_D2 <- all_paper
variables_D2 <- unique(all_paper$variable)
data_D2$variable <- factor(data_D2$variable, levels=variables_D2)
data_D2_GDP <- filter(data_D2, grepl("GDP", variable), Scope=="global")

regs_D2 <- c("BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World")

data_D2_GDP <- filter(data_D2_GDP, variable=="GDP|PPP", period>=2015, period<=2020, Category=="National policies", region %in% regs_D2)
#data_D2_GDP_tmp <- filter(data_D2, model %in% c("DNE21+ V.14", "GEM-E3"), variable=="GDP|MER", period>=2015, period<=2020, Category=="National policies", region %in% regs_D2)
#data_D2_GDP <- rbind(data_D2_GDP, data_D2_GDP_tmp)
  
ggplot(data=data_D2_GDP) +
    geom_line(aes(x=period, y=value, colour=model)) +
    facet_wrap(~region, scales="free_y") +
    ylim(0, NA) +
    ylab("billion US$2010/yr") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

