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

options(scipen=999)
options(digits = 2)

regs_paper<- c("CHN", "EU",  "IND", "USA", "World")

all_paper_results <- filter(all_cd_links, year>=2010, year<=2050, region%in%regs_paper) 

# emissions_gap
emissions_gap <- filter(str(all_paper_results, period==2030, 
                        variable%in%c("Emissions|Kyoto Gases"),
                        Category%in%c('NDC', 'Carbon budget 1000','Carbon budget 400')) %>% 
                 spread(Category, value=value) %>%
                 mutate(gap_2C =`NDC` - `Carbon budget 1000`) %>%
                 mutate(gap_1.5C =`NDC` - `Carbon budget 400`) %>%
                 gather(`NDC`, `Carbon budget 1000`, `Carbon budget 400`, `gap_2C`, `gap_1.5C`, key="Category", value=value)
write.table(emissions_gap, "Indicators/data/emissions_gap.csv", row.names=F, sep=";")
emissions_gap_stat <- filter(all_paper_results, year==2030, 
                             variable%in%c("Emissions|Kyoto Gases"),
                             Category%in%c('NDC', 'Carbon budget 1000','Carbon budget 400')) %>% 
                      spread(Category, value=value) %>%
                      mutate(gap_2C =`NDC` - `Carbon budget 1000`) %>%
                      mutate(gap_1.5C =`NDC` - `Carbon budget 400`) %>%
                      gather(`NDC`, `Carbon budget 1000`, `Carbon budget 400`, `gap_2C`, `gap_1.5C`, key="Category", value=value) %>%
                      group_by(Category, region, period, unit, variable) %>%
                      summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                               mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T)) %>%
                      arrange(variable, Category)
write.table(emissions_gap, "Indicators/data/emissions_gap_stat.csv", row.names=F, sep=";")

# emissions_breadkown
emissions_breakdown <- filter(all_paper_results, period%in%c(2015, 2030, 2050), 
                       variable%in%c("Emissions|CO2|AFOLU", "Emissions", "Emissions|CO2",
                                      "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases"),
                        Category%in%c('NDC', 'Carbon budget 1000','Carbon budget 400'))
emissions_breakdown <- as.data.table(emissions_breakdown)
emissions_breakdown[variable=="Emissions|CH4"]$value<-emissions_breakdown[variable=="Emissions|CH4"]$value*25
emissions_breakdown[variable=="Emissions|N2O"]$value<-emissions_breakdown[variable=="Emissions|N2O"]$value*(298/1000)
emissions_breakdown <- as.data.frame(emissions_breakdown)
emissions_breakdown$unit <- "Mt CO2-equiv/yr"
emissions_breakdown <- spread(emissions_breakdown, variable, value=value) %>%
                       mutate(`Emissions|CO2|Excl. AFOLU`=`Emissions|CO2`-`Emissions|CO2|AFOLU`) %>%
                       gather(`Emissions|CO2`, `Emissions|CO2|AFOLU`, `Emissions|CO2|Excl. AFOLU`,
                              `Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`, key="variable", value=value)
write.table(emissions_breakdown, "Indicators/data/emissions_breakdown.csv", row.names=F, sep=";")

emissions_breakdown_stat <- group_by(emissions_breakdown, Category, region, period, unit, variable) %>%
                            summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                                      mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T)) %>%
                            arrange(variable, Category)
write.table(emissions_breakdown_stat, "Indicators/data/emissions_breakdown_stat.csv", row.names=F, sep=";")

