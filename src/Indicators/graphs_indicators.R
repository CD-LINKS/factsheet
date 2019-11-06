source("functions/plot_functions_NatCom.R")
source("functions/plot_functions_xcut_NatCom.R")

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
library(grid)
library(scales)
library(RColorBrewer)
library(maptools)

cats_fig8 <- c('No policy', 'National policies', 'NDC', '2C_66', '1.5C_50')
data_figure8_stat <- filter(data_figure8, scenario%in%cats_fig8) %>%
                     group_by(scenario, region, year, variable) %>%
                     summarise(median=median(value), min=quantile(value, 0.1), max=quantile(value, 0.9))
d <- filter(data_figure8_stat, region=="World", variable=="Emissions|Sulfur")
p8_1 <- ggplot(data=d) + geom_ribbon(aes(x=year,ymin=min,ymax=max, fill=scenario), alpha=.15)
plot(p8_1)
p8_2 <- ggplot(data=data_figure8_stat) + 
      geom_ribbon(aes(x=year,ymin=min,ymax=max, fill=scenario), alpha=.15) + 
      facet_wrap(region~variable, scales = "free_y", ncol=3)
plot(p8_2)

d9_1 <- group_by(filter(data_figure9, year<=2050), scenario, region, year, variable) %>%
        summarise(average=mean(value), median=median(value), perc_10=quantile(value, 0.1), perc_90=quantile(value, 0.9))
d9_2 <- group_by(filter(data_figure9, year<=2050), scenario, region, year, variable) %>%
        summarise(average=mean(growth), median=median(growth), perc_10=quantile(growth, 0.1), perc_90=quantile(growth, 0.9))
d9_3 <- group_by(filter(data_figure9, year<=2050), scenario, region, year, variable) %>%
        summarise(average=mean(perc_growth), median=median(perc_growth), perc_10=quantile(perc_growth, 0.1), perc_90=quantile(perc_growth, 0.9))
p9_1 <- ggplot(data=d9_1) + geom_line(aes(x=year, y=average, colour=scenario)) +
        facet_wrap(~region, scales = "free_y") +
        ylab("natural forest area (mn. ha)") +
        theme_bw()
plot(p9_1)
ggsave(file="graphs/p9_1.jpg", p9_1,width=20,height=12,dpi=200)
p9_2 <- ggplot(data=d9_2) + geom_line(aes(x=year, y=median, colour=scenario)) +
        facet_wrap(~region, scales = "free_y") +
        ylab("5-year area growth (mn. ha)") +
        theme_bw()
plot(p9_2)
ggsave(file="graphs/p9_2.jpg", p9_2,width=20,height=12,dpi=200)
p9_3 <- ggplot(data=d9_3) + geom_line(aes(x=year, y=median, colour=scenario)) +
        facet_wrap(~region, scales = "free_y") +
        ylab("5-year area growth (%)") +
        theme_bw()
plot(p9_3)
ggsave(file="graphs/p9_3.jpg", p9_3,width=20,height=12,dpi=200)

# check low carbon share
#V4
NPi_elec_V4 <- filter(NPi_V4_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="National_policies_V4") %>% mutate(sector="Electricity")
NPi2020_1000_elec_V4 <- filter(NPi2020_1000_V4_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66_V4") %>% mutate(sector="Electricity")
# V5
NPi_elec_V5 <- filter(NPi_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="National_policies_V5") %>% mutate(sector="Electricity")
NPi2020_1000_elec_V5 <- filter(NPi2020_1000_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66_V5") %>% mutate(sector="Electricity")

d <- rbind(NPi_elec_V4, NPi2020_1000_elec_V4) %>% rbind(NPi_elec_V5) %>% rbind(NPi2020_1000_elec_V5)
fig <- ggplot(data=filter(d, region=="World")) + 
       geom_line(aes(x=year, y=value, colour=scenario)) +
       theme_bw()
plot(fig)