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
library(RColorBrewer)

# TEST
# 1. models with CO2 energy emissions
cntr <- unique(all_indicators$region)
cntr_selec <- c("AUS", "BRA", "CAN", "CHN", "EU", "IDN", "IND", "JPN", "MEX", "ROK", "RUS", "SAF","TUR", "USA", "World")
d <- filter(all_indicators, variable=="Emissions|CO2|Energy", region %in% cntr_selec, period==2030)
d_selec <- filter(d, region%in%c("CAN", "IDN"), Category=="National policies") %>%
            spread(key=model, value=value)
# 2. national models with Kyoto Gases
d_nat <- filter(all_indicators, variable=="Emissions|Kyoto Gases", region %in% cntr_selec, period==2030) %>%
         spread(key=model, value=value)


# Figure 1 (facet)
d1 <- gather(d_selec_Kyoto_glob_stat, 7:ncol(d_selec_Kyoto_stat), key="year", value="value") %>% 
      spread(key=statistic, value=value)
d1$year <- as.integer(d1$year)

ggplot(data=d1) +
   geom_ribbon(aes(x=year, ymin=tenp, ymax=ninetyp, fill=scenario), alpha=.15) +
   geom_line(aes(x=year, y=median, colour=scenario)) +
   facet_wrap(~region, scales = "free_y") +
   #ylim(0,NA) +
   scale_fill_brewer(palette="Set2") +
   scale_colour_brewer(palette="Set1") +
   guides(colour = guide_legend(reverse = TRUE)) +
   guides(fill = guide_legend(reverse = TRUE)) +
   theme_bw()

# Figure 11
d11 <- filter(d_cd_links_peak_zero_inclhist_stat, statistic=="median") %>%
       ungroup(d_cd_links_peak_zero_inclhist_stat) %>% 
       mutate(gas=ifelse(variable%in%c('Peak year|Kyoto Gases', 'Zero Emissions Year|Kyoto Gases'), "Kyoto", "CO2")) %>%
       mutate(var=ifelse(variable%in%c('Peak year|Kyoto Gases', 'Peak year|CO2'), "Peak", "Zero")) %>%
       select(-variable) %>%
       rename(variable=var) %>%
       spread(key="variable", value=value)
ggplot(data=d11) +
       geom_point(aes(x=Peak, y=Zero, colour=region, shape=scenario), size=4) +
       geom_text(aes(x=Peak, y=Zero, label=region), nudge_y=2, size=2) +
       #scale_colour_brewer(palette="Set2") +
       facet_wrap(~gas) +
        theme_bw()
