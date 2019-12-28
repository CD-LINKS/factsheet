library(dplyr)
library(tidyverse)
library(stringr)
library(XLConnect)
library(data.table)

#setwd("~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/CD-LINKS")
#setwd("C:/Users/mrroelfs/OneDrive/CD-LINKS/6_R/CD-LINKS")
#setwd("~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/CD-LINKS")

# create CD-LINKS model outputs (all)

# THIS does not work yet, run 'main_WP2_3_indicators.R' from factsheet_oct2017, make sure to start in dir 'factsheet/src'
# source('factsheet/src/Functions/ReadFactsheetData.R')
#all_original <- ReadFactSheetData_raw('factsheet')
#all_processed <- ReadFactSheetData('factsheet', all_original)
#currentdir <- getwd()
#factsheetdir <- "C:/Users/mrroelfs/OneDrive/Rwork/factsheet_oct2017/src"
#factsheetdir <- "~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet_oct2017/src"
#factsheetdir <- "../factsheet_oct2017/src"
#setwd(factsheetdir)

# SET working dir to src setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet/src")
source('load_data_NatCom_paper.R')
#setwd(currentdir)
all_cd_links <- all
# changes: only global models, Selection of Categories, change Category->scenario, period->year, remove Baseline
all_cd_links <- filter(all_cd_links, Scope=="global")
all_cd_links <- select(all_cd_links, -Scope)
all_cd_links <- select(all_cd_links, -scenario)
cats <- c("No policy", "National policies", "NDC", "Carbon budget 1600", "Carbon budget 1000 (2030)", "Carbon budget 1000", "Carbon budget 400")
all_cd_links <- filter(all_cd_links, Category %in% cats)
all_cd_links <- rename(all_cd_links, scenario=Category)
colnames(all_cd_links)[colnames(all_cd_links)=="period"] <- "year"
all_cd_links <- select(all_cd_links, -Baseline)

# create factors for easy selection in
all_scens <- unique(all_cd_links$scenario)
all_model <- unique(all_cd_links$model)
all_region <- unique(all_cd_links$region)
all_variable <- unique(all_cd_links$variable)
all_cd_links$scenario <- factor(all_cd_links$scenario, levels=all_scens)
all_cd_links$model <- factor(all_cd_links$model, levels=all_model)
all_cd_links$region <- factor(all_cd_links$region, levels=all_region)
all_cd_links$variable <- factor(all_cd_links$variable, levels=all_variable)

# change scenario names
#all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000 (2030)", "2C_66_2030")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000", "2C_66")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1600", "2C_50")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 400", "1.5C_50")

all_scens <- unique(all_cd_links$scenario)
all_cd_links$scenario <- factor(all_cd_links$scenario, levels=all_scens)

# general settings
models_global <- c("AIM/CGE", "COPPE-COFFEE 1.0", "DNE21+ V.14", "GEM-E3", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "POLES CDL", "REMIND-MAgPIE 1.7-3.0", "WITCH2016")
scens <- c("No policy", "National policies", "NDC", "2C_50", "2C_66", "1.5C_50", "2C_66 (2030)")
regions <- c("BRA", "CHN", "EU", "IND", "JPN", "RUS", "USA", "World")
years <- c('2005', '2010', '2015', '2020', '2025', '2030', '2035', '2040', '2045', '2050', '2055', '2060', '2065', '2070', '2075', '2080', '2085', '2090', '2095', '2100')
stats <- c('mean', 'median', 'min', 'max', 'tenp', 'ninetyp')

GWPCH4 = 25
GWPN2O = 298

# data from IMAGE
Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS", sep="")
Project=paste("CD-LINKS")
TIMERGeneration = 'TIMER_2015'
# Source scripts (after setting working directory)
source('../../TIMER_output/functions/Settings.R')
source('../../TIMER_output/functions/General Functions.R')
source('../../TIMER_output/functions/Import_TIMER_output.R')
source('../../TIMER_output/functions/Process_TIMER_output.R')
# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy', Rundir, Project, TIMERGeneration, Policy=FALSE)
NoPolicy_ind <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=FALSE)
NPi <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi_ind <- ProcessTimerScenario(NPi, Rundir, Project, Policy=FALSE)

# Historical data from PRIMAP
# http://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:2959897
PRIMAP <- read.csv("data/PRIMAP-hist_v1.2_14-Dec-2017.csv", header=TRUE, sep=",")
PRIMAP_CDLINKS <- PRIMAP
regions_CDLINKS_history <- c("BRA", "CHN", "EU28", "IND", "JPN", "RUS", "USA", "EARTH")
years_CDLINKS_history <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015")
colnames(PRIMAP_CDLINKS)[colnames(PRIMAP_CDLINKS)=="country"] <- "region"
PRIMAP_CDLINKS <- filter(PRIMAP_CDLINKS, region %in% regions_CDLINKS_history)
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EARTH","World")
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EU28","EU")
PRIMAP_CDLINKS$scenario=str_replace_all(PRIMAP_CDLINKS$scenario,"HISTORY","History")


# select data for Total Kyoto emissions from 1990 and transfer to MtCO2eq
PRIMAP_selec_Kyoto <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="KYOTOGHGAR4")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_Kyoto <- cbind(PRIMAP_selec_Kyoto[1:5], PRIMAP_selec_Kyoto[, 6:ncol(PRIMAP_selec_Kyoto)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, variable="Emissions|Kyoto Gases")
PRIMAP_selec_Kyoto$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, source="PRIMAP")
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, statistic="value")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, variable, scenario, region, unit, source, statistic, num_range("X", 1990:2015))
colnames(PRIMAP_selec_Kyoto) = gsub("X", "", colnames(PRIMAP_selec_Kyoto))
# determine RoW category
Countries_Kyoto <- filter(PRIMAP_selec_Kyoto, region!='World')
Countries_Kyoto <- gather(Countries_Kyoto, 7:ncol(Countries_Kyoto), key="year", value=value)
RoW_Kyoto <- group_by(Countries_Kyoto, variable, scenario, unit, source, statistic, year) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Kyoto <- mutate(RoW_Kyoto, region="ROW") %>% select(variable, scenario, region, unit, source, statistic, everything())
RoW_Kyoto <- spread(RoW_Kyoto, key='year', value=value)
RoW_Kyoto <- as.data.frame(RoW_Kyoto)
PRIMAP_selec_Kyoto <- rbind(PRIMAP_selec_Kyoto, RoW_Kyoto)
write.table(PRIMAP_selec_Kyoto, file="data/History_Kyoto.csv", sep=";", row.names = FALSE)

# for figure 3)
# Individual gases, split up in 'CO2 excl AFOLU CO2' and "AFOLU CO2"
PRIMAP_selec_CO2 <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2 <- select(PRIMAP_selec_CO2, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_CO2 <- cbind(PRIMAP_selec_CO2[1:5], PRIMAP_selec_CO2[, 6:ncol(PRIMAP_selec_CO2)]/1000)
PRIMAP_selec_CO2$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_CO2 <- data.frame(PRIMAP_selec_CO2)
PRIMAP_selec_CO2 <- mutate(PRIMAP_selec_CO2, variable="Emissions|CO2") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_CO2, file="data/History_CO2.csv", sep=";", row.names = FALSE)

PRIMAP_selec_Agriculture_CO2 <- filter(PRIMAP_CDLINKS, category=="CAT4", entity=="CO2")
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_Agriculture_CO2 <- cbind(PRIMAP_selec_Agriculture_CO2[1:5], PRIMAP_selec_Agriculture_CO2[, 6:ncol(PRIMAP_selec_Agriculture_CO2)]/1000)
PRIMAP_selec_Agriculture_CO2$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_Agriculture_CO2 <- data.frame(PRIMAP_selec_Agriculture_CO2)
PRIMAP_selec_Agriculture_CO2 <- mutate(PRIMAP_selec_Agriculture_CO2, variable="Emissions|CO2|Agriculture") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_Agriculture_CO2, file="data/History_CO2_Agriculture.csv", sep=";", row.names = FALSE)

PRIMAP_selec_LULUCF_CO2 <- filter(PRIMAP_CDLINKS, category=="CAT5", entity=="CO2")
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_LULUCF_CO2 <- cbind(PRIMAP_selec_LULUCF_CO2[1:5], PRIMAP_selec_LULUCF_CO2[, 6:ncol(PRIMAP_selec_LULUCF_CO2)]/1000)
PRIMAP_selec_LULUCF_CO2$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_LULUCF_CO2 <- data.frame(PRIMAP_selec_LULUCF_CO2)
PRIMAP_selec_LULUCF_CO2 <- mutate(PRIMAP_selec_LULUCF_CO2, variable="Emissions|CO2|LULUCF") %>% select(variable, scenario, region, category, entity, unit, everything())

write.table(PRIMAP_selec_LULUCF_CO2, file="data/History_CO2_LULUCF.csv", sep=";", row.names = FALSE)

# create AFOLU CO2' 
PRIMAP_selec_Agriculture_CO2_tmp <- gather(PRIMAP_selec_Agriculture_CO2, 7:ncol(PRIMAP_selec_Agriculture_CO2), key="year", value=value)
PRIMAP_selec_LULUCF_CO2_tmp <- gather(PRIMAP_selec_LULUCF_CO2, 7:ncol(PRIMAP_selec_LULUCF_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2 <- rbind(PRIMAP_selec_Agriculture_CO2_tmp, PRIMAP_selec_LULUCF_CO2_tmp)
PRIMAP_selec_AFOLU_CO2 <- group_by(PRIMAP_selec_AFOLU_CO2, scenario, region, entity, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, category="CAT4+5") %>% select(scenario, region, category, entity, unit, year, value)
PRIMAP_selec_AFOLU_CO2 <- spread(PRIMAP_selec_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_AFOLU_CO2 <- data.frame(PRIMAP_selec_AFOLU_CO2)
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, variable="Emissions|CO2|AFOLU") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_AFOLU_CO2, file="data/History_CO2_AFOLU.csv", sep=";", row.names = FALSE)

# create 'CO2 excl AFOLU CO2'
PRIMAP_selec_CO2_tmp <- gather(PRIMAP_selec_CO2, 7:ncol(PRIMAP_selec_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp <- gather(PRIMAP_selec_AFOLU_CO2, 7:ncol(PRIMAP_selec_AFOLU_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp$value <- -1*PRIMAP_selec_AFOLU_CO2_tmp$value
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_tmp, PRIMAP_selec_AFOLU_CO2_tmp)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- group_by(PRIMAP_selec_CO2_Excl_AFOLU_CO2, scenario, region, entity, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, category="CAT0-CAT4/5") %>% select(scenario, region, category, entity, unit, year, value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- spread(PRIMAP_selec_CO2_Excl_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- data.frame(PRIMAP_selec_CO2_Excl_AFOLU_CO2)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, variable="Emissions|CO2 Excl. AFOLU") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_CO2_Excl_AFOLU_CO2, file="data/History_CO2_ExclAFOLU.csv", sep=";", row.names = FALSE)

PRIMAP_selec_CH4 <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="CH4")
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_CH4 <- cbind(PRIMAP_selec_CH4[1:5], GWPCH4*PRIMAP_selec_CH4[, 6:ncol(PRIMAP_selec_CH4)]/1000)
PRIMAP_selec_CH4$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_CH4 <- data.frame(PRIMAP_selec_CH4)
PRIMAP_selec_CH4 <- mutate(PRIMAP_selec_CH4, variable="Emissions|CH4") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_CH4, file="data/History_CH4.csv", sep=";", row.names = FALSE)

PRIMAP_selec_N2O <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="N2O")
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_N2O <- cbind(PRIMAP_selec_N2O[1:5], GWPN2O*PRIMAP_selec_N2O[, 6:ncol(PRIMAP_selec_N2O)]/1000)
PRIMAP_selec_N2O$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_N2O <- data.frame(PRIMAP_selec_N2O)
PRIMAP_selec_N2O <- mutate(PRIMAP_selec_N2O, variable="Emissions|N2O") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_N2O, file="data/History_N2O.csv", sep=";", row.names = FALSE)

PRIMAP_selec_FGases <- filter(PRIMAP_CDLINKS, category=="CAT0", entity==ifelse(GWPCH4==25, "FGASESAR4", "FGASES"))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, scenario, region, category, entity, unit, num_range("X", 1990:2015))
PRIMAP_selec_FGases <- cbind(PRIMAP_selec_FGases[1:5], PRIMAP_selec_FGases[, 6:ncol(PRIMAP_selec_FGases)]/1000)
PRIMAP_selec_FGases$unit <- "Mt CO2-equiv/yr"
PRIMAP_selec_FGases <- data.frame(PRIMAP_selec_FGases)
PRIMAP_selec_FGases <- mutate(PRIMAP_selec_FGases, variable="Emissions|FGases") %>% select(variable, scenario, region, category, entity, unit, everything())
write.table(PRIMAP_selec_FGases, file="data/History_FGases.csv", sep=";", row.names = FALSE)

# add gases to one data frame
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, PRIMAP_selec_AFOLU_CO2) %>% 
                      rbind(PRIMAP_selec_CH4) %>%
                      rbind(PRIMAP_selec_N2O) %>% 
                      rbind(PRIMAP_selec_FGases)
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, source="PRIMAP")
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, statistic="value")
PRIMAP_selec_Gases <- select(PRIMAP_selec_Gases, variable, scenario, region, unit, source, statistic, everything())
PRIMAP_selec_Gases <- select(PRIMAP_selec_Gases, -category, -entity)
colnames(PRIMAP_selec_Gases) = gsub("X", "", colnames(PRIMAP_selec_Gases))
write.table(PRIMAP_selec_Gases, file="data/History_AllGases.csv", sep=";", row.names = FALSE)


# FIGURE 1
# Create GHG emissions data, median, min, max
# 1 data
d_cd_links_Kyoto <- filter(all_cd_links, scenario %in% scens, region %in% regions, variable=="Emissions|Kyoto Gases")
d_selec_Kyoto <- group_by(d_cd_links_Kyoto, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                              median=median(value,na.rm=TRUE),
                                                                                              min=min(value, na.rm=TRUE),
                                                                                              max=max(value, na.rm=TRUE),
                                                                                              tenp=quantile(value, .10, na.rm=TRUE),
                                                                                              ninetyp=quantile(value, .90, na.rm=TRUE))
d_selec_Kyoto <- gather(d_selec_Kyoto, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_selec_Kyoto <- spread(d_selec_Kyoto, key=year, value=value)
d_selec_Kyoto <- mutate(d_selec_Kyoto, source="CD-LINKS")
d_selec_Kyoto <- ungroup(d_selec_Kyoto)
d_selec_Kyoto <- select(d_selec_Kyoto, variable, scenario, region, unit, source, statistic, everything())
write.table(d_selec_Kyoto, file="data/Figure1_Kyoto.csv", sep=";", row.names = FALSE)

#TODO read efforst sharing
###EffortSharingEmissions <- readWorksheetFromFile("data/Effort sharing data_IMAGE_adjusted.xlsx", 
###                            sheet="emissions_AP_2C", 
###                            startRow = 2,
###                            endCol = 36)
###EffortSharingEmissions <- select(EffortSharingEmissions, -starts_with("class_"))
###EffortSharingEmissions <- mutate()
# TO DO: afmaken

# FIGURE 3 (bar chart countries)
# including rest of the world category
GHG_fig3 <- c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")
d_cd_links_GHG_countries <- filter(all_cd_links, scenario %in% scens, region %in% regions, variable %in% GHG_fig3)
# convert CH4 and N2O to MtCO2eq
d_cd_links_GHG_countries <- d_cd_links_GHG_countries %>% mutate(value2=ifelse(variable=="Emissions|CH4", value*GWPCH4, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_GHG_countries <- d_cd_links_GHG_countries %>% mutate(value2=ifelse(variable=="Emissions|N2O", value*GWPN2O/1000, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_GHG_countries$unit <- "Mt CO2-equiv/yr"
write.table(d_cd_links_GHG_countries, file="data/temp.csv", sep=";", row.names = FALSE)
d_cd_links_CO2_exclAFOLU <- filter(d_cd_links_GHG_countries, variable=="Emissions|CO2" | variable=="Emissions|CO2|AFOLU")
d_cd_links_CO2_exclAFOLU <- d_cd_links_CO2_exclAFOLU %>% mutate(value2=ifelse(variable=="Emissions|CO2|AFOLU", -1*value, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_CO2_exclAFOLU <- group_by(d_cd_links_CO2_exclAFOLU, scenario, model, region, year, unit) %>% summarise(value=sum(value))
d_cd_links_CO2_exclAFOLU <- mutate(d_cd_links_CO2_exclAFOLU, variable="Emissions|CO2|Excl. AFOLU")
d_cd_links_CO2_exclAFOLU <- select(d_cd_links_CO2_exclAFOLU, scenario, model, region, year, unit, variable, value)
d_cd_links_CO2_exclAFOLU <- as.data.frame(d_cd_links_CO2_exclAFOLU)
d_cd_links_GHG_countries <- rbind(d_cd_links_GHG_countries, d_cd_links_CO2_exclAFOLU)
d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, variable!="Emissions|CO2")
d_cd_links_GHG_countries <- arrange(d_cd_links_GHG_countries, scenario, region, year, variable, model)

d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, year>=2010, year<=2050)
d_cd_links_GHG_countries_stat <- group_by(d_cd_links_GHG_countries, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
d_cd_links_GHG_countries_stat <- gather(d_cd_links_GHG_countries_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_cd_links_GHG_countries_stat <- spread(d_cd_links_GHG_countries_stat, key=year, value=value)
d_cd_links_GHG_countries_stat <- mutate(d_cd_links_GHG_countries_stat, source="CD-LINKS")
d_cd_links_GHG_countries_stat <- select(d_cd_links_GHG_countries_stat, variable, scenario, region, unit, source, statistic, everything())
write.table(d_cd_links_GHG_countries_stat , file="data/figure3.csv", sep=";", row.names = FALSE)


# FIGURE 5
start_year_fig5=2010
end_year_fig5=2050
# Three steps 1) determine historical emissions (per region) for 1850-2015, 2) determine projections 3) In Excel, determine budgets
#1. Historical cumulative emissions 1990-2015
#tmp_data <- (PRIMAP_selec_CO2_1850_2015, 7:ncol(PRIMAP_selec_CO2_1850_2015),key='year',value=value)
# determine cumulative budgets for historic periods 1850-1990 and 1990-2015
# make dataframe structure for CO2 emissions equal to data used for figure
# 1850-1989
PRIMAP_selec_CO2_1850_2015 <- filter(PRIMAP, country %in% regions_PRIMAP_history, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, scenario, country, category, entity, unit, num_range("X", 1850:start_year_fig5))
PRIMAP_selec_CO2_1850_2015 <- cbind(PRIMAP_selec_CO2_1850_2015[1:5], PRIMAP_selec_CO2_1850_2015[, 6:ncol(PRIMAP_selec_CO2_1850_2015)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EARTH","World")
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EU28","EU")
PRIMAP_selec_CO2_1850_2015$scenario=str_replace_all(PRIMAP_selec_CO2_1850_2015$scenario,"HISTORY","History")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, variable="Emissions|CO2")
colnames(PRIMAP_selec_CO2_1850_2015)[colnames(PRIMAP_selec_CO2_1850_2015)=="country"] <- "region"
PRIMAP_selec_CO2_1850_2015$region <- factor(PRIMAP_selec_CO2_1850_2015$region, levels=regions)
PRIMAP_selec_CO2_1850_2015$unit <- "Mt CO2"
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, source="PRIMAP")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, statistic="value")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, variable, scenario, region, unit, source, statistic, num_range("X", 1850:start_year_fig5))
colnames(PRIMAP_selec_CO2_1850_2015) = gsub("X", "", colnames(PRIMAP_selec_CO2_1850_2015))


#2. Remaining emissions depending on budget (400, 1000, 1600)
d_cd_links_CO2 <- select(all_cd_links, scenario, model, region, year, value, unit, variable)
d_cd_links_CO2 <- filter(d_cd_links_CO2, scenario %in% scens, region %in% regions, variable=="Emissions|CO2")
d_cd_links_CO2=data.table(d_cd_links_CO2)
yy=seq(start_year_fig5,2100)
d_cd_links_CO2 = d_cd_links_CO2[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('scenario','model','region', 'unit', 'variable')]
setnames(d_cd_links_CO2,"V1","value")
setnames(d_cd_links_CO2,"V2","year")
setcolorder(d_cd_links_CO2,c('scenario','model','region', 'year', 'value', 'unit', 'variable'))
d_cd_links_CO2 <- filter(d_cd_links_CO2, year>start_year_fig5, year<=end_year_fig5)
d_cd_links_CO2 <- spread(d_cd_links_CO2, key=year, value=value)

data_figure5_CO2emissions_model <- inner_join(PRIMAP_selec_CO2_1850_2015, d_cd_links_CO2, by=c('region'))
data_figure5_CO2emissions_model <- select(data_figure5_CO2emissions_model, variable.y, region, scenario.y, model, unit.y, num_range("", 1850:end_year_fig5))
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, variable=variable.y, unit=unit.y)
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, scenario=scenario.y)
data_figure5_CO2emissions_model$region <- factor(data_figure5_CO2emissions_model$region, levels=regions)
data_figure5_CO2emissions_model$scenario <- factor(data_figure5_CO2emissions_model$scenario, levels=scens)

#data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, num_range("", start_year_fig5:end_year_fig5), key="year", value=value)
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, 6:ncol(data_figure5_CO2emissions_model), key="year", value=value)
data_figure5_CO2emissions_stat <- group_by(data_figure5_CO2emissions_stat, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                        median=median(value,na.rm=TRUE),
                                                                                                        min=min(value, na.rm=TRUE),
                                                                                                        max=max(value, na.rm=TRUE),
                                                                                                        tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                        ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
data_figure5_CO2emissions_stat <- spread(data_figure5_CO2emissions_stat, key=year, value=value)
data_figure5_CO2emissions_stat$statistic <- factor(data_figure5_CO2emissions_stat$statistic, level=stats)
  
# 3. Determine statistics for 2100 budgets
data_figure5_CO2budget_stat <- filter(all_cd_links, scenario %in% scens, region %in% regions, year==2100, variable=="Carbon budget")
data_figure5_CO2budget_stat <- group_by(data_figure5_CO2budget_stat, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                       median=median(value,na.rm=TRUE),
                                                                                                       min=min(value, na.rm=TRUE),
                                                                                                       max=max(value, na.rm=TRUE),
                                                                                                       tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                       ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2budget_stat <- data_figure5_CO2budget_stat %>% ungroup() %>% select(-year)
data_figure5_CO2budget_stat <- gather(data_figure5_CO2budget_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=`2100`)
data_figure5_CO2budget_stat$statistic <- factor(data_figure5_CO2budget_stat$statistic, level=stats)

#4 Export to Excel (change later to R code)
write.table(data_figure5_CO2emissions_model , file="data/figure5_CO2emissions_model.csv", sep=";", row.names = FALSE) 
write.table(data_figure5_CO2emissions_stat , file="data/figure5_CO2emissions_stat.csv", sep=";", row.names = FALSE)
write.table(data_figure5_CO2budget_stat , file="data/figure5_CO2budget_stat.csv", sep=";", row.names = FALSE)

#5 Import again in R
data_figure5_model <- read.csv("data/data_figure5_ToR.csv", header=TRUE, sep=";")
data_figure5_stat <- data_figure5_model
data_figure5_stat <- gather(data_figure5_stat, 9:ncol(data_figure5_stat), key="year", value=value)
data_figure5_stat$value <- as.numeric(data_figure5_stat$value)
data_figure5_stat <- group_by(data_figure5_stat, variable, budget.scenario, budget.effort.sharing, scenario, region, unit, year) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_stat <- gather(data_figure5_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
#data_figure5_stat <- filter(data_figure5_stat, year>=1990, year<=2050)
data_figure5_stat <- spread(data_figure5_stat, key=year, value=value)
data_figure5_stat$statistic <- factor(data_figure5_stat$statistic, level=stats)
colnames(data_figure5_stat) = gsub("X", "", colnames(data_figure5_stat))
write.table(data_figure5_stat , file="data/figure5_stat.csv", sep=";", row.names = FALSE)

# plot
data_figure5_plot <- gather(data_figure5, 6:ncol(data_figure5), key="year", value=value)
data_figure5_plot <- filter(data_figure5_plot, region=="World", Category=="Carbon budget 1000", year>=start_year_fig5-5, year<=2020)
ggplot(data_figure5_plot) + geom_point(aes(x=year, y=value, colour=model)) + ylim(0, NA)
       



