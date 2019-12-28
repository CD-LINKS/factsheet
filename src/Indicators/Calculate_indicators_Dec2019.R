# Data processing ---------------------------------------------------------
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(tidyr)      # spread
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob
library(xtable)
library(grid)
library(scales)
library(readxl)
library(dplyr)      # %>%
library(ggplot2)    # ggplot

# Figure 1: Total GHG emissions
# Figure 2 en 3: breakdown of global GHG emissions to emissions type CO2, CH4, N2O, FGases and countries
# Figure 4: GHG/CO2 intensity
# Figure 5: carbon budget
# Figure 6: IMAGE innovation indicators (non-fossil share of final energy
# Figure 7: carbon investments, in 'Data_Low-carbon-investment-gap_v2.xlsx'
# Figure 8: air pollution (Sulphur, OC, BC)
# Figure 9: deforestation (forest cover)
# Figure 10: policy coverage
# Figure 11: peak year vs zero emissions eyar

# First read in data with Read_in_data_indicators.R

# GDP settings for figure 4
GDP_MER_PPP <- "PPP"

#adjust names for CD-LINKS data
all_cd_links <- all_indicators

# changes: only global models, Selection of Categories, change Category->scenario, period->year, remove Baseline
#all_cd_links <- filter(all_cd_links, Scope=="global")
#all_cd_links <- select(all_cd_links, -Scope)
all_cd_links <- select(all_cd_links, -scenario)
cats <- c("No new policies", "National policies", "NDC", "Carbon budget 1600", "Carbon budget 1000 (2030)", "Carbon budget 1000", "Carbon budget 400")
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
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000 (2030)", "2C_66 (2030)")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000", "2C_66")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1600", "2C_50")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 400", "1.5C_50")

all_scens <- unique(all_cd_links$scenario)
all_cd_links$scenario <- factor(all_cd_links$scenario, levels=all_scens)

# change unit names
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Mt CO2-equiv/yr", "Mt CO2eq/yr")

# General settings --------------------------------------------------------


# general settings
models_global <- c("AIM/CGE", "COPPE-COFFEE 1.0", "DNE21+ V.14", "GEM-E3", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "POLES CDL", "REMIND-MAgPIE 1.7-3.0", "WITCH2016")
scens_indicators <- c("No new policies", "National policies", "NDC", "2C_50", "2C_66", "1.5C_50", "2C_66 (2030)")
regions_indicators <- c("CAN", "BRA", "CHN", "EU", "IDN", "IND", "JPN", "RUS", "USA", "World", "ROW")
regions_indicators_peaked <- c("CAN", "BRA", "EU", "RUS", "USA")
regions_indicators_not_peaked <- c("CHN", "IDN", "IND", "JPN", "World", "ROW")
regions_indicators_incl_bunkers <- c("CAN","BRA", "CHN", "EU", "IDN", "IND", "JPN", "RUS", "USA", "World", "ROW", "Bunkers")
regions_indicators_IMAGE <- c("CAN","BRA", "CHN", "EU", "IDN", "INDIA", "INDO", "JAP", "RUS", "USA", "World", "ROW", "Bunkers")
years_indicators <- c('2005', '2010', '2015', '2020', '2025', '2030', '2035', '2040', '2045', '2050', '2055', '2060', '2065', '2070', '2075', '2080', '2085', '2090', '2095', '2100')
stats_indicators <- c('mean', 'median', 'min', 'max', 'tenp', 'ninetyp')

GWPCH4 = 25
GWPN2O = 298

# Historical data PRIMAP and EDGAR --------------------------------------------------


# Historical data from PRIMAP
# http://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:2959897
PRIMAP <- read.csv("indicators/data/historical_data/PRIMAP-hist_v1.2_14-Dec-2017.csv", header=TRUE, sep=",")
PRIMAP_CDLINKS <- PRIMAP
regions_indicators_CDLINKS_history <- c("CAN", "BRA", "CHN", "EU28", "IDN", "IND", "JPN", "RUS", "USA", "EARTH")
#years_indicators_CDLINKS_history <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015")
colnames(PRIMAP_CDLINKS)[colnames(PRIMAP_CDLINKS)=="country"] <- "region"
PRIMAP_CDLINKS <- filter(PRIMAP_CDLINKS, region %in% regions_indicators_CDLINKS_history)
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EARTH","World")
PRIMAP_CDLINKS$region=str_replace_all(PRIMAP_CDLINKS$region,"EU28","EU")
PRIMAP_CDLINKS$scenario=str_replace_all(PRIMAP_CDLINKS$scenario,"HISTORY","History")

# Historical data from EDGAR
# http://edgar.jrc.ec.europa.eu/overview.php?v=432_GHG&SECURE=123
EDGAR <- read.csv("indicators/data/historical_data/EDGAR.csv", header=TRUE, sep=";")
EDGAR_bunkers <- filter(EDGAR, Name %in% c('Int. Aviation', 'Int. Shipping'))
colnames(EDGAR_bunkers) = gsub("X", "", colnames(EDGAR_bunkers))
colnames(EDGAR_bunkers)[colnames(EDGAR_bunkers)=="World.Region"] <- "region"
EDGAR_bunkers <- gather(EDGAR_bunkers, 7:ncol(EDGAR_bunkers), key="year", value=value) %>%
                 select(-IPCC.Annex, -ISO_A3, -Name, -IPCC, -IPCC_description)
EDGAR_bunkers_total <- group_by(EDGAR_bunkers, year) %>% summarize(value=sum(value)) %>%
                       mutate(region="Bunkers") %>%
                       select(year, region, value)
EDGAR_bunkers_total$value <- 10^-3*EDGAR_bunkers_total$value
EDGAR_bunkers_total$unit <- "Mt CO2eq/yr"

# Historical data Figure 1 ------------------------------------------------

# for figure 1)
# select data for Total Kyoto emissions from 1990 and transfer to MtCO2eq
PRIMAP_selec_Kyoto <- filter(PRIMAP_CDLINKS, category=="CAT0", entity=="KYOTOGHGAR4")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_Kyoto <- cbind(PRIMAP_selec_Kyoto[1:5], PRIMAP_selec_Kyoto[, 6:ncol(PRIMAP_selec_Kyoto)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, variable="Emissions|Kyoto Gases")
PRIMAP_selec_Kyoto$unit <- "Mt CO2eq/yr"
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, source="PRIMAP")
PRIMAP_selec_Kyoto <- mutate(PRIMAP_selec_Kyoto, statistic="value")
PRIMAP_selec_Kyoto <- select(PRIMAP_selec_Kyoto, variable, scenario, region, unit, source, statistic, num_range("X", 1970:2015))
colnames(PRIMAP_selec_Kyoto) = gsub("X", "", colnames(PRIMAP_selec_Kyoto))
# add missing 2013, 2014, 2015 data
tmp1 <- filter(EDGAR_bunkers_total, year==2012)
tmp1$year <- 2013
tmp2 <- filter(EDGAR_bunkers_total, year==2012)
tmp2$year <- 2014
tmp3 <- filter(EDGAR_bunkers_total, year==2012)
tmp3$year <- 2015
EDGAR_bunkers_total_adj <- rbind(EDGAR_bunkers_total, tmp1) %>% rbind(tmp2) %>% rbind(tmp3)
# add bunkers to World total
tmp <- filter(PRIMAP_selec_Kyoto, region=="World")
tmp <- gather(tmp, 7:ncol(PRIMAP_selec_Kyoto), key="year", value=value)
tmp <- inner_join(tmp, EDGAR_bunkers_total_adj, by=c('year')) %>% 
       mutate(value=value.x+value.y) %>%
       select(variable, scenario, region.x, unit.x, source, statistic, year, value) %>%
       rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
tmp <- spread(tmp, key=year, value=value)
PRIMAP_selec_Kyoto <- filter(PRIMAP_selec_Kyoto, region!="World")
PRIMAP_selec_Kyoto <- rbind(PRIMAP_selec_Kyoto, tmp)
PRIMAP_selec_Kyoto_output <- select(PRIMAP_selec_Kyoto, variable, scenario, region, unit, source, statistic, num_range("", 1990:2015))
write.table(PRIMAP_selec_Kyoto_output, file="Indicators/data/stocktake_tool/History_Kyoto.csv", sep=";", row.names = FALSE)

# Historical data Figure 2, 3 -----------------------------------------

# for figure 2-3)
# Individual gases, split up in 'CO2 excl AFOLU CO2' and "AFOLU CO2"
PRIMAP_Fig2_3 <- PRIMAP_CDLINKS

PRIMAP_selec_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2 <- select(PRIMAP_selec_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_CO2 <- cbind(PRIMAP_selec_CO2[1:5], PRIMAP_selec_CO2[, 6:ncol(PRIMAP_selec_CO2)]/1000)
PRIMAP_selec_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_CO2 <- data.frame(PRIMAP_selec_CO2)
PRIMAP_selec_CO2 <- mutate(PRIMAP_selec_CO2, variable="Emissions|CO2") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_CO2) = gsub("X", "", colnames(PRIMAP_selec_CO2))
PRIMAP_selec_CO2 <- select(PRIMAP_selec_CO2, -category, -entity)
write.table(PRIMAP_selec_CO2, file="Indicators/data/stocktake_tool/History_CO2.csv", sep=";", row.names = FALSE)

PRIMAP_selec_Agriculture_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT4", entity=="CO2")
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_Agriculture_CO2 <- cbind(PRIMAP_selec_Agriculture_CO2[1:5], PRIMAP_selec_Agriculture_CO2[, 6:ncol(PRIMAP_selec_Agriculture_CO2)]/1000)
PRIMAP_selec_Agriculture_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_Agriculture_CO2 <- data.frame(PRIMAP_selec_Agriculture_CO2)
PRIMAP_selec_Agriculture_CO2 <- mutate(PRIMAP_selec_Agriculture_CO2, variable="Emissions|CO2|Agriculture") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_Agriculture_CO2) = gsub("X", "", colnames(PRIMAP_selec_Agriculture_CO2))
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, -category, -entity)
write.table(PRIMAP_selec_Agriculture_CO2, file="Indicators/data/stocktake_tool/History_CO2_Agriculture.csv", sep=";", row.names = FALSE)

PRIMAP_selec_LULUCF_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT5", entity=="CO2")
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_LULUCF_CO2 <- cbind(PRIMAP_selec_LULUCF_CO2[1:5], PRIMAP_selec_LULUCF_CO2[, 6:ncol(PRIMAP_selec_LULUCF_CO2)]/1000)
PRIMAP_selec_LULUCF_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_LULUCF_CO2 <- data.frame(PRIMAP_selec_LULUCF_CO2)
PRIMAP_selec_LULUCF_CO2 <- mutate(PRIMAP_selec_LULUCF_CO2, variable="Emissions|CO2|LULUCF") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_LULUCF_CO2) = gsub("X", "", colnames(PRIMAP_selec_LULUCF_CO2))
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, -category, -entity)
write.table(PRIMAP_selec_LULUCF_CO2, file="Indicators/data/stocktake_tool/History_CO2_LULUCF.csv", sep=";", row.names = FALSE)

# create AFOLU CO2' 
PRIMAP_selec_Agriculture_CO2_tmp <- gather(PRIMAP_selec_Agriculture_CO2, 5:ncol(PRIMAP_selec_Agriculture_CO2), key="year", value=value)
PRIMAP_selec_LULUCF_CO2_tmp <- gather(PRIMAP_selec_LULUCF_CO2, 5:ncol(PRIMAP_selec_LULUCF_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2 <- rbind(PRIMAP_selec_Agriculture_CO2_tmp, PRIMAP_selec_LULUCF_CO2_tmp)
PRIMAP_selec_AFOLU_CO2 <- group_by(PRIMAP_selec_AFOLU_CO2, scenario, region, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, category="CAT4+5") %>% select(scenario, region, unit, year, value)
PRIMAP_selec_AFOLU_CO2 <- spread(PRIMAP_selec_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_AFOLU_CO2 <- data.frame(PRIMAP_selec_AFOLU_CO2)
PRIMAP_selec_AFOLU_CO2 <- mutate(PRIMAP_selec_AFOLU_CO2, variable="Emissions|CO2|AFOLU") %>% select(variable, scenario, region, unit, everything())
colnames(PRIMAP_selec_AFOLU_CO2) = gsub("X", "", colnames(PRIMAP_selec_AFOLU_CO2))
#PRIMAP_selec_AFOLU_CO2 <- select(PRIMAP_selec_AFOLU_CO2, -category, -entity)
write.table(PRIMAP_selec_AFOLU_CO2, file="Indicators/data/stocktake_tool/History_CO2_AFOLU.csv", sep=";", row.names = FALSE)

# create 'CO2 excl AFOLU CO2'
PRIMAP_selec_CO2_tmp <- gather(PRIMAP_selec_CO2, 5:ncol(PRIMAP_selec_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp <- gather(PRIMAP_selec_AFOLU_CO2, 5:ncol(PRIMAP_selec_AFOLU_CO2), key="year", value=value)
PRIMAP_selec_AFOLU_CO2_tmp$value <- -1*PRIMAP_selec_AFOLU_CO2_tmp$value
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_tmp, PRIMAP_selec_AFOLU_CO2_tmp)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- group_by(PRIMAP_selec_CO2_Excl_AFOLU_CO2, scenario, region, unit, year) %>% summarise(value=sum(value))
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, category="CAT0-CAT4/5") %>% select(scenario, region, unit, year, value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- spread(PRIMAP_selec_CO2_Excl_AFOLU_CO2, key=year, value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- data.frame(PRIMAP_selec_CO2_Excl_AFOLU_CO2)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- mutate(PRIMAP_selec_CO2_Excl_AFOLU_CO2, variable="Emissions|CO2|Excl. AFOLU") %>% select(variable, scenario, region, unit, everything())
colnames(PRIMAP_selec_CO2_Excl_AFOLU_CO2) = gsub("X", "", colnames(PRIMAP_selec_CO2_Excl_AFOLU_CO2))
# add bunkers to world total
tmp <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region=="World")
tmp <- gather(tmp, 5:ncol(PRIMAP_selec_CO2_Excl_AFOLU_CO2), key="year", value=value)
tmp <- inner_join(tmp, EDGAR_bunkers_total_adj, by=c('year')) %>% 
  mutate(value=value.x+value.y) %>%
  select(variable, scenario, region.x, unit.x, year, value) %>%
  rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
tmp <- spread(tmp, key=year, value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region!="World")
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, tmp)
# add bunkers as region
tmp_bunkers <- spread(EDGAR_bunkers_total_adj, key=year, value=value)
tmp_bunkers <- mutate(tmp_bunkers, variable="Emissions|CO2|Excl. AFOLU")
tmp_bunkers <- mutate(tmp_bunkers, scenario="History")
tmp_bunkers <- select(tmp_bunkers, variable, scenario, region, unit, everything())
PRIMAP_selec_CO2_Excl_AFOLU_CO2 <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, tmp_bunkers)
write.table(PRIMAP_selec_CO2_Excl_AFOLU_CO2, file="Indicators/data/stocktake_tool/History_CO2_ExclAFOLU.csv", sep=";", row.names = FALSE)


PRIMAP_selec_CH4 <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="CH4")
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_CH4 <- cbind(PRIMAP_selec_CH4[1:5], GWPCH4*PRIMAP_selec_CH4[, 6:ncol(PRIMAP_selec_CH4)]/1000)
PRIMAP_selec_CH4$unit <- "Mt CO2eq/yr"
PRIMAP_selec_CH4 <- data.frame(PRIMAP_selec_CH4)
PRIMAP_selec_CH4 <- mutate(PRIMAP_selec_CH4, variable="Emissions|CH4") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_CH4) = gsub("X", "", colnames(PRIMAP_selec_CH4))
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, -category, -entity)
write.table(PRIMAP_selec_CH4, file="Indicators/data/stocktake_tool/History_CH4.csv", sep=";", row.names = FALSE)

PRIMAP_selec_N2O <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="N2O")
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_N2O <- cbind(PRIMAP_selec_N2O[1:5], GWPN2O*PRIMAP_selec_N2O[, 6:ncol(PRIMAP_selec_N2O)]/1000)
PRIMAP_selec_N2O$unit <- "Mt CO2eq/yr"
PRIMAP_selec_N2O <- data.frame(PRIMAP_selec_N2O)
PRIMAP_selec_N2O <- mutate(PRIMAP_selec_N2O, variable="Emissions|N2O") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_N2O) = gsub("X", "", colnames(PRIMAP_selec_N2O))
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, -category, -entity)
write.table(PRIMAP_selec_N2O, file="Indicators/data/stocktake_tool/History_N2O.csv", sep=";", row.names = FALSE)

PRIMAP_selec_FGases <- filter(PRIMAP_Fig2_3, category=="CAT0", entity==ifelse(GWPCH4==25, "FGASESAR4", "FGASES"))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_FGases <- cbind(PRIMAP_selec_FGases[1:5], PRIMAP_selec_FGases[, 6:ncol(PRIMAP_selec_FGases)]/1000)
PRIMAP_selec_FGases$unit <- "Mt CO2eq/yr"
PRIMAP_selec_FGases <- data.frame(PRIMAP_selec_FGases)
PRIMAP_selec_FGases <- mutate(PRIMAP_selec_FGases, variable="Emissions|F-Gases") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_FGases) = gsub("X", "", colnames(PRIMAP_selec_FGases))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, -category, -entity)
write.table(PRIMAP_selec_FGases, file="Indicators/data/stocktake_tool/History_FGases.csv", sep=";", row.names = FALSE)

# add gases to one data frame
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_CO2_Excl_AFOLU_CO2, PRIMAP_selec_AFOLU_CO2) %>% 
                      rbind(PRIMAP_selec_CH4) %>%
                      rbind(PRIMAP_selec_N2O) %>% 
                      rbind(PRIMAP_selec_FGases)
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, source="PRIMAP")
PRIMAP_selec_Gases <- mutate(PRIMAP_selec_Gases, statistic="value")
PRIMAP_selec_Gases <- select(PRIMAP_selec_Gases, variable, scenario, region, unit, source, statistic, everything())
# add total Kyoto emissions
tmp_bunkers_fig3 <- as.data.frame(tmp_bunkers)
tmp_bunkers_fig3$variable <- "Emissions|Kyoto Gases"
tmp_bunkers_fig3 <- mutate(tmp_bunkers_fig3, source="EDGAR")
tmp_bunkers_fig3 <- mutate(tmp_bunkers_fig3, statistic="value") 
tmp_bunkers_fig3 <- select(tmp_bunkers_fig3, variable, scenario, region, unit, source, statistic, everything())
PRIMAP_selec_Kyoto_fig3 <- rbind(PRIMAP_selec_Kyoto, tmp_bunkers_fig3)
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_Gases, PRIMAP_selec_Kyoto_fig3)
#PRIMAP_selec_Gases <- as.data.frame(PRIMAP_selec_Gases)
#colnames(PRIMAP_selec_Gases) = gsub("X", "", colnames(PRIMAP_selec_Gases))
# determine RoW category
Countries_Gases_hist <- gather(PRIMAP_selec_Gases, 7:ncol(PRIMAP_selec_Gases), key="year", value=value)
Countries_Gases_hist <- Countries_Gases_hist %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases_hist <- group_by(Countries_Gases_hist, variable, scenario, unit, statistic, year) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases_hist <- mutate(RoW_Gases_hist, source="PRIMAP")
RoW_Gases_hist <- mutate(RoW_Gases_hist, region="ROW") %>% select(variable, scenario, unit, source, statistic, region, year, value)
RoW_Gases_hist <- spread(RoW_Gases_hist, key='year', value=value)
RoW_Gases_hist <- as.data.frame(RoW_Gases_hist)
PRIMAP_selec_Gases <- rbind(PRIMAP_selec_Gases, RoW_Gases_hist)
PRIMAP_selec_Gases <- arrange(PRIMAP_selec_Gases, variable, region)
PRIMAP_selec_Gases_output <- select(PRIMAP_selec_Gases, variable, scenario, region, unit, source, statistic, num_range("", 1990:2015))
write.table(filter(PRIMAP_selec_Gases_output, variable!="Emissions|Kyoto Gases"), file="Indicators/data/stocktake_tool/History_fig2.csv", sep=";", row.names = FALSE)
write.table(PRIMAP_selec_Gases_output, file="Indicators/data/stocktake_tool/History_fig3.csv", sep=";", row.names = FALSE)

# Historical data Figure 4------------------------------------------
Kyoto_hist <- gather(PRIMAP_selec_Kyoto, 7:ncol(PRIMAP_selec_Kyoto), key="year", value=value)
Kyoto_hist <- filter(Kyoto_hist, year>=1990, year<=2015)
Kyoto_hist$year <- as.numeric(Kyoto_hist$year)
Kyoto_hist$region <- factor(Kyoto_hist$region, levels=regions_indicators)
Kyoto_hist <- select(Kyoto_hist, -source, -statistic)
#CO2_hist <- gather(PRIMAP_selec_CO2, 5:ncol(PRIMAP_selec_CO2), key="year", value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region!="Bunkers")
CO2_hist <- gather(PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers, 5:ncol(PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers), key="year", value=value)
CO2_hist <- filter(CO2_hist, year>=1990, year<=2015)
CO2_hist$year <- as.numeric(CO2_hist$year)
CO2_hist$region <- factor(CO2_hist$region, levels=regions_indicators)
GHG_hist <- rbind(Kyoto_hist, CO2_hist)
# Use OECD GDP history from IMAGE model (million $US2010)
GDP_MER_IMAGE <- NoPolicy$GDP_MER
GDP_MER_IMAGE$region=str_replace_all(GDP_MER_IMAGE$region,"INDIA", "IND")
GDP_MER_IMAGE$region=str_replace_all(GDP_MER_IMAGE$region,"JAP", "JPN")
GDP_MER_hist <- filter(GDP_MER_IMAGE, year>=1990, year<=2015, region %in% regions_indicators)
GDP_MER_hist$region <- factor(GDP_MER_hist$region, levels=regions_indicators)
# Use World Bank GDP (PPP) SSP2 database (billion $US2005 converted to million $US2010)
# https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=30
# See "GDP PPP from SSP database.xlsx"
#GDP_PPP_hist <- read.table("Indicators/data/historical_data/GDP_PPP_SSP_hist.csv", header=TRUE, sep=";")
GDP_PPP_hist <- read.table("Indicators/data/historical_data/GDP_PPP_WDI_hist.csv", header=TRUE, sep=";")
colnames(GDP_PPP_hist) = gsub("X", "", colnames(GDP_PPP_hist))
GDP_PPP_hist <- gather(GDP_PPP_hist, 3:ncol(GDP_PPP_hist), key="year", value=value)
#GDP_PPP_hist$value <- 1107.74*GDP_PPP_hist$value #convert to million $2010 dollars
# https://stats.areppim.com/calc/calc_usdlrxdeflator.php
GDP_PPP_hist$value <- 0.98*1000*GDP_PPP_hist$value #convert to million $2010 dollars
GDP_PPP_hist$unit <- "billion US$2010/yr"
GDP_PPP_hist$year <- as.numeric(GDP_PPP_hist$year)

if(GDP_MER_PPP=="PPP") { GHG_intensity_hist <- inner_join(GHG_hist, GDP_PPP_hist, by=c('year', 'region'))
                         GHG_intensity_hist <- mutate(GHG_intensity_hist, value=value.x/value.y)
                         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|Kyoto Gases", "variable"] <- "GHG Intensity of GDP|PPP"
                         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|CO2|Excl. AFOLU", "variable"] <- "Carbon Intensity (excl AFOLU) of GDP|PPP"
} else { GHG_intensity_hist <- inner_join(GHG_hist, GDP_MER_hist, by=c('year', 'region'))
         GHG_intensity_hist <- mutate(GHG_intensity_hist, value=value.x/value.y)
         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|Kyoto Gases", "variable"] <- "GHG Intensity of GDP|MER"
         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|CO2|Excl. AFOLU", "variable"] <- "Carbon Intensity of GDP|MER"
}
GHG_intensity_hist$scenario <- "History"
GHG_intensity_hist <- mutate(GHG_intensity_hist, unit="Mt CO2eq/million US($2010)")
GHG_intensity_hist$source <- "PRIMAP, World Bank"
GHG_intensity_hist$statistic <- "value"
GHG_intensity_hist <- select(GHG_intensity_hist, variable, scenario, region, unit, source, statistic, year, value)
write.table(GHG_intensity_hist, paste0("Indicators/data/stocktake_tool/GHG_intensity_", GDP_MER_PPP, "_hist.csv"), sep=";", row.names=F)

# check, this line of code has probabl moved by accident
#d_selec_Kyoto_stat <- gather(d_selec_Kyoto_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)

# annual intensity improvement
GHG_intensity_rate_annual_hist <- group_by(GHG_intensity_hist, variable, scenario, region, unit, source) %>% 
                                  mutate(value=value/lag(value)-1) %>%
                                  select(variable, scenario, region, unit, source, statistic, year, value) %>%
                                  filter(year>=1980)
GHG_intensity_rate_annual_hist <- filter(GHG_intensity_rate_annual_hist, year>=1990)
GHG_intensity_rate_annual_hist$value <- 100*GHG_intensity_rate_annual_hist$value
GHG_intensity_rate_annual_hist <- spread(GHG_intensity_rate_annual_hist, key='year', value=value)
write.table(GHG_intensity_rate_annual_hist, file="Indicators/data/stocktake_tool/History_GHG_Intensity_improvement_1yrperiod.csv", sep=";", row.names = FALSE)

# annual intensity improvement based on 5-year periods
GHG_intensity_5yr_hist <- filter(GHG_intensity_hist, year %in% c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015))
write.table(GHG_intensity_5yr_hist, "Indicators/data/stocktake_tool/GHG_intensity_hist_5y.csv", sep=";", row.names=F)
GHG_intensity_rate_annual5yrperiod_hist <- group_by(GHG_intensity_5yr_hist, variable, scenario, region, unit, source) %>% 
                                           mutate(value=(value/lag(value))^(1/5)-1) %>%
                                           select(variable, scenario, region, unit, source, statistic, year, value) %>%
                                           filter(year>=1980)
GHG_intensity_rate_annual5yrperiod_hist <- filter(GHG_intensity_rate_annual5yrperiod_hist, year>=1990)
GHG_intensity_rate_annual5yrperiod_hist$value <- 100*GHG_intensity_rate_annual5yrperiod_hist$value
GHG_intensity_rate_annual5yrperiod_hist <- spread(GHG_intensity_rate_annual5yrperiod_hist, key='year', value=value)
write.table(GHG_intensity_rate_annual5yrperiod_hist, file="Indicators/data/stocktake_tool/History_GHG_Intensity_improvement_5yrperiod.csv", sep=";", row.names = FALSE)

# Historical data Figure 11
all_peak_tmp2 <- filter(all_import_before_add_variables, variable=='Emissions|CO2') %>%
  group_by(scenario,Category,Baseline,model,region,Scope,unit,variable) %>%
  arrange(desc(value)) %>%
  filter(rank(period) == 1) 

# Historical data Figure 11 -----------------------------------------------


# peak_historical_Kyoto <- filter(PRIMAP_selec_Kyoto, region=="World")
# See Levin and Rich (2017), and UN Environment GAP report 2018
# https://wriorg.s3.amazonaws.com/s3fs-public/turning-points-trends-countries-reaching-peak-greenhouse-gas-emissions-over-time.pdf
peak_historical_Kyoto <- gather(PRIMAP_selec_Kyoto, 7:ncol(PRIMAP_selec_Kyoto), key="year", value="value")
peak_historical_Kyoto$region <- factor(peak_historical_Kyoto$region, levels=regions_indicators)
peak_historical_Kyoto <- filter(peak_historical_Kyoto, !(region %in% c('CHN', 'IDN', 'IND', 'JPN', 'World'))) %>%
                         group_by(variable, scenario, region, unit, source, statistic) %>%
                         filter(rank(desc(value))==1)
peak_historical_Kyoto <- select(peak_historical_Kyoto, -value) %>% 
                         ungroup() %>%
                         mutate(value=year, variable="Peak year|Kyoto Gases")
peak_historical_Kyoto$year <- 2015

peak_historical_CO2 <- mutate(PRIMAP_selec_CO2, source="PRIMAP", statistic="value") %>%
                       select(variable, scenario, region, unit, source, statistic, everything())
peak_historical_CO2 <- gather(peak_historical_CO2, 7:ncol(peak_historical_CO2), key="year", value="value")
peak_historical_CO2$region <- factor(peak_historical_CO2$region, levels=regions_indicators)
peak_historical_CO2 <- filter(peak_historical_CO2, !(region %in% c('CHN', 'IDN', 'IND', 'JPN', 'World'))) %>%
                       group_by(variable, scenario, region, unit, source, statistic) %>%
                       filter(rank(desc(value))==1)
                       peak_historical_CO2 <- select(peak_historical_CO2, -value) %>% 
                       ungroup() %>%
                       mutate(value=year, variable="Peak year|CO2")
peak_historical_CO2$year <- 2015  

peak_historical <- rbind(peak_historical_Kyoto, peak_historical_CO2)

# Figure 1 ----------------------------------------------------------------
# Emissions gap

# Data FIGURE 1
# Create GHG emissions data, median, min, max
# 1 data
d_selec_Kyoto <- filter(all_cd_links, Scope=="global", scenario %in% scens_indicators, region %in% regions_indicators, year>=2010, year<=2050, variable=="Emissions|Kyoto Gases")
d_selec_Kyoto_glob_stat <- group_by(d_selec_Kyoto, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                    median=median(value,na.rm=TRUE),
                                                                                                    min=min(value, na.rm=TRUE),
                                                                                                    max=max(value, na.rm=TRUE),
                                                                                                    tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                    ninetyp=quantile(value, .90, na.rm=TRUE))
d_selec_Kyoto_glob_stat <- gather(d_selec_Kyoto_glob_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_selec_Kyoto_glob_stat <- spread(d_selec_Kyoto_glob_stat, key=year, value=value)
d_selec_Kyoto_glob_stat <- mutate(d_selec_Kyoto_glob_stat, source="CD-LINKS_global")
d_selec_Kyoto_glob_stat <- ungroup(d_selec_Kyoto_glob_stat)
d_selec_Kyoto_glob_stat <- select(d_selec_Kyoto_glob_stat, variable, scenario, region, unit, source, statistic, everything())
# add national models data
d_selec_Kyoto_nat <- filter(all_cd_links, Scope=="national", scenario %in% scens_indicators, region %in% regions_indicators, year>=2010, year<=2050, 
                            variable=="Emissions|Kyoto Gases")
# National model names are not shown in tool, so for regions with two models, the dataset for figure 1 contains duplicates
d_selec_Kyoto_nat <- mutate(d_selec_Kyoto_nat, source="CD-LINKS_national", statistic="value") %>%
                     select(variable, scenario, region, unit, source, statistic, everything())
d_selec_Kyoto_nat <- spread(d_selec_Kyoto_nat, key="year", value="value") %>%
                     select(-model, -Scope)
d_selec_Kyoto_stat <- rbind(d_selec_Kyoto_glob_stat, d_selec_Kyoto_nat)
write.table(d_selec_Kyoto_stat, file="Indicators/data/stocktake_tool/figure1_without_ES.csv", sep=";", row.names = FALSE)

data_fig1_proj <- gather(d_selec_Kyoto_stat, 7:ncol(d_selec_Kyoto_stat), key="year", value=value)
data_fig1_hist <- gather(PRIMAP_selec_Kyoto_output, 7:ncol(PRIMAP_selec_Kyoto_output), key="year", value=value)
data_fig1 <- rbind(data_fig1_hist, data_fig1_proj)

#TODO read efforst sharing

# Effort sharing 2C_50_AP	--> emissions_AP_1600
# Effort sharing 2C_66_AP	--> emissions_AP_1000
# Effort sharing 1.5C_50_AP	--> emissions_AP_400

# Effort sharing 2C_50_PCC --> 	emissions_PCC_1600
# Effort sharing 2C_66_PCC --> emissions_PCC_1000
# Effort sharing 1.5C_50_PCC --> emissions_PCC_400

# Effort sharing 2C_50_IEPC	--> emissions_IEPC_1600
# Effort sharing 2C_66_IEPC	--> emissions_IEPC_1000
# Effort sharing 1.5C_50_IEPC	--> emissions_IEPC_400

# Effort sharing 2C_50_GF	--> emissions_GF_1600
# Effort sharing 2C_66_GF	--> emissions_GF_1000
# Effort sharing 1.5C_50_GF	--> emissions_GF_400

ES_sheets <- excel_sheets("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx")

ES_AP_1600 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_AP_1600",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_50_AP")
ES_AP_1000 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_AP_1000",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_66_AP")
ES_AP_400 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_AP_400",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 1.5C_50_AP")
ES_PCC_1600 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_PCC_1600",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_50_PCC")
ES_PCC_1000 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_PCC_1000",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_66_PCC")
ES_PCC_400 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_PCC_400",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 1.5C_50_PCC")
ES_IEPC_1600 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_IEPC_1600",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_50_IEPC")
ES_IEPC_1000 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_IEPC_1000",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_66_IEPC")
ES_IEPC_400 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_IEPC_400",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 1.5C_50_IEPC")
ES_GF_1600 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_GF_1600",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_50_GF")
ES_GF_1000 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_GF_1000",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 2C_66_GF")
ES_GF_400 <- read_excel("Indicators/data/Effort sharing data_IMAGE_vJuly2019_adjusted.xlsx", sheet = "emissions_GF_400",range = "A2:AJ29") %>% mutate(scenario="Effort sharing 1.5C_50_GF")

ES_data <- rbind(ES_AP_1600, ES_AP_1000) %>% rbind(ES_AP_400) %>% rbind(ES_PCC_1600) %>% rbind(ES_PCC_1000) %>% rbind(ES_PCC_400) %>% 
  rbind(ES_IEPC_1600) %>% rbind(ES_IEPC_1000) %>% rbind(ES_IEPC_400) %>% rbind(ES_GF_1600) %>% rbind(ES_GF_1000) %>% rbind(ES_GF_400)
ES_data <- select(ES_data, -'class_ 27', -'class_ 27', -'class_ 28', -'class_ 29', -'class_ 30', -'class_ 31', -'class_ 32', -'class_ 33') %>%
  select(t, scenario, everything())
ES_data <- gather(ES_data, 3:ncol(ES_data), key="region", value="value")          
ES_data <- rename(ES_data, year=t) %>%
  mutate(variable="Emissions|Kyoto Gases", unit="Mt CO2-equiv/yr", source="IMAGE", statistic="value") %>%
  select(variable, scenario, region, unit, source, statistic, year, value) %>%
  filter(year>=2010, year<=2050, region %in% regions_indicators_IMAGE) %>%
  spread(key=year, value=value)
ES_data$region=str_replace_all(ES_data$region,"INDIA", "IND")
ES_data$region=str_replace_all(ES_data$region,"INDO", "IDN")
ES_data$region=str_replace_all(ES_data$region,"JAP", "JPN")
write.table(ES_data, file="Indicators/data/stocktake_tool/Figure1_Effort_sharing.csv", sep=";", row.names = FALSE)

d_figure1 <- rbind(ES_data, d_selec_Kyoto_stat)
write.table(d_figure1, file="Indicators/data/stocktake_tool/figure1.csv", sep=";", row.names = FALSE)

# Figure 2 and 3 ----------------------------------------------------------
# 2. Emissions per country
# 3. Emissions per GHG

#  Data FIGURE 2 and 3 (bar chart country split up and ghg emissions split up)
# including rest of the world category
GHG_fig3 <- c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")
d_cd_links_GHG_countries <- filter(all_cd_links, Scope=="global", scenario %in% scens_indicators, region %in% regions_indicators, variable %in% GHG_fig3)
d_bunkers_fig3 <- filter(all_cd_links, Scope=="global", region=="Bunkers", variable=="Emissions|Kyoto Gases")
d_bunkers_fig3$variable <- "Emissions|CO2|Excl. AFOLU"
d_cd_links_GHG_countries <- rbind(d_cd_links_GHG_countries, d_bunkers_fig3)
# convert CH4 and N2O to MtCO2eq
d_cd_links_GHG_countries <- d_cd_links_GHG_countries %>% mutate(value2=ifelse(variable=="Emissions|CH4", value*GWPCH4, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_GHG_countries <- d_cd_links_GHG_countries %>% mutate(value2=ifelse(variable=="Emissions|N2O", value*GWPN2O/1000, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_GHG_countries$unit <- "Mt CO2eq/yr"
d_cd_links_CO2_exclAFOLU <- filter(d_cd_links_GHG_countries, variable=="Emissions|CO2" | variable=="Emissions|CO2|AFOLU")
d_cd_links_CO2_exclAFOLU <- d_cd_links_CO2_exclAFOLU %>% mutate(value2=ifelse(variable=="Emissions|CO2|AFOLU", -1*value, value)) %>% select(-value) %>% rename(value=value2)
d_cd_links_CO2_exclAFOLU <- group_by(d_cd_links_CO2_exclAFOLU, scenario, model, region, year, unit) %>% summarise(value=sum(value))
d_cd_links_CO2_exclAFOLU <- mutate(d_cd_links_CO2_exclAFOLU, variable="Emissions|CO2|Excl. AFOLU")
d_cd_links_CO2_exclAFOLU <- select(d_cd_links_CO2_exclAFOLU, scenario, model, region, year, unit, variable, value)
d_cd_links_CO2_exclAFOLU <- as.data.frame(d_cd_links_CO2_exclAFOLU)
d_cd_links_GHG_countries_tmp <- select(d_cd_links_GHG_countries, -Scope)
d_cd_links_GHG_countries <- rbind(d_cd_links_GHG_countries_tmp, d_cd_links_CO2_exclAFOLU)
d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, variable!="Emissions|CO2")
d_cd_links_GHG_countries <- arrange(d_cd_links_GHG_countries, scenario, region, year, variable, model)
write.table(d_cd_links_GHG_countries, file="Indicators/data/stocktake_tooltemp2.csv", sep=";", row.names = FALSE)
d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, year>=2010, year<=2050)
d_cd_links_GHG_countries_stat <- group_by(d_cd_links_GHG_countries, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE))
                                                                                                                                 #mean=mean(value,na.rm=TRUE),
                                                                                                                                 #min=min(value, na.rm=TRUE),
                                                                                                                                 #max=max(value, na.rm=TRUE),
                                                                                                                                 #tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 #ninetyp=quantile(value, .90, na.rm=TRUE)
                                                                                                                                 #)
#d_cd_links_GHG_countries_stat <- gather(d_cd_links_GHG_countries_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_cd_links_GHG_countries_stat <- gather(d_cd_links_GHG_countries_stat, 'mean', key='statistic', value=value)
d_cd_links_GHG_countries_stat <- as.data.frame(d_cd_links_GHG_countries_stat)
d_cd_links_GHG_countries_stat$region <- factor(d_cd_links_GHG_countries_stat$region, levels=regions_indicators_incl_bunkers)
# determine RoW category
Countries_Gases <- d_cd_links_GHG_countries_stat %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases <- group_by(Countries_Gases, scenario, year, variable, unit, statistic) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases <- mutate(RoW_Gases, region="ROW") %>% select(scenario, region, year, variable, unit, statistic, value)
RoW_Gases <- as.data.frame(RoW_Gases)
d_cd_links_GHG_countries_stat <- rbind(d_cd_links_GHG_countries_stat, RoW_Gases)

d_cd_links_GHG_countries_stat_fig2 <- mutate(d_cd_links_GHG_countries_stat, source="CD-LINKS_global")
d_cd_links_GHG_countries_stat_fig2 <- d_cd_links_GHG_countries_stat_fig2[c("variable", "scenario", "region", "unit", "source", "statistic", "year", "value")]
d_cd_links_GHG_countries_stat_fig2 <- spread(d_cd_links_GHG_countries_stat_fig2, key="year", value=value)
write.table(d_cd_links_GHG_countries_stat_fig2 , file="Indicators/data/stocktake_toolfigure2.csv", sep=";", row.names = FALSE)

d_cd_links_GHG_excl_Kyoto_countries_stat <- spread(d_cd_links_GHG_countries_stat, key=year, value=value)
d_cd_links_GHG_excl_Kyoto_countries_stat <- mutate(d_cd_links_GHG_excl_Kyoto_countries_stat, source="CD-LINKS_global")
d_cd_links_GHG_excl_Kyoto_countries_stat <- select(d_cd_links_GHG_excl_Kyoto_countries_stat, variable, scenario, region, unit, source, statistic, everything())

# determine Kyoto RoW category
d_selec_Kyoto_stat_global <- filter(d_selec_Kyoto_stat, source=="CD-LINKS_global")
Countries_Gases_Kyoto <- gather(d_selec_Kyoto_stat_global, 7:ncol(d_selec_Kyoto_stat), key="year", value=value) %>% as.data.frame()
Countries_Gases_Kyoto <- Countries_Gases_Kyoto %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases_Kyoto <- group_by(Countries_Gases_Kyoto, scenario, year, variable, unit, statistic) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases_Kyoto <- mutate(RoW_Gases_Kyoto, region="ROW") %>% select(scenario, region, year, variable, unit, statistic, value) %>% mutate(source="CD-LINKS_global")
RoW_Gases_Kyoto <- as.data.frame(RoW_Gases_Kyoto)
Gases_Kyoto_stat <- rbind(Countries_Gases_Kyoto, RoW_Gases_Kyoto)
Gases_Kyoto_stat <- spread(Gases_Kyoto_stat, key="year", value=value)

# add total kyoto emissions
d_selec_Kyoto_median <- filter(Gases_Kyoto_stat, statistic=="mean")
d_cd_links_GHG_countries_stat <- rbind(d_cd_links_GHG_excl_Kyoto_countries_stat, d_selec_Kyoto_median)
d_cd_links_GHG_countries_stat$region <- factor(d_cd_links_GHG_countries_stat$region, level=regions_indicators_incl_bunkers)
d_cd_links_GHG_countries_stat <- arrange(d_cd_links_GHG_countries_stat, variable, scenario, region)
#d_cd_links_GHG_countries_stat_fig3 <- spread(d_cd_links_GHG_countries_stat, key="year", value=value)
write.table(d_cd_links_GHG_countries_stat, file="Indicators/data/stocktake_toolfigure3.csv", sep=";", row.names = FALSE)
write.table(d_cd_links_GHG_countries_stat, file="Indicators/data/stocktake_toolfigure2_3.csv", sep=";", row.names = FALSE)


#  Data Figure 4 ---------------------------------------------------------------
# Decarbonisation rate 

if (GDP_MER_PPP == "PPP") { intens=filter(all_cd_links, Scope=="global", variable%in%c("Carbon Intensity of GDP|PPP (excl AFOLU)","GHG Intensity of GDP|PPP"),
                            scenario %in% scens_indicators, region %in% regions_indicators, region!="Bunkers", year<=2050)
} else {intens=filter(all_cd_links, Scope=="global", variable%in%c("Carbon Intensity of GDP|MER (excl AFOLU)","GHG Intensity of GDP|MER"),
                      scenario %in% scens_indicators, region %in% regions_indicators, region!="Bunkers", year<=2050)
}
# delete 2005 MESSage values (previous: Add 2005 data to MESSAGE (median of other models))
intens=filter(intens, !(year==2005 & model=="MESSAGEix-GLOBIOM_1.0"))
intens[intens[,"year"]==2005 & intens[,"model"]=="MESSAGEix-GLOBIOM_1.0", "value"] <- 0
intens <- arrange(intens, variable, scenario, model, region, year)
write.table(intens,"Indicators/data/stocktake_toolfigure4_GHG_intensity.csv", sep=";", row.names = FALSE)
intens_stat <- group_by(intens, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                       median=median(value,na.rm=TRUE),
                                                                                       min=min(value, na.rm=TRUE),
                                                                                       max=max(value, na.rm=TRUE),
                                                                                       tenp=quantile(value, .10, na.rm=TRUE),
                                                                                       ninetyp=quantile(value, .90, na.rm=TRUE))
write.table(intens_stat, paste0("Indicators/data/stocktake_toolGHG_intensity_", GDP_MER_PPP, "_stat.csv"), sep=";", row.names=F)

# calculate annual rate (based on five year periods)
intensrate <- group_by(intens, scenario, model, region, unit, variable) %>% mutate(prev=lag(value)) %>% 
  mutate(value=(value/prev)^(1/5)-1) %>%
  select(variable, scenario, region, unit, model, year, value) %>% 
  filter(year>=2010)
#colnames(intensrate)[colnames(intensrate)=="rate"] <- "value"
intensrate$value <- 100*intensrate$value
intensrate$unit <- "%/yr"
intensrate_stat <- group_by(intensrate, variable, scenario, region, year, variable, unit) %>% summarise(median=median(value,na.rm=TRUE),
                       mean=mean(value,na.rm=TRUE),
                       min=min(value, na.rm=TRUE),
                       max=max(value, na.rm=TRUE),
                       tenp=quantile(value, .10, na.rm=TRUE),
                       ninetyp=quantile(value, .90, na.rm=TRUE))
intensrate_stat <- gather(intensrate_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
intensrate_stat <- spread(intensrate_stat, year, value)
write.table(intensrate_stat,"Indicators/data/stocktake_toolfigure4.csv", sep=";", row.names = FALSE)


#  Data Figure 5 ----------------------------------------------------------------
# Depletetion of carbon budget

regions_indicators_PRIMAP_history <- c("BRA", "CAN", "CHN", "EU28", "IDN", "IND", "JPN", "RUS", "USA", "CAN", "TUR", "EARTH")
start_year_projections_fig5=2010
end_year_projections_fig5=2050
# Three steps 1) determine historical emissions (per region) for 1850-2015, 2) determine projections 3) In Excel, determine budgets
#1. Historical cumulative emissions 1990-2015
#tmp_data <- (PRIMAP_selec_CO2_1850_2015, 7:ncol(PRIMAP_selec_CO2_1850_2015),key='year',value=value)
# determine cumulative budgets for historic periods 1850-1990 and 1990-2015
# make dataframe structure for CO2 emissions equal to data used for figure
# 1850-1989
PRIMAP_selec_CO2_1850_2015 <- filter(PRIMAP, country %in% regions_indicators_PRIMAP_history, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, scenario, country, category, entity, unit, num_range("X", 1850:start_year_projections_fig5))
PRIMAP_selec_CO2_1850_2015 <- cbind(PRIMAP_selec_CO2_1850_2015[1:5], PRIMAP_selec_CO2_1850_2015[, 6:ncol(PRIMAP_selec_CO2_1850_2015)]/1000)
# make dataframe structure for Kyoto emissions equal to data used for figure
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EARTH","World")
PRIMAP_selec_CO2_1850_2015$country=str_replace_all(PRIMAP_selec_CO2_1850_2015$country,"EU28","EU")
PRIMAP_selec_CO2_1850_2015$scenario=str_replace_all(PRIMAP_selec_CO2_1850_2015$scenario,"HISTORY","History")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, variable="Emissions|CO2")
colnames(PRIMAP_selec_CO2_1850_2015)[colnames(PRIMAP_selec_CO2_1850_2015)=="country"] <- "region"
PRIMAP_selec_CO2_1850_2015$region <- factor(PRIMAP_selec_CO2_1850_2015$region, levels=regions_indicators)
PRIMAP_selec_CO2_1850_2015$unit <- "Mt CO2"
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, source="PRIMAP")
PRIMAP_selec_CO2_1850_2015 <- mutate(PRIMAP_selec_CO2_1850_2015, statistic="value")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, variable, scenario, region, unit, source, statistic, num_range("X", 1850:start_year_projections_fig5))
colnames(PRIMAP_selec_CO2_1850_2015) = gsub("X", "", colnames(PRIMAP_selec_CO2_1850_2015))

#2. Remaining emissions depending on budget (400, 1000, 1600)
d_cd_links_CO2 <- filter(all_cd_links, Scope=="global") %>%
                  select(scenario, model, region, year, value, unit, variable)
d_cd_links_CO2 <- filter(d_cd_links_CO2, scenario %in% scens_indicators, region %in% regions_indicators, variable=="Emissions|CO2")
d_cd_links_CO2=data.table(d_cd_links_CO2)
yy=seq(start_year_projections_fig5,2100)
d_cd_links_CO2 = d_cd_links_CO2[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('scenario','model','region', 'unit', 'variable')]
setnames(d_cd_links_CO2,"V1","value")
setnames(d_cd_links_CO2,"V2","year")
setcolorder(d_cd_links_CO2,c('scenario','model','region', 'year', 'value', 'unit', 'variable'))
d_cd_links_CO2 <- filter(d_cd_links_CO2, year>start_year_projections_fig5, year<=end_year_projections_fig5)
d_cd_links_CO2 <- spread(d_cd_links_CO2, key=year, value=value)

data_figure5_CO2emissions_model <- inner_join(PRIMAP_selec_CO2_1850_2015, d_cd_links_CO2, by=c('region'))
data_figure5_CO2emissions_model <- select(data_figure5_CO2emissions_model, variable.y, region, scenario.y, model, unit.y, num_range("", 1850:end_year_projections_fig5))
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, variable=variable.y, unit=unit.y)
data_figure5_CO2emissions_model <- rename(data_figure5_CO2emissions_model, scenario=scenario.y)
data_figure5_CO2emissions_model$region <- factor(data_figure5_CO2emissions_model$region, levels=regions_indicators)
data_figure5_CO2emissions_model$scenario <- factor(data_figure5_CO2emissions_model$scenario, levels=scens_indicators)

#data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, num_range("", start_year_projections_fig5:end_year_projections_fig5), key="year", value=value)
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_model, 6:ncol(data_figure5_CO2emissions_model), key="year", value=value)
data_figure5_CO2emissions_stat <- group_by(data_figure5_CO2emissions_stat, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                        median=median(value,na.rm=TRUE),
                                                                                                        min=min(value, na.rm=TRUE),
                                                                                                        max=max(value, na.rm=TRUE),
                                                                                                        tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                        ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2emissions_stat <- gather(data_figure5_CO2emissions_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
data_figure5_CO2emissions_stat <- spread(data_figure5_CO2emissions_stat, key=year, value=value)
data_figure5_CO2emissions_stat$statistic <- factor(data_figure5_CO2emissions_stat$statistic, level=stats_indicators)
  
# 3. Determine statistics for 2100 budgets
data_figure5_CO2budget <- filter(all_cd_links, Scope=="global", scenario %in% scens_indicators, region %in% regions_indicators, year==2100, variable=="Carbon budget")
data_figure5_CO2budget_stat <- group_by(data_figure5_CO2budget, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                       median=median(value,na.rm=TRUE),
                                                                                                       min=min(value, na.rm=TRUE),
                                                                                                       max=max(value, na.rm=TRUE),
                                                                                                       tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                       ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_CO2budget_stat <- data_figure5_CO2budget_stat %>% ungroup() %>% select(-year)
data_figure5_CO2budget_stat <- gather(data_figure5_CO2budget_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=`2100`)
data_figure5_CO2budget_stat$statistic <- factor(data_figure5_CO2budget_stat$statistic, level=stats_indicators)

#4 Export to Excel (change later to R code) to translate annual CO2 emissions into budgets
write.table(data_figure5_CO2emissions_model , file="Indicators/data/stocktake_tool/figure5_CO2emissions_model.csv", sep=";", row.names = FALSE) 
write.table(data_figure5_CO2emissions_stat , file="Indicators/data/stocktake_tool/figure5_CO2emissions_stat.csv", sep=";", row.names = FALSE)
write.table(data_figure5_CO2budget_stat , file="Indicators/data/stocktake_tool/figure5_CO2budget_stat.csv", sep=";", row.names = FALSE)

#5 Import again in R (effort sharing)
data_figure5_model <- read.csv("Indicators/data/stocktake_tool/data_figure5_ToR.csv", header=TRUE, sep=";")
#data_figure5_stat <- data_figure5_model
colnames(data_figure5_model) = gsub("X", "", colnames(data_figure5_model))
data_figure5_model <- gather(data_figure5_model, 9:ncol(data_figure5_model), key="year", value=value)
data_figure5_model$value <- as.numeric(data_figure5_model$value)
data_figure5_stat <- group_by(data_figure5_model, variable, budget.scenario, budget.effort.sharing, scenario, region, unit, year) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_stat <- gather(data_figure5_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
#data_figure5_stat <- filter(data_figure5_stat, year>=1990, year<=2050)
data_figure5_stat$value <- as.numeric(data_figure5_stat$value)
data_figure5_stat <- spread(data_figure5_stat, key=year, value=value)
data_figure5_stat$statistic <- factor(data_figure5_stat$statistic, level=stats_indicators)
colnames(data_figure5_stat) = gsub("X", "", colnames(data_figure5_stat))
data_figure5_stat_fig <- select(data_figure5_stat, -(num_range("", 1850:1989))) %>% as.data.frame()
write.table(data_figure5_stat_fig , file="Indicators/data/stocktake_tool/figure5_stat.csv", sep=";", row.names = FALSE)

# plot
#data_figure5_plot <- gather(data_figure5, 6:ncol(data_figure5), key="year", value=value)
#data_figure5_plot <- filter(data_figure5_plot, region=="World", Category=="Carbon budget 1000", year>=start_year_projections_fig5-5, year<=2020)
#ggplot(data_figure5_plot) + geom_point(aes(x=year, y=value, colour=model)) + ylim(0, NA)
       





#  Data Figure 6 -----------------
# Based on PBL IMAGE data, not other models
rm(Fig6_elec); rm(Fig6_res); rm(Fig6_transport);
NoPolicy_fig6_elec <- filter(NoPolicy_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="No policy") %>% mutate(sector="Electricity")
NPi_fig6_elec  <- filter(NPi_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="National policies") %>% mutate(sector="Electricity")
INDCi_fig6_elec <- filter(INDCi_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="NDC") %>% mutate(sector="Electricity")
INDC2030i_1000_fig6_elec <- filter(INDC2030i_1000_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66 (2030)") %>% mutate(sector="Electricity")
NPi2020_1600_fig6_elec <- filter(NPi2020_1600_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_50") %>% mutate(sector="Electricity")
NPi2020_1000_fig6_elec <- filter(NPi2020_1000_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66") %>% mutate(sector="Electricity")
NPi2020_400_fig6_elec <- filter(NPi2020_400_i_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="1.5C_50") %>% mutate(sector="Electricity")
Fig6_elec <- rbind(NoPolicy_fig6_elec, NPi_fig6_elec) %>% 
             rbind(INDCi_fig6_elec) %>% 
             rbind(INDC2030i_1000_fig6_elec) %>% 
             rbind(NPi2020_1600_fig6_elec) %>% 
             rbind(NPi2020_1000_fig6_elec) %>% 
             rbind(NPi2020_400_fig6_elec)
Fig6_elec <- mutate(Fig6_elec, variable="Final Energy|Electricity|Non-fossil share")
Fig6_elec <- select(Fig6_elec, year, region, value, unit, scenario, sector, variable)

NoPolicy_fig6_res <- filter(NoPolicy_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="No policy") %>% mutate(sector="Residential buildings")
NPi_fig6_res  <- filter(NPi_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="National policies") %>% mutate(sector="Residential buildings")
INDCi_fig6_res <- filter(INDCi_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="NDC") %>% mutate(sector="Residential buildings")
INDC2030i_1000_fig6_res <- filter(INDC2030i_1000_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_66 (2030)", sector="Residential buildings")
NPi2020_1600_fig6_res <- filter(NPi2020_1600_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_50") %>% mutate(sector="Residential buildings")
NPi2020_1000_fig6_res <- filter(NPi2020_1000_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_66") %>% mutate(sector="Residential buildings")
NPi2020_400_fig6_res <- filter(NPi2020_400_i_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="1.5C_50") %>% mutate(sector="Residential buildings")
Fig6_res <- rbind(NoPolicy_fig6_res, NPi_fig6_res) %>% 
            rbind(INDCi_fig6_res) %>% 
            rbind(INDC2030i_1000_fig6_res) %>% 
            rbind(NPi2020_1600_fig6_res) %>% 
            rbind(NPi2020_1000_fig6_res) %>% 
            rbind(NPi2020_400_fig6_res)
Fig6_res <- mutate(Fig6_res, variable="Final Energy|Residential buildings|Non-fossil share")
Fig6_res <- select(Fig6_res, year, region, value, unit, scenario, sector, variable)

NoPolicy_fig6_transport <- filter(NoPolicy_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="No policy") %>% mutate(sector="Transport")
NPi_fig6_transport  <- filter(NPi_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="National policies") %>% mutate(sector="Transport")
INDCi_fig6_transport <- filter(INDCi_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="NDC") %>% mutate(sector="Transport")
INDC2030i_1000_fig6_transport <- filter(INDC2030i_i_1000_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_66 (2030)") %>% mutate(sector="Transport")
NPi2020_1600_fig6_transport <- filter(NPi2020_1600_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_50") %>% mutate(sector="Transport")
NPi2020_1000_fig6_transport <- filter(NPi2020_1000_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_66") %>% mutate(sector="Transport")
NPi2020_400_fig6_transport <- filter(NPi2020_400_i_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="1.5C_50") %>% mutate(sector="Transport")
Fig6_transport <- rbind(NoPolicy_fig6_transport, NPi_fig6_transport) %>% 
  rbind(INDCi_fig6_transport) %>% 
  rbind(INDC2030i_1000_fig6_transport) %>% 
  rbind(NPi2020_1600_fig6_transport) %>% 
  rbind(NPi2020_1000_fig6_transport) %>% 
  rbind(NPi2020_400_fig6_transport)
Fig6_transport <- mutate(Fig6_transport, variable="Final Energy|Transport|Non-fossil share")
Fig6_transport <- select(Fig6_transport, year, region, value, unit, scenario, sector, variable)
data_figure6 <- rbind(Fig6_elec, Fig6_res) %>% rbind(Fig6_transport)
data_figure6 <- mutate(data_figure6, source="PBL")
data_figure6 <- mutate(data_figure6, statistic="value")
data_figure6 <- select(data_figure6, variable, scenario, region, unit, source, statistic, year, value)
data_figure6 <- spread(data_figure6, key=year, value=value)
data_figure6 <- filter(data_figure6, region %in% regions_indicators_IMAGE)
write.table(data_figure6 , file="Indicators/data/stocktake_tool/figure6.csv", sep=";", row.names = FALSE)

# Figure 7 is carbon investments, in 'Data_Low-carbon-investment-gap_v2.xlsx'
# FIND Excel file where data is transformed

# Data Figure 7 -----------------------------------------------------------


# FIGURE 8
data_figure8 <- filter(all_cd_links, Scope=="global", scenario %in% scens_indicators, region %in% regions_indicators, variable%in%c("Emissions|Sulfur", "Emissions|OC", "Emissions|BC"))
d_cd_links_Air_pollution <- filter(data_figure8, year>=2010, year<=2050)
d_cd_links_Air_pollution_stat <- group_by(d_cd_links_Air_pollution, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
d_cd_links_Air_pollution_stat <- gather(d_cd_links_Air_pollution_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_cd_links_Air_pollution_stat <- spread(d_cd_links_Air_pollution_stat, key=year, value=value)
d_cd_links_Air_pollution_stat <- mutate(d_cd_links_Air_pollution_stat, source="CD-LINKS")
d_cd_links_Air_pollution_stat <- select(d_cd_links_Air_pollution_stat, variable, scenario, region, unit, source, statistic, everything())
write.table(d_cd_links_Air_pollution_stat , file="Indicators/data/stocktake_tool/figure8.csv", sep=";", row.names = FALSE)

# Data figure 8 -----------------------------------------------------------

# Data figure 9 ---------------------------------
#FIGURE 9
data_figure9 <- filter(all_cd_links, Scope=="global", value>0, scenario %in% scens_indicators, region %in% regions_indicators, variable%in%c("Land Cover|Forest"))
data_figure9 <- group_by(data_figure9, scenario, model, region, variable) %>% 
                mutate(growth=value-lag(value), perc_growth=(value/lag(value))^(1/5)-1) %>% filter(year>=2010)
d_cd_links_Deforestation <- filter(data_figure9, year>=2010, year<=2050)
d_cd_links_Deforestation_stat <- group_by(d_cd_links_Deforestation, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
d_cd_links_Deforestation_stat <- gather(d_cd_links_Deforestation_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_cd_links_Deforestation_stat <- spread(d_cd_links_Deforestation_stat, key=year, value=value)
d_cd_links_Deforestation_stat <- mutate(d_cd_links_Deforestation_stat, source="CD-LINKS")
d_cd_links_Deforestation_stat <- select(d_cd_links_Deforestation_stat, variable, scenario, region, unit, source, statistic, everything())
write.table(d_cd_links_Deforestation_stat , file="Indicators/data/stocktake_tool/figure9.csv", sep=";", row.names = FALSE)

# Data figure 10 ---------------------------------

# Figure 10 is in ' Data analysis - Final data_update 10.07.2018.xlsx', Data for CD-LINKS project.xlsx'
# FIND Excel file where data is transformed

# Data figure 11 ---------------------------------
# Figure 11 Peak year vs zero emissions year
scens_fig11 <- c('2C_50', '2C_66', '1.5C_50', '2C_66 (2030)')
var_fig11 <- c('Peak year|Kyoto Gases', 'Peak year|CO2', 'Zero Emissions Year|CO2', 'Zero Emissions Year|Kyoto Gases')
d_cd_links_peak_zero <- filter(all_cd_links, value <= 2100, Scope=="global", scenario %in% scens_fig11, region %in% regions_indicators, variable%in%var_fig11)
# peak year and zero emissions year have also different 'period', which makes statistics difficult, set to 2015
d_cd_links_peak_zero$year <- 2015

# Historical data and projectons are not always consistent. The countries with historical peak years have been
#   identified at 'Historical data Figure 11', other countries should have at least a peak year of 2015 or beyond
d_cd_links_peak_zero <- mutate(d_cd_links_peak_zero, value=replace(value, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2') & value<2015, 2015))
# replace peak year for Indonesia (IDN), as land use projections are already very low compared to historical emissions
d_cd_links_peak_zero <- mutate(d_cd_links_peak_zero, value=replace(value, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2') & region == "IDN" & scenario=="2C_50", 2020))
d_cd_links_peak_zero <- mutate(d_cd_links_peak_zero, value=replace(value, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2') & region == "IDN" & scenario=="2C_66", 2020))
d_cd_links_peak_zero <- mutate(d_cd_links_peak_zero, value=replace(value, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2') & region == "IDN" & scenario=="2C_66 (2030)", 2030))
d_cd_links_peak_zero <- mutate(d_cd_links_peak_zero, value=replace(value, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2') & region == "IDN" & scenario=="1.5C_50", 2020))

# calculate statistics
d_cd_links_peak_zero_stat <- group_by(d_cd_links_peak_zero, scenario, region, year, variable, unit) %>% 
                             summarise(mean=mean(value,na.rm=TRUE),
                                       median=median(value,na.rm=TRUE),
                                       min=min(value, na.rm=TRUE),
                                       max=max(value, na.rm=TRUE),
                                       tenp=quantile(value, .10, na.rm=TRUE),
                                       ninetyp=quantile(value, .90, na.rm=TRUE))
d_cd_links_peak_zero_stat$scenario <- factor(d_cd_links_peak_zero_stat$scenario, levels=scens_fig11)
d_cd_links_peak_zero_stat <- gather(d_cd_links_peak_zero_stat, 6:ncol(d_cd_links_peak_zero_stat), key="statistic", value="value") %>%
                             mutate(source="CD-LINKS_global")
write.table(d_cd_links_peak_zero_stat , file="Indicators/data/stocktake_tool/figure11_without_hist.csv", sep=";", row.names = FALSE)

# replace peak year for countries/regions that already peaked before 2015
tmp_peak <- filter(d_cd_links_peak_zero, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2'), 
                   region %in% regions_indicators_peaked)
tmp_peak <- left_join(tmp_peak, peak_historical, by=c('region', 'year', 'variable')) %>%
                      mutate(value=ifelse(value.x<value.y, value.x, value.y)) %>%
                      rename(scenario=scenario.x, unit=unit.x) %>%
                      select(scenario, model, region, year, Scope, value, unit, variable)
tmp_peak$value <- as.numeric(tmp_peak$value)
tmp_peak_stat <- group_by(tmp_peak, scenario, region, year, variable, unit) %>% 
                 summarise(mean=mean(value,na.rm=TRUE),
                           median=mean(value,na.rm=TRUE),
                           min=mean(value,na.rm=TRUE),
                           max=mean(value,na.rm=TRUE),
                           tenp=mean(value,na.rm=TRUE),
                           ninetyp=mean(value,na.rm=TRUE))

tmp_not_peaked_1 <- filter(d_cd_links_peak_zero, variable %in% c('Peak year|Kyoto Gases', 'Peak year|CO2'), 
                                        !(region %in% regions_indicators_peaked))
tmp_not_peaked_2 <- filter(d_cd_links_peak_zero, variable %in% c('Zero Emissions Year|CO2', 'Zero Emissions Year|Kyoto Gases'))
tmp_not_peaked <- rbind(tmp_not_peaked_1, tmp_not_peaked_2) 
tmp_not_peaked_stat <- group_by(tmp_not_peaked, scenario, region, year, variable, unit) %>% 
                       summarise(mean=mean(value,na.rm=TRUE),
                                 median=median(value,na.rm=TRUE),
                                 min=min(value, na.rm=TRUE),
                                 max=max(value, na.rm=TRUE),
                                 tenp=quantile(value, .10, na.rm=TRUE),
                                 ninetyp=quantile(value, .90, na.rm=TRUE))

d_cd_links_peak_zero_inclhist_stat <- rbind(tmp_peak_stat, tmp_not_peaked_stat)
d_cd_links_peak_zero_inclhist_stat$scenario <- factor(d_cd_links_peak_zero_inclhist_stat$scenario, levels=scens_fig11)
d_cd_links_peak_zero_inclhist_stat$variable <- factor(d_cd_links_peak_zero_inclhist_stat$variable, level=var_fig11)
d_cd_links_peak_zero_inclhist_stat <- gather(d_cd_links_peak_zero_inclhist_stat, 6:ncol(d_cd_links_peak_zero_inclhist_stat), key="statistic", value="value") %>%
                                      mutate(source="CD-LINKS_global")
write.table(d_cd_links_peak_zero_inclhist_stat , file="Indicators/data/stocktake_tool/figure11.csv", sep=";", row.names = FALSE)

# Calculate gaps
d1 <- as.data.frame(d_selec_Kyoto) #1
d2_3 <- as.data.frame(d_cd_links_GHG_countries) #2-3
d2_3_CO2 <- filter(d2_3, variable%in%c('Emissions|CO2|AFOLU', 'Emissions|CO2|Excl. AFOLU')) %>%
            spread(key=variable, value=value) %>%
            mutate(`Emissions|CO2`=`Emissions|CO2|AFOLU`+`Emissions|CO2|Excl. AFOLU`) %>%
            gather('Emissions|CO2|AFOLU', 'Emissions|CO2|Excl. AFOLU', 'Emissions|CO2', key=variable, value=value) %>%
            filter(variable=='Emissions|CO2') %>% as.data.frame()
d2_3_nonCO2 <- filter(d2_3, variable%in%c('Emissions|CH4', 'Emissions|N2O', 'Emissions|F-Gases')) %>%
               spread(key=variable, value=value) %>%
               mutate(`Emissions|Non-CO2`=`Emissions|CH4`+`Emissions|N2O`+`Emissions|F-Gases`) %>%
               gather('Emissions|CH4', 'Emissions|N2O', 'Emissions|F-Gases', 'Emissions|Non-CO2', key=variable, value=value) %>%
               filter(variable=='Emissions|Non-CO2') %>% as.data.frame()
d2_3 <- rbind(d2_3, d2_3_CO2) %>% rbind(d2_3_nonCO2)
d4 <- as.data.frame(intensrate) #4
gap5_2C <- filter(data_figure5_model, year>=2010, budget.effort.sharing=="Cost optimal", scenario%in%c('NDC', '2C_66'), budget.scenario%in%c('2C_66')) %>%
         spread(key=scenario, value=value) %>%
         mutate(gap_2C=`NDC`-`2C_66`) %>%
         select(-NDC,-`2C_66`) %>%
         gather('gap_2C', key="gap", value=value)
gap5_1.5C <- filter(data_figure5_model, year>=2010, budget.effort.sharing=="Cost optimal", scenario%in%c('NDC', '1.5C_50'), budget.scenario%in%c('1.5C_50')) %>%
             spread(key=scenario, value=value) %>%
             mutate(gap_1.5C=`NDC`-`1.5C_50`) %>%
             select(-NDC,-`1.5C_50`) %>%
             gather('gap_1.5C', key="gap", value=value)
gap5 <- rbind(gap5_2C, gap5_1.5C) %>% 
      select(-budget.effort.sharing, -budget.scenario, -Budget) %>%
      as.data.frame()
gap5$variable=str_replace_all(gap5$variable,"Emissions\\|CO2","Carbon budget")
d6 <- select(data_figure6, -source, -statistic) %>% 
      mutate(model="IMAGE 3.0") %>% 
      select(variable, scenario, region, unit, model, everything())
d6 <- gather(d6, 6:ncol(d6), key="year", value=value)
d6 <- as.data.frame(d6)
d6$region=str_replace_all(d6$region,"INDIA","IND")
d7 <- read.table("Indicators/data/stocktake_tool/data_figure7.csv", header=TRUE, sep=";")
d7 <- gather(d7, 'Total.energy.investment', 'Low.carbon.investment', key='variable', value=value)
d7$variable=str_replace_all(d7$variable,"\\."," ")
d7$scenario=str_replace_all(d7$scenario,"CPol","National policies")
d7$scenario=str_replace_all(d7$scenario,"2C","2C_66")
d7$scenario=str_replace_all(d7$scenario,"1.5C","1.5C_50")
d7 <- as.data.frame(d7) #4
gaps <- rbind(d1,d2_3) %>% rbind(d4) %>% rbind(d6) %>% rbind(d7)
gaps <- filter(gaps, region%in%c('CHN', 'USA', 'EU','IND', 'World'), scenario%in%c('NDC', '2C_66', '1.5C_50'))
gaps <- spread(gaps, key=scenario, value=value) %>% 
        mutate(gap_2C=`NDC`-`2C_66`, gap_1.5C=`NDC`-`1.5C_50`) %>%
        select(-NDC,-`1.5C_50`, -`2C_66`) %>%
        gather('gap_2C', 'gap_1.5C', key="gap", value=value)
gaps <- rbind(gaps, gap5)
gaps_stat <- group_by(gaps, region, year, unit, variable, gap) %>%
             summarise(median=median(value, na.rm=T), perc_10=quantile(value, 0.1, na.rm=T), perc_90=quantile(value, 0.9, na.rm=T))
write.table(gaps_stat, "Indicators/data/stocktake_tool/gaps_stat.csv", sep=";", row.names=F)

