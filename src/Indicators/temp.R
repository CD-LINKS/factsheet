# GDP settings for figure 4
GDP_MER_PPP <- "PPP"

#adjust names for CD-LINKS data
all_cd_links <- all_paper

# changes: only global models, Selection of Categories, change Category->scenario, period->year, remove Baseline
all_cd_links <- filter(all_cd_links, Scope=="global")
all_cd_links <- select(all_cd_links, -Scope)
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
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000 (2030)", "2C_66_2030")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1000", "2C_66")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 1600", "2C_50")
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Carbon budget 400", "1.5C_50")

all_scens <- unique(all_cd_links$scenario)
all_cd_links$scenario <- factor(all_cd_links$scenario, levels=all_scens)

# change unit names
all_cd_links$scenario=str_replace_all(all_cd_links$scenario,"Mt CO2-equiv/yr", "Mt CO2eq/yr")

# !!!!!!! CHECK !!!!!!! Contains scenarios 2030_high and 2030_verylow. Is this not changed in main_WP2_3_indicators.R

# General settings --------------------------------------------------------


# general settings
models_global <- c("AIM/CGE", "COPPE-COFFEE 1.0", "DNE21+ V.14", "GEM-E3", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "POLES CDL", "REMIND-MAgPIE 1.7-3.0", "WITCH2016")
scens <- c("No new policies", "National policies", "NDC", "2C_50", "2C_66", "1.5C_50", "2C_66 (2030)")
regions <- c("BRA", "CHN", "EU", "IND", "JPN", "RUS", "USA", "World", "ROW")
years <- c('2005', '2010', '2015', '2020', '2025', '2030', '2035', '2040', '2045', '2050', '2055', '2060', '2065', '2070', '2075', '2080', '2085', '2090', '2095', '2100')
stats <- c('mean', 'median', 'min', 'max', 'tenp', 'ninetyp')

regions_IMAGE <- c("BRA", "CHN", "EU", "INDIA", "JAP", "RUS", "USA", "World", "ROW", "Bunkers")

GWPCH4 = 25
GWPN2O = 298

# Historical data PRIMAP and EDGAR --------------------------------------------------


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

# Historical data from EDGAR
# http://edgar.jrc.ec.europa.eu/overview.php?v=432_GHG&SECURE=123
EDGAR <- read.csv("data/EDGAR.csv", header=TRUE, sep=";")
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
write.table(PRIMAP_selec_Kyoto_output, file="Indicators/data/History_Kyoto.csv", sep=";", row.names = FALSE)

# Historical data Figure 2, 3 ------------------------------------------


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
write.table(PRIMAP_selec_CO2, file="Indicators/data/History_CO2.csv", sep=";", row.names = FALSE)

PRIMAP_selec_Agriculture_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT4", entity=="CO2")
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_Agriculture_CO2 <- cbind(PRIMAP_selec_Agriculture_CO2[1:5], PRIMAP_selec_Agriculture_CO2[, 6:ncol(PRIMAP_selec_Agriculture_CO2)]/1000)
PRIMAP_selec_Agriculture_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_Agriculture_CO2 <- data.frame(PRIMAP_selec_Agriculture_CO2)
PRIMAP_selec_Agriculture_CO2 <- mutate(PRIMAP_selec_Agriculture_CO2, variable="Emissions|CO2|Agriculture") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_Agriculture_CO2) = gsub("X", "", colnames(PRIMAP_selec_Agriculture_CO2))
PRIMAP_selec_Agriculture_CO2 <- select(PRIMAP_selec_Agriculture_CO2, -category, -entity)
write.table(PRIMAP_selec_Agriculture_CO2, file="Indicators/data/History_CO2_Agriculture.csv", sep=";", row.names = FALSE)

PRIMAP_selec_LULUCF_CO2 <- filter(PRIMAP_Fig2_3, category=="CAT5", entity=="CO2")
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_LULUCF_CO2 <- cbind(PRIMAP_selec_LULUCF_CO2[1:5], PRIMAP_selec_LULUCF_CO2[, 6:ncol(PRIMAP_selec_LULUCF_CO2)]/1000)
PRIMAP_selec_LULUCF_CO2$unit <- "Mt CO2eq/yr"
PRIMAP_selec_LULUCF_CO2 <- data.frame(PRIMAP_selec_LULUCF_CO2)
PRIMAP_selec_LULUCF_CO2 <- mutate(PRIMAP_selec_LULUCF_CO2, variable="Emissions|CO2|LULUCF") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_LULUCF_CO2) = gsub("X", "", colnames(PRIMAP_selec_LULUCF_CO2))
PRIMAP_selec_LULUCF_CO2 <- select(PRIMAP_selec_LULUCF_CO2, -category, -entity)
write.table(PRIMAP_selec_LULUCF_CO2, file="Indicators/data/History_CO2_LULUCF.csv", sep=";", row.names = FALSE)

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
write.table(PRIMAP_selec_AFOLU_CO2, file="Indicators/data/History_CO2_AFOLU.csv", sep=";", row.names = FALSE)

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
write.table(PRIMAP_selec_CO2_Excl_AFOLU_CO2, file="Indicators/data/History_CO2_ExclAFOLU.csv", sep=";", row.names = FALSE)


PRIMAP_selec_CH4 <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="CH4")
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_CH4 <- cbind(PRIMAP_selec_CH4[1:5], GWPCH4*PRIMAP_selec_CH4[, 6:ncol(PRIMAP_selec_CH4)]/1000)
PRIMAP_selec_CH4$unit <- "Mt CO2eq/yr"
PRIMAP_selec_CH4 <- data.frame(PRIMAP_selec_CH4)
PRIMAP_selec_CH4 <- mutate(PRIMAP_selec_CH4, variable="Emissions|CH4") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_CH4) = gsub("X", "", colnames(PRIMAP_selec_CH4))
PRIMAP_selec_CH4 <- select(PRIMAP_selec_CH4, -category, -entity)
write.table(PRIMAP_selec_CH4, file="Indicators/data/History_CH4.csv", sep=";", row.names = FALSE)

PRIMAP_selec_N2O <- filter(PRIMAP_Fig2_3, category=="CAT0", entity=="N2O")
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_N2O <- cbind(PRIMAP_selec_N2O[1:5], GWPN2O*PRIMAP_selec_N2O[, 6:ncol(PRIMAP_selec_N2O)]/1000)
PRIMAP_selec_N2O$unit <- "Mt CO2eq/yr"
PRIMAP_selec_N2O <- data.frame(PRIMAP_selec_N2O)
PRIMAP_selec_N2O <- mutate(PRIMAP_selec_N2O, variable="Emissions|N2O") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_N2O) = gsub("X", "", colnames(PRIMAP_selec_N2O))
PRIMAP_selec_N2O <- select(PRIMAP_selec_N2O, -category, -entity)
write.table(PRIMAP_selec_N2O, file="Indicators/data/History_N2O.csv", sep=";", row.names = FALSE)

PRIMAP_selec_FGases <- filter(PRIMAP_Fig2_3, category=="CAT0", entity==ifelse(GWPCH4==25, "FGASESAR4", "FGASES"))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, scenario, region, category, entity, unit, num_range("X", 1970:2015))
PRIMAP_selec_FGases <- cbind(PRIMAP_selec_FGases[1:5], PRIMAP_selec_FGases[, 6:ncol(PRIMAP_selec_FGases)]/1000)
PRIMAP_selec_FGases$unit <- "Mt CO2eq/yr"
PRIMAP_selec_FGases <- data.frame(PRIMAP_selec_FGases)
PRIMAP_selec_FGases <- mutate(PRIMAP_selec_FGases, variable="Emissions|F-Gases") %>% select(variable, scenario, region, category, entity, unit, everything())
colnames(PRIMAP_selec_FGases) = gsub("X", "", colnames(PRIMAP_selec_FGases))
PRIMAP_selec_FGases <- select(PRIMAP_selec_FGases, -category, -entity)
write.table(PRIMAP_selec_FGases, file="Indicators/data/History_FGases.csv", sep=";", row.names = FALSE)

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
write.table(filter(PRIMAP_selec_Gases_output, variable!="Emissions|Kyoto Gases"), file="Indicators/data/History_fig2.csv", sep=";", row.names = FALSE)
write.table(PRIMAP_selec_Gases_output, file="Indicators/data/History_fig3.csv", sep=";", row.names = FALSE)

# Historical data Figure 4------------------------------------------
Kyoto_hist <- gather(PRIMAP_selec_Kyoto, 7:ncol(PRIMAP_selec_Kyoto), key="year", value=value)
Kyoto_hist <- filter(Kyoto_hist, year>=1990, year<=2015)
Kyoto_hist$year <- as.numeric(Kyoto_hist$year)
Kyoto_hist$region <- factor(Kyoto_hist$region, levels=regions)
Kyoto_hist <- select(Kyoto_hist, -source, -statistic)
#CO2_hist <- gather(PRIMAP_selec_CO2, 5:ncol(PRIMAP_selec_CO2), key="year", value=value)
PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers <- filter(PRIMAP_selec_CO2_Excl_AFOLU_CO2, region!="Bunkers")
CO2_hist <- gather(PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers, 5:ncol(PRIMAP_selec_CO2_Excl_AFOLU_CO2_bunkers), key="year", value=value)
CO2_hist <- filter(CO2_hist, year>=1990, year<=2015)
CO2_hist$year <- as.numeric(CO2_hist$year)
CO2_hist$region <- factor(CO2_hist$region, levels=regions)
GHG_hist <- rbind(Kyoto_hist, CO2_hist)
# Use OECD GDP history from IMAGE model (million $US2010)
GDP_MER_IMAGE <- NoPolicy$GDP_MER
GDP_MER_IMAGE$region=str_replace_all(GDP_MER_IMAGE$region,"INDIA", "IND")
GDP_MER_IMAGE$region=str_replace_all(GDP_MER_IMAGE$region,"JAP", "JPN")
GDP_MER_hist <- filter(GDP_MER_IMAGE, year>=1990, year<=2015, region %in% regions)
GDP_MER_hist$region <- factor(GDP_MER_hist$region, levels=regions)
# Use World Bank GDP (PPP) SSP2 database (billion $US2005 converted to million $US2010)
# https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=30
# See "GDP PPP from SSP database.xlsx"
GDP_PPP_hist <- read.table("Indicators/data/GDP_PPP_SSP_hist.csv", header=TRUE, sep=";")
#GDP_PPP_hist <- read.table("Indicators/data/GDP_PPP_WDI_hist.csv", header=TRUE, sep=";")
colnames(GDP_PPP_hist) = gsub("X", "", colnames(GDP_PPP_hist))
GDP_PPP_hist <- gather(GDP_PPP_hist, 3:ncol(GDP_PPP_hist), key="year", value=value)
#GDP_PPP_hist$value <- 1107.74*GDP_PPP_hist$value #convert to million $2010 dollars
# https://stats.areppim.com/calc/calc_usdlrxdeflator.php
GDP_PPP_hist$value <- 0.98*1000*GDP_PPP_hist$value #convert to million $2010 dollars
GDP_PPP_hist$unit <- "billion US$2010/yr"
GDP_PPP_hist$year <- as.numeric(GDP_PPP_hist$year)

if(GDP_MER_PPP=="PPP") { GHG_intensity_hist <- inner_join(GHG_hist, GDP_PPP_hist, by=c('year', 'region'))
                         #GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|Kyoto Gases", "variable"] <- "GHG Intensity of GDP|PPP"
                         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|CO2|Excl. AFOLU", "variable"] <- "Carbon Intensity of GDP|PPP"
} else { GHG_intensity_hist <- inner_join(GHG_hist, GDP_MER_hist, by=c('year', 'region'))
         GHG_intensity_hist[GHG_intensity_hist[,"variable"]=="Emissions|CO2|Excl. AFOLU"", "variable"] <- "Carbon Intensity of GDP|MER"
}
GHG_intensity_hist <- mutate(GHG_intensity_hist, value=value.x/value.y)
GHG_intensity_hist$scenario <- "History"
GHG_intensity_hist <- mutate(GHG_intensity_hist, unit="Mt CO2eq/million US($2010)")
GHG_intensity_hist$source <- "PRIMAP, World Bank"
GHG_intensity_hist$statistic <- "value"
GHG_intensity_hist <- select(GHG_intensity_hist, variable, scenario, region, unit, source, statistic, year, value)
write.table(GHG_intensity_hist, "Indicators/data/GHG_intensity_hist.csv", sep=";", row.names=F)

d_selec_Kyoto_stat <- gather(d_selec_Kyoto_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)

# annual intensity improvement
GHG_intensity_rate_annual_hist <- group_by(GHG_intensity_hist, variable, scenario, region, unit, source) %>% 
                                  mutate(value=value/lag(value)-1) %>%
                                  select(variable, scenario, region, unit, source, statistic, year, value) %>%
                                  filter(year>=1980)
GHG_intensity_rate_annual_hist <- filter(GHG_intensity_rate_annual_hist, year>=1990)
GHG_intensity_rate_annual_hist$value <- 100*GHG_intensity_rate_annual_hist$value
GHG_intensity_rate_annual_hist <- spread(GHG_intensity_rate_annual_hist, key='year', value=value)
write.table(GHG_intensity_rate_annual_hist, file="Indicators/data/History_GHG_Intensity_improvement_1yrperiod.csv", sep=";", row.names = FALSE)

# annual intensity improvement based on 5-year periods
GHG_intensity_5yr_hist <- filter(GHG_intensity_hist, year %in% c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015))
write.table(GHG_intensity_5yr_hist, "Indicators/data/GHG_intensity_hist_5y.csv", sep=";", row.names=F)
GHG_intensity_rate_annual5yrperiod_hist <- group_by(GHG_intensity_5yr_hist, variable, scenario, region, unit, source) %>% 
                                           mutate(value=(value/lag(value))^(1/5)-1) %>%
                                           select(variable, scenario, region, unit, source, statistic, year, value) %>%
                                           filter(year>=1980)
GHG_intensity_rate_annual5yrperiod_hist <- filter(GHG_intensity_rate_annual5yrperiod_hist, year>=1990)
GHG_intensity_rate_annual5yrperiod_hist$value <- 100*GHG_intensity_rate_annual5yrperiod_hist$value
GHG_intensity_rate_annual5yrperiod_hist <- spread(GHG_intensity_rate_annual5yrperiod_hist, key='year', value=value)
write.table(GHG_intensity_rate_annual5yrperiod_hist, file="Indicators/data/History_GHG_Intensity_improvement_5yrperiod.csv", sep=";", row.names = FALSE)

# Figure 1 ----------------------------------------------------------------
# Emissions gap

# Data FIGURE 1
# Create GHG emissions data, median, min, max
# 1 data
d_selec_Kyoto <- filter(all_cd_links, scenario %in% scens, region %in% regions, year>=2010, year<=2050, variable=="Emissions|Kyoto Gases")
d_selec_Kyoto_stat <- group_by(d_selec_Kyoto, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                              median=median(value,na.rm=TRUE),
                                                                                              min=min(value, na.rm=TRUE),
                                                                                              max=max(value, na.rm=TRUE),
                                                                                              tenp=quantile(value, .10, na.rm=TRUE),
                                                                                              ninetyp=quantile(value, .90, na.rm=TRUE))
d_selec_Kyoto_stat <- gather(d_selec_Kyoto_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_selec_Kyoto_stat <- spread(d_selec_Kyoto_stat, key=year, value=value)
d_selec_Kyoto_stat <- mutate(d_selec_Kyoto_stat, source="CD-LINKS")
d_selec_Kyoto_stat <- ungroup(d_selec_Kyoto_stat)
d_selec_Kyoto_stat <- select(d_selec_Kyoto_stat, variable, scenario, region, unit, source, statistic, everything())
write.table(d_selec_Kyoto_stat, file="Indicators/data/Figure1_Kyoto.csv", sep=";", row.names = FALSE)

data_fig1_proj <- gather(d_selec_Kyoto_stat, 7:ncol(d_selec_Kyoto_stat), key="year", value=value)
data_fig1_hist <- gather(PRIMAP_selec_Kyoto_output, 7:ncol(PRIMAP_selec_Kyoto_output), key="year", value=value)
data_fig1 <- rbind(data_fig1_hist, data_fig1_proj)
#q <- ggplot(data=data_fig1) + geom_line(aes(year, value, colour=region, group=interaction(scenario, statistic)))
#q1_1 <- ggplot(data=filter(data_fig1, statistic=="median")) +  
#  geom_line(aes(year, value, colour=region, linetype=scenario,  group=interaction(scenario, region))) + 
#  theme_bw()
#, size=statistic, shape=region
#plot(q1_1)

#q1_2 <- ggplot(data=filter(data_fig1, statistic=="median")) +  
#     geom_line(aes(year, value, colour=region, linetype=scenario,  group=interaction(scenario, region))) + 
#     scale_colour_manual(values=c(rhg_cols[1:9])) +
#     theme_bw()
#, size=statistic, shape=region
#plot(q1_2)
#ggplotly(q1_2)



#TODO read efforst sharing
# EffortSharingEmissions <- readWorksheetFromFile("Indicators/data/Effort sharing data_IMAGE_adjusted.xlsx", 
#                             sheet="emissions_AP_2C", 
#                             startRow = 2,
#                             endCol = 36)
# EffortSharingEmissions <- select(EffortSharingEmissions, -starts_with("class_"))
# EffortSharingEmissions <- mutate()
# TO DO: afmaken

# Figure 2 and 3 ----------------------------------------------------------
# 2. Emissions per country
# 3. Emissions per GHG

#  Data FIGURE 2 and 3 (bar chart country split up and ghg emissions split up)
# including rest of the world category
GHG_fig3 <- c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")
d_cd_links_GHG_countries <- filter(all_cd_links, scenario %in% scens, region %in% regions, variable %in% GHG_fig3)
d_bunkers_fig3 <- filter(all_cd_links, region=="Bunkers", variable=="Emissions|Kyoto Gases")
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
d_cd_links_GHG_countries <- rbind(d_cd_links_GHG_countries, d_cd_links_CO2_exclAFOLU)
d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, variable!="Emissions|CO2")
d_cd_links_GHG_countries <- arrange(d_cd_links_GHG_countries, scenario, region, year, variable, model)
write.table(d_cd_links_GHG_countries, file="Indicators/data/temp2.csv", sep=";", row.names = FALSE)
d_cd_links_GHG_countries <- filter(d_cd_links_GHG_countries, year>=2010, year<=2050)
d_cd_links_GHG_countries_stat <- group_by(d_cd_links_GHG_countries, scenario, region, year, variable, unit) %>% summarise(median=median(value,na.rm=TRUE))
                                                                                                                                 #mean=mean(value,na.rm=TRUE),
                                                                                                                                 #min=min(value, na.rm=TRUE),
                                                                                                                                 #max=max(value, na.rm=TRUE),
                                                                                                                                 #tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 #ninetyp=quantile(value, .90, na.rm=TRUE)
                                                                                                                                 #)
#d_cd_links_GHG_countries_stat <- gather(d_cd_links_GHG_countries_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
d_cd_links_GHG_countries_stat <- gather(d_cd_links_GHG_countries_stat, 'median', key='statistic', value=value)
d_cd_links_GHG_countries_stat <- as.data.frame(d_cd_links_GHG_countries_stat)
d_cd_links_GHG_countries_stat$region <- factor(d_cd_links_GHG_countries_stat$region, levels=regions)
# determine RoW category
Countries_Gases <- d_cd_links_GHG_countries_stat %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases <- group_by(Countries_Gases, scenario, year, variable, unit, statistic) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases <- mutate(RoW_Gases, region="ROW") %>% select(scenario, region, year, variable, unit, statistic, value)
RoW_Gases <- as.data.frame(RoW_Gases)
d_cd_links_GHG_countries_stat <- rbind(d_cd_links_GHG_countries_stat, RoW_Gases)

d_cd_links_GHG_excl_Kyoto_countries_stat <- spread(d_cd_links_GHG_countries_stat, key=year, value=value)
d_cd_links_GHG_excl_Kyoto_countries_stat <- mutate(d_cd_links_GHG_excl_Kyoto_countries_stat, source="CD-LINKS")
d_cd_links_GHG_excl_Kyoto_countries_stat <- select(d_cd_links_GHG_excl_Kyoto_countries_stat, variable, scenario, region, unit, source, statistic, everything())
d_cd_links_GHG_countries_stat_fig2 <- spread(d_cd_links_GHG_countries_stat, key="year", value=value)
write.table(d_cd_links_GHG_countries_stat_fig2 , file="Indicators/data/figure2.csv", sep=";", row.names = FALSE)

# determine Kyoto RoW category
Countries_Gases_Kyoto <- gather(d_selec_Kyoto_stat, 7:ncol(d_selec_Kyoto_stat), key="year", value=value)
Countries_Gases_Kyoto <- Countries_Gases_Kyoto %>% mutate(value2=ifelse(region!="World", -1*value, value)) %>% select(-value) %>% rename(value=value2)
RoW_Gases_Kyoto <- group_by(Countries_Gases_Kyoto, scenario, year, variable, unit, statistic) %>% summarise(value=sum(value,na.rm=TRUE))
RoW_Gases_Kyoto <- mutate(RoW_Gases_Kyoto, region="ROW") %>% select(scenario, region, year, variable, unit, statistic, value) %>% mutate(source="CD-LINKS")
RoW_Gases_Kyoto <- as.data.frame(RoW_Gases_Kyoto)
RoW_Gases_Kyoto <- spread(RoW_Gases_Kyoto, key="year", value=value)
d_selec_Kyoto_stat <- rbind(d_selec_Kyoto_stat, RoW_Gases_Kyoto)

# add total kyoto emissions
d_selec_Kyoto_median <- filter(d_selec_Kyoto_stat, statistic=="median")
d_cd_links_GHG_countries_stat <- rbind(d_cd_links_GHG_excl_Kyoto_countries_stat, d_selec_Kyoto_median)
d_cd_links_GHG_countries_stat$region <- factor(d_cd_links_GHG_countries_stat$region, level=regions)
d_cd_links_GHG_countries_stat <- arrange(d_cd_links_GHG_countries_stat, variable, scenario, region)
#d_cd_links_GHG_countries_stat_fig3 <- spread(d_cd_links_GHG_countries_stat, key="year", value=value)
write.table(d_cd_links_GHG_countries_stat, file="Indicators/data/figure3.csv", sep=";", row.names = FALSE)
write.table(d_cd_links_GHG_countries_stat, file="Indicators/data/figure2_3.csv", sep=";", row.names = FALSE)




#  Data Figure 4 ---------------------------------------------------------------
# Decarbonisation rate 

if (GDP_MER_PPP = "PPP") { intens=filter(all_cd_links, variable%in%c("Carbon Intensity of GDP|PPP","GHG Intensity of GDP|PPP"),
                          scenario %in% scens, region %in% regions, region!="Bunkers")
} else {intens=filter(all_cd_links, variable%in%c("Carbon Intensity of GDP|MER","GHG Intensity of GDP|MER"),
                      scenario %in% scens, region %in% regions, region!="Bunkers")
}
# delete 2005 MESSage values (previous: Add 2005 data to MESSAGE (median of other models))
intens=filter(intens, !(year==2005 & model=="MESSAGEix-GLOBIOM_1.0"))
intens[intens[,"year"]==2005 & intens[,"model"]=="MESSAGEix-GLOBIOM_1.0", "value"] <- 0
intens <- arrange(intens, variable, scenario, model, region, year)
write.table(intens,"Indicators/data/figure4_GHG_intensity.csv", sep=";", row.names = FALSE)
intens_stat <- group_by(intens, scenario, region, year, variable, unit) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                       median=median(value,na.rm=TRUE),
                                                                                       min=min(value, na.rm=TRUE),
                                                                                       max=max(value, na.rm=TRUE),
                                                                                       tenp=quantile(value, .10, na.rm=TRUE),
                                                                                       ninetyp=quantile(value, .90, na.rm=TRUE))
write.table(intens_stat, "Indicators/data/GHG_intensity_stat.csv", sep=";", row.names=F)

# calculate annual rate
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
write.table(intensrate_stat,"Indicators/data/figure4_CintensityImprovement_range.csv", sep=";", row.names = FALSE)


#  Data Figure 5 ----------------------------------------------------------------
# Depletetion of carbon budget

regions_PRIMAP_history <- c("BRA", "CHN", "EU28", "IND", "JPN", "RUS", "USA", "CAN", "TUR", "EARTH")
start_year_projections_fig5=2010
end_year_projections_fig5=2050
# Three steps 1) determine historical emissions (per region) for 1850-2015, 2) determine projections 3) In Excel, determine budgets
#1. Historical cumulative emissions 1990-2015
#tmp_data <- (PRIMAP_selec_CO2_1850_2015, 7:ncol(PRIMAP_selec_CO2_1850_2015),key='year',value=value)
# determine cumulative budgets for historic periods 1850-1990 and 1990-2015
# make dataframe structure for CO2 emissions equal to data used for figure
# 1850-1989
PRIMAP_selec_CO2_1850_2015 <- filter(PRIMAP, country %in% regions_PRIMAP_history, category=="CAT0", entity=="CO2")
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, scenario, country, category, entity, unit, num_range("X", 1850:start_year_projections_fig5))
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
PRIMAP_selec_CO2_1850_2015 <- select(PRIMAP_selec_CO2_1850_2015, variable, scenario, region, unit, source, statistic, num_range("X", 1850:start_year_projections_fig5))
colnames(PRIMAP_selec_CO2_1850_2015) = gsub("X", "", colnames(PRIMAP_selec_CO2_1850_2015))

#2. Remaining emissions depending on budget (400, 1000, 1600)
d_cd_links_CO2 <- select(all_cd_links, scenario, model, region, year, value, unit, variable)
d_cd_links_CO2 <- filter(d_cd_links_CO2, scenario %in% scens, region %in% regions, variable=="Emissions|CO2")
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
data_figure5_CO2emissions_model$region <- factor(data_figure5_CO2emissions_model$region, levels=regions)
data_figure5_CO2emissions_model$scenario <- factor(data_figure5_CO2emissions_model$scenario, levels=scens)

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

#4 Export to Excel (change later to R code) to translate annual CO2 emissions into budgets
write.table(data_figure5_CO2emissions_model , file="Indicators/data/figure5_CO2emissions_model.csv", sep=";", row.names = FALSE) 
write.table(data_figure5_CO2emissions_stat , file="Indicators/data/figure5_CO2emissions_stat.csv", sep=";", row.names = FALSE)
write.table(data_figure5_CO2budget_stat , file="Indicators/data/figure5_CO2budget_stat.csv", sep=";", row.names = FALSE)

#5 Import again in R (effort sharing)
data_figure5_model <- read.csv("Indicators/data/data_figure5_ToR.csv", header=TRUE, sep=";")
data_figure5_stat <- data_figure5_model
colnames(data_figure5_stat) = gsub("X", "", colnames(data_figure5_stat))
data_figure5_stat <- gather(data_figure5_stat, 10:ncol(data_figure5_stat), key="year", value=value)
data_figure5_stat$value <- as.numeric(data_figure5_stat$value)
data_figure5_stat <- group_by(data_figure5_stat, variable, budget.scenario, budget.effort.sharing, scenario, region, unit, year) %>% summarise(mean=mean(value,na.rm=TRUE),
                                                                                                                                 median=median(value,na.rm=TRUE),
                                                                                                                                 min=min(value, na.rm=TRUE),
                                                                                                                                 max=max(value, na.rm=TRUE),
                                                                                                                                 tenp=quantile(value, .10, na.rm=TRUE),
                                                                                                                                 ninetyp=quantile(value, .90, na.rm=TRUE))
data_figure5_stat <- gather(data_figure5_stat, 'mean', 'median', 'min', 'max', 'tenp', 'ninetyp', key='statistic', value=value)
#data_figure5_stat <- filter(data_figure5_stat, year>=1990, year<=2050)
data_figure5_stat$value <- as.numeric(data_figure5_stat$value)
data_figure5_stat <- spread(data_figure5_stat, key=year, value=value)
data_figure5_stat$statistic <- factor(data_figure5_stat$statistic, level=stats)
colnames(data_figure5_stat) = gsub("X", "", colnames(data_figure5_stat))
data_figure5_stat_fig <- select(data_figure5_stat, -(num_range("", 1850:1989)))
write.table(data_figure5_stat_fig , file="Indicators/data/figure5_stat.csv", sep=";", row.names = FALSE)

# plot
#data_figure5_plot <- gather(data_figure5, 6:ncol(data_figure5), key="year", value=value)
#data_figure5_plot <- filter(data_figure5_plot, region=="World", Category=="Carbon budget 1000", year>=start_year_projections_fig5-5, year<=2020)
#ggplot(data_figure5_plot) + geom_point(aes(x=year, y=value, colour=model)) + ylim(0, NA)
       





#  Data Figure 6 -----------------
# Based on PBL IMAGE data, not other models
NoPolicy_fig6_elec <- filter(NoPolicy_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="No policy") %>% mutate(sector="Electricity")
NPi_fig6_elec  <- filter(NPi_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="National policies") %>% mutate(sector="Electricity")
INDCi_fig6_elec <- filter(INDCi_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="NDC") %>% mutate(sector="Electricity")
INDC2030i_1600_fig6_elec <- filter(INDC2030i_1000_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66 (2030)") %>% mutate(sector="Electricity")
NPi2020_1600_fig6_elec <- filter(NPi2020_1000_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_50") %>% mutate(sector="Electricity")
NPi2020_1000_fig6_elec <- filter(NPi2020_1000_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="2C_66") %>% mutate(sector="Electricity")
NPi2020_400_fig6_elec <- filter(NPi2020_400_indicators$NonFossilElecShare, year>=2010, year<=2050) %>% mutate(scenario="1.5C_50") %>% mutate(sector="Electricity")
Fig6_elec <- rbind(NoPolicy_fig6_elec, NPi_fig6_elec) %>% 
             rbind(INDCi_fig6_elec) %>% 
             rbind(INDC2030i_1600_fig6_elec) %>% 
             rbind(NPi2020_1600_fig6_elec) %>% 
             rbind(NPi2020_1000_fig6_elec) %>% 
             rbind(NPi2020_400_fig6_elec)
Fig6_elec <- mutate(Fig6_elec, variable="Final Energy|Electricity|Non-fossil share")
Fig6_elec <- select(Fig6_elec, year, region, value, unit, scenario, sector, variable)

NoPolicy_fig6_res <- filter(NoPolicy_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="No policy") %>% mutate(sector="Residential buildings")
NPi_fig6_res  <- filter(NPi_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="National policies") %>% mutate(sector="Residential buildings")
INDCi_fig6_res <- filter(INDCi_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="NDC") %>% mutate(sector="Residential buildings")
INDC2030i_1600_fig6_res <- filter(INDC2030i_1000_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_66 (2030)", sector="Residential buildings")
NPi2020_1600_fig6_res <- filter(NPi2020_1600_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_50") %>% mutate(sector="Residential buildings")
NPi2020_1000_fig6_res <- filter(NPi2020_1000_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="2C_66") %>% mutate(sector="Residential buildings")
NPi2020_400_fig6_res <- filter(NPi2020_400_indicators$NonFossilResBuildingsShare, year>=2010, year<=2050, population_group=="Total") %>% mutate(scenario="1.5C_50") %>% mutate(sector="Residential buildings")
Fig6_res <- rbind(NoPolicy_fig6_res, NPi_fig6_res) %>% 
            rbind(INDCi_fig6_res) %>% 
            rbind(INDC2030i_1600_fig6_res) %>% 
            rbind(NPi2020_1600_fig6_res) %>% 
            rbind(NPi2020_1000_fig6_res) %>% 
            rbind(NPi2020_400_fig6_res)
Fig6_res <- mutate(Fig6_res, variable="Final Energy|Residential buildings|Non-fossil share")
Fig6_res <- select(Fig6_res, year, region, value, unit, scenario, sector, variable)

NoPolicy_fig6_transport <- filter(NoPolicy_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="No policy") %>% mutate(sector="Transport")
NPi_fig6_transport  <- filter(NPi_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="National policies") %>% mutate(sector="Transport")
INDCi_fig6_transport <- filter(INDCi_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="NDC") %>% mutate(sector="Transport")
INDC2030i_1600_fig6_transport <- filter(INDC2030i_1000_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_66 (2030)") %>% mutate(sector="Transport")
NPi2020_1600_fig6_transport <- filter(NPi2020_1600_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_50") %>% mutate(sector="Transport")
NPi2020_1000_fig6_transport <- filter(NPi2020_1000_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="2C_66") %>% mutate(sector="Transport")
NPi2020_400_fig6_transport <- filter(NPi2020_400_indicators$NonFossilTransportShare, year>=2010, year<=2050, travel_mode=="Total", type=="Total") %>% mutate(scenario="1.5C_50") %>% mutate(sector="Transport")
Fig6_transport <- rbind(NoPolicy_fig6_transport, NPi_fig6_transport) %>% 
  rbind(INDCi_fig6_transport) %>% 
  rbind(INDC2030i_1600_fig6_transport) %>% 
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
data_figure6 <- filter(data_figure6, region %in% regions_IMAGE)
write.table(data_figure6 , file="Indicators/data/figure6.csv", sep=";", row.names = FALSE)

# innovation indicators
#d_fig6 = gather(data_figure6, 7:ncol(data_figure6), key='year', value=value)
#d_fig6$scenario <- factor(d_fig6$scenario, level=scens)
#d_fig6a = filter(d_fig6, region=="CHN", scenario=="NDC")
#ggplot(data=d_fig6a) + geom_line(aes(year, value, group=variable, colour=variable))


#ggplot(data=d_fig6) + geom_line(aes(year, value, group=variable, colour=variable)) + facet_grid(region ~ scenario) +
#  scale_x_discrete(limits = c(2010,2050), breaks=seq(2010,2050,1))

# Kyoto emissions
#ggplot() + geom_line(data=filter(x, region=="EU", main_sector=="Total", GHG_Category=="EMISCO2EQ", year>=2010, year<=2050), aes(year, value, colour=scenario))
#
#x <- filter(all, region=="EU", variable=="Emissions|Kyoto Gases", Scope=="global", 
#            scenario %in% c('INDCi_V3', 'NPi2020_1000_V3'), period>=2010, period<=2030)
#p <- ggplot(x) + geom_line(aes(period, value, colour=model, linetype=scenario, group=interaction(model, scenario))) +
#                 ggtitle("GHG emissions - European Union") + ylab("MtCO2eq")
#p <- ggplot(x) + geom_line(aes(period, value, colour=model, linetype=scenario)) +
#  ggtitle("GHG emissions - European Union") + ylab("MtCO2eq")
#plot(p)
#ggplotly(p, tooltip="all")


# Figure 7 is carbon investments, in 'Data_Low-carbon-investment-gap_v2.xlsx'
# FIND Excel file where data is transformed

# FIGURE 8
data_figure8 <- filter(all_cd_links, scenario %in% scens, region %in% regions, variable%in%c("Emissions|Sulfur", "Emissions|OC", "Emissions|BC"))
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
write.table(d_cd_links_Air_pollution_stat , file="Indicators/data/figure8.csv", sep=";", row.names = FALSE)
  
# FIGURE 9
data_figure9 <- filter(all_cd_links, value>0, scenario %in% scens, region %in% regions, variable%in%c("Land Cover|Forest|Natural Forest"))
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
write.table(d_cd_links_Deforestation_stat , file="Indicators/data/figure9.csv", sep=";", row.names = FALSE)

# Figure 10 is in 'Data for CD-LINKS project.xlsx'
# FIND Excel file where data is transformed
