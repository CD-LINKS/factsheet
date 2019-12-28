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
