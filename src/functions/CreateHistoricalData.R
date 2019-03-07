# This module creates 'all_hist', which has the same structure as 'all'
# All hist contains historical data on GHG emissions, Energy and GDP (incl. CO2-intensity)
#  for which 'Category' is set to 'Historical' and the models to 'PRIMAP', 'IEA' and 'OECD'. 
# For Total Kyoto Emissions bunker emissions from EDGAR are added
# Strucure of code
# 1. Import PRIMAP GHG emissions
#    Create and fill "Emissions" variables
#    "Emissions|Kyoto Gases", "Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", 
#    "Emissions|CO2|AFOLU", "Emissions|N2O","Emissions|CH4","Emissions|F-Gases"
# 2. Import IEA Energy
#    Create and fill "Primary Energy", "Secondary Eenrgy", "Final Energy" variables
# 3. Import OECD GDP (MER)
#    Create and fill "GDP|MER" variables
# 4. Create CO2-intensity

# Not on IEA data
# For electricity and heat the database (IEA-tj-1990-2015-v2.csv") is not always consistent between individual flows/products and totals.
# 1. The sum of all products (e.g. COAL, CRUDEOIL, ) for each flow (ELMAINE, ELMAINC, ELAUTOE, ELAUTOC, HEMAINH, HEMAINC, HEAUTOH, HEAUTOC) 
#    does not always equal  'TOTAL'. This is especially the case for data before 2005.
#    We have used the sum of all individual products, so not the 'TOTAL',
# 2. The sum of (ELMAINE, ELMAINC, ELAUTOE, ELAUTOC) does not always equal ELAUTOUTPUT.
#    We have used the individual (ELMAINE, ELMAINC, ELAUTOE, ELAUTOC) flows, and not ELOUTPUT.

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

#setwd("~/disks/y/Kennisbasis/image/model/Timer/ontwapps_Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet_oct2017/src")
source("functions/HistoricalDataFunctions.R")
source("functions/overwrite.R")
source("functions/calcVariable.R")
all_hist <- ""

# general settings
regions_IAM <- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "CAN", "TUR", "Bunkers", "World")
GWP_CH4 <- 25
GWP_N2O <- 298

# Historical GHG emissions (GgCO2eq = MtCO2e)
#-->"Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|AFOLU", "Emissions|CH4","Emissions|N2O","Emissions|F-Gases"
# Data from PRIMAP
# http://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:2959897
# Emissions|CO2|Energy and Industrial Processes
#   CAT0 = National Total
#   CAT1 = Total energy
#   CAT2 = Total industry
#   CAT3 = Solvent and other product use
# Emissions|CO2|AFOLU
#   CAT4 = Agriculture
#   CAT5 = LULUCF
# GHG emissions
#   KYOTOGHGAR4 = Kyoto gases in GWP AR4
#   CO2
#   CH4
#   N2O
#   FGASES
PRIMAP <- read.csv("data/PRIMAP-hist_v1.2_14-Dec-2017.csv", header=TRUE, sep=",")
PRIMAP_IAM <- PRIMAP
regions_PRIMAP_IAM <- c("BRA", "CHN", "EU28", "IND", "JPN", "RUS", "USA", "CAN", "TUR", "EARTH")
years_CDLINKS_history <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015")
colnames(PRIMAP_IAM)[colnames(PRIMAP_IAM)=="country"] <- "region"
colnames(PRIMAP_IAM)[colnames(PRIMAP_IAM)=="year"] <- "period"
PRIMAP_IAM <- filter(PRIMAP_IAM, region %in% regions_PRIMAP_IAM)
PRIMAP_IAM$region=str_replace_all(PRIMAP_IAM$region,"EARTH","World")
PRIMAP_IAM$region=str_replace_all(PRIMAP_IAM$region,"EU28","EU")

PRIMAP_Kyoto <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "KYOTOGHGAR4", "Emissions|Kyoto Gases")
PRIMAP_CO2 <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "CO2", "Emissions|CO2")
PRIMAP_CO2_energy <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT1", "CO2", "Emissions|CO2|Energy")
PRIMAP_CO2_industry <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT2", "CAT3"), "CO2", "Emissions|CO2|Industrial Processes")
PRIMAP_CO2_energy_industry <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT1", "CAT2", "CAT3"), "CO2", "Emissions|CO2|Energy and Industrial Processes")
PRIMAP_CO2_AFOLU <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT4", "CAT5"), "CO2", "Emissions|CO2|AFOLU")
PRIMAP_CH4 <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "CH4", "Emissions|CH4")
PRIMAP_N2O <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "N2O", "Emissions|N2O")
PRIMAP_FGases <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "FGASESAR4", "Emissions|F-Gases")

# ADD BUNKERS emissions from EDGAR
# Historical data from EDGAR
# http://edgar.jrc.ec.europa.eu/overview.php?v=432_GHG&SECURE=123
EDGAR <- read.csv("data/EDGAR.csv", header=TRUE, sep=";")
EDGAR_bunkers <- filter(EDGAR, Name %in% c('Int. Aviation', 'Int. Shipping'))
colnames(EDGAR_bunkers) = gsub("X", "", colnames(EDGAR_bunkers))
colnames(EDGAR_bunkers)[colnames(EDGAR_bunkers)=="World.Region"] <- "region"
EDGAR_bunkers <- gather(EDGAR_bunkers, 7:ncol(EDGAR_bunkers), key="period", value=value) %>%
  select(-IPCC.Annex, -ISO_A3, -Name, -IPCC, -IPCC_description)
EDGAR_bunkers <- group_by(EDGAR_bunkers, period) %>% summarize(value=sum(value)) %>%
  mutate(region="Bunkers") %>%
  select(period, region, value)
EDGAR_bunkers$value <- 10^-3*EDGAR_bunkers$value
EDGAR_bunkers$unit <- "Mt CO2eq/yr"
EDGAR_bunkers$period <- as.numeric(EDGAR_bunkers$period )
write.table(EDGAR_bunkers, file="data/EDGAR_bunkers.txt", sep=";", row.names=FALSE)
# add missing 2013, 2014, 2015 data
tmp1 <- filter(EDGAR_bunkers, period==2012)
tmp1$period <- 2013
tmp2 <- filter(EDGAR_bunkers, period==2012)
tmp2$period <- 2014
tmp3 <- filter(EDGAR_bunkers, period==2012)
tmp3$period <- 2015
EDGAR_bunkers <- rbind(EDGAR_bunkers, tmp1) %>% rbind(tmp2) %>% rbind(tmp3)
# add bunkers to CO2 andk Kyoto World total
# Total Kyoto
tmp <- filter(PRIMAP_Kyoto, region=="World")
tmp <- left_join(tmp, EDGAR_bunkers, by=c('period')) %>% 
  mutate(value=value.x+value.y) %>%
  select(scenario, Category, Baseline, model, region.x, period, Scope, value, unit.x, variable) %>%
  rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
PRIMAP_Kyoto <- filter(PRIMAP_Kyoto, region!="World")
PRIMAP_Kyoto <- rbind(PRIMAP_Kyoto, tmp)
# CO2
tmp <- filter(PRIMAP_CO2, region=="World")
tmp <- left_join(tmp, EDGAR_bunkers, by=c('period')) %>% 
  mutate(value=value.x+value.y) %>%
  select(scenario, Category, Baseline, model, region.x, period, Scope, value, unit.x, variable) %>%
  rename(region=region.x, unit=unit.x)
tmp <- as.data.frame(tmp)
PRIMAP_CO2 <- filter(PRIMAP_CO2, region!="World")
PRIMAP_CO2 <- rbind(PRIMAP_CO2, tmp)

# Add to all
all_PRIMAP <- rbind(PRIMAP_Kyoto,PRIMAP_CO2) %>% rbind(PRIMAP_CO2_energy) %>% rbind(PRIMAP_CO2_industry) %>% rbind(PRIMAP_CO2_energy_industry) %>% 
              rbind(PRIMAP_CO2_AFOLU) %>% rbind(PRIMAP_CH4) %>% rbind(PRIMAP_N2O) %>% rbind(PRIMAP_FGases)
write.table(PRIMAP_IAM, file="data/PRIMAP_IAM.csv", sep=";", row.names=F)  
all_hist <- all_PRIMAP

# add variables
all_hist <- calcVariable(all_hist,'`Emissions|Kyoto Gases|Excl. AFOLU CO2` ~ (`Emissions|Kyoto Gases`)-(`Emissions|CO2|AFOLU`)' , newUnit='Mt')
all_hist <- calcVariable(all_hist,'`Emissions|Non-CO2` ~ 25*(`Emissions|CH4`)+298*(`Emissions|N2O`)+(`Emissions|F-Gases`)' , newUnit='EJ/$US 2005')

################# IEA ENERGY DATA ##############################

# Impport historical energy data
# IEA codes
IEA_elec_flows <- c("ELMAINE","ELAUTOE", "ELMAINC", "ELAUTOC")
IEA_heat_flows <- c("HEMAINH", "HEMAINC", "HEAUTOH", "HEAUTOC")
IEA_industry_flow <- c("TOTIND", "NONENUSE")
IEA_bunkers_flow <- c("WORLDAV", "WORLDMAR")
IEA_transport_flow <- c("DOMESAIR", "DOMESNAV", "PIPELINE", "RAIL","ROAD","TRNONSPE", IEA_bunkers_flow)
IEA_buildings_flow <- c("RESIDENT", "COMMPUB")
IEA_other_flow <- c("AGRICULT", "FISHING", "ONONSPEC")
IEA_tfc_flow <- c(IEA_industry_flow, IEA_transport_flow, IEA_bunkers_flow, IEA_buildings_flow, IEA_other_flow)
IEA_flows_import <- c("TPES", "TFC", IEA_industry_flow, IEA_bunkers_flow, IEA_transport_flow, IEA_buildings_flow, IEA_other_flow, IEA_elec_flows, IEA_heat_flows)

IEA_coal_product <- c("HARDCOAL",	"BROWN",	"ANTCOAL",	"COKCOAL",	"BITCOAL",	"SUBCOAL",	"LIGNITE",	
                      "PATFUEL",	"OVENCOKE",	"GASCOKE",	"COALTAR",	"BKB",	"GASWKSGS",	"COKEOVGS",	"BLFURGS",	"OGASES")
IEA_crude_oil_product <- c("CRNGFEED",	"CRUDEOIL",	"NGL",	"REFFEEDS",	"ADDITIVE",	"NONCRUDE")
IEA_light_oil_product <- c("REFINGAS",	"ETHANE",	"LPG",	"NONBIOGASO",	"AVGAS",	"JETGAS",	"JETKERO",	"OTHKERO",	
                           "NONBIODIES",	"RESFUEL",	"NAPHTHA",	"WHITESP",	"LUBRIC",	"BITUMEN",	"PARWAX",	"PETCOKE",	
                           "ONONSPEC",	"NONBIOJETK")
IEA_other_ren_product <- c("GEOTHERM", "SOLARPV", "SOLARTH", "WIND", "TIDE", "OTHER")
# biomass include CHARCOAL?
IEA_biomass_product <- c("PRIMSBIO", "CHARCOAL", "BIOGASES", "BIODIESEL", "BIOGASOL", "BIOJETKERO", 
                         "OBIOLIQ", "INDWASTE", "MUNWASTEN", "MUNWASTER")
IEA_ren_products <- c("NUCLEAR", "HYDRO", IEA_other_ren_product, IEA_biomass_product, "ELECTR", "HEAT")
IEA_products <- c(IEA_coal_product, IEA_crude_oil_product, IEA_light_oil_product, "NATGAS", "NUCLEAR", 
                  "HYDRO", IEA_other_ren_product, IEA_biomass_product, "ELECTR", "HEAT")
IEA_EU <- c("AUSTRIA", "BELGIUM", "BULGARIA", "CROATIA", "CYPRUS", "CZECH", "DENMARK", "ESTONIA", "FINLAND", "FRANCE", "GERMANY", "GREECE", "HUNGARY", "IRELAND", "ITALY", 
            "LATVIA", "LITHUANIA", "LUXEMBOU", "MALTA", "NETHLAND", "POLAND", "PORTUGAL", "ROMANIA", "SLOVAKIA", "SLOVENIA", "SPAIN", "SWEDEN", "UK")
IEA_China <- c("CHINA", "HONGKONG", "MONGOLIA", "TAIPEI")
IEA_Bunkers <- c("WORLDAV", "WORLDMAR")
regions_IEA_IAM_import <- c("BRAZIL",  "INDIA", "JAPAN", "RUSSIA", "USA", "CANADA", "TURKEY", "WORLD", IEA_EU, IEA_China, IEA_Bunkers, "WORLDAV", "WORLDMAR")
#regions_IEA_IAM <- c("BRAZIL",  "CHINA+", "EU28", "INDIA", "JAPAN", "RUSSIA", "USA", "CANADA", "TURKEY", "BUNKERS", "WORLD")
regions_IEA_IAM <- c("BRAZIL",  "CHINA+", "EU28", "INDIA", "JAPAN", "RUSSIA", "USA", "CANADA", "TURKEY", "WORLD", "BUNKERS")

# IEA
IEA_energy_import <- read.csv("data/IEA-tj-1990-2015-v2.csv", header=TRUE, sep=",")
IEA_energy <- rename(IEA_energy_import, region=COUNTRY, period=Year, unit=UNIT, value=VALUE) %>%
  select(region, FLOW, PRODUCT, period, value, unit)
# detect missing records and add zero's (necassary for CalVariables function)
IEA_empty <- CreateEmptyIEA_energy(IEA_energy, regions_IEA_IAM_import, IEA_flows_import, c(IEA_products))
IEA_tmp <- anti_join(IEA_empty, IEA_energy, by=c('region', 'FLOW', 'PRODUCT', 'period', 'unit'))
IEA_energy <- rbind(IEA_energy, IEA_tmp)

# Add EU28, CHINA+ and BUNKERS regions
IEA_energy_EU <- filter(IEA_energy, region %in% IEA_EU) %>% group_by(FLOW, PRODUCT, period, unit)
IEA_energy_EU <- summarise(IEA_energy_EU, value=sum(value)) %>% ungroup() %>% mutate(region = "EU28") %>%
                 select(region, FLOW, PRODUCT, period, unit, value)
IEA_energy_China <- filter(IEA_energy, region %in% IEA_China) %>% group_by(FLOW, PRODUCT, period, unit)
IEA_energy_China <- summarise(IEA_energy_China, value=sum(value)) %>% ungroup() %>% mutate(region = "CHINA+") %>%
                    select(region, FLOW, PRODUCT, period, unit, value)
IEA_energy_bunkers <- filter(IEA_energy, region %in% IEA_Bunkers) %>% 
                      group_by(FLOW, PRODUCT, period, unit) %>%
                      summarise(value=sum(value)) %>% 
                      ungroup() %>% 
                      mutate(region="BUNKERS") %>%
                      select(region, FLOW, PRODUCT, period, unit, value)
IEA_energy <- rbind(IEA_energy, IEA_energy_EU) %>% 
              rbind(IEA_energy_China) %>%
              rbind(IEA_energy_bunkers) %>%
              arrange(region, FLOW, PRODUCT, period)

# Only select CD-LINKS countries and use same region names
IEA_energy <- filter(IEA_energy, region %in% regions_IEA_IAM)
IEA_energy$region=str_replace_all(IEA_energy$region,"BRAZIL","BRA")
IEA_energy$region=str_replace_all(IEA_energy$region,"CHINA\\+","CHN")
IEA_energy$region=str_replace_all(IEA_energy$region,"INDIA","IND")
IEA_energy$region=str_replace_all(IEA_energy$region,"EU28","EU")
IEA_energy$region=str_replace_all(IEA_energy$region,"JAPAN","JPN")
IEA_energy$region=str_replace_all(IEA_energy$region,"RUSSIA","RUS")
IEA_energy$region=str_replace_all(IEA_energy$region,"USA","USA")
IEA_energy$region=str_replace_all(IEA_energy$region,"TURKEY","TUR")
IEA_energy$region=str_replace_all(IEA_energy$region,"CANADA","CAN") 
IEA_energy$region=str_replace_all(IEA_energy$region,"WORLD","World") 
IEA_energy$region=str_replace_all(IEA_energy$region,"BUNKERS","Bunkers") 
write.table(IEA_energy, file="data/IEA.csv", sep=";", row.names=F)  

# Electricity (TJ to EJ)
IEA_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_products, variable="Secondary Energy|Electricity")
IEA_Coal_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_coal_product, variable="Secondary Energy|Electricity|Coal")
IEA_CrudeOil_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_crude_oil_product, variable="Secondary Energy|Electricity|Crude oil")
IEA_LightOil_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_light_oil_product, variable="Secondary Energy|Electricity|Light oil")
IEA_NatGas_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product="NATGAS", variable="Secondary Energy|Electricity|Gas")
IEA_Nuclear_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product="NUCLEAR", variable="Secondary Energy|Electricity|Nuclear")
IEA_Hydro_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product="HYDRO", variable="Secondary Energy|Electricity|Hydro")
IEA_OtherREN_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_other_ren_product, variable="Secondary Energy|Electricity|Other Renewables")
IEA_Biomass_Electricity <- ConvertIEA2IAM(IEA_energy, flow=IEA_elec_flows, product=IEA_biomass_product, variable="Secondary Energy|Electricity|Biomass")
# Heat (TJ to EJ)
IEA_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_products, variable="Secondary Energy|Heat")
IEA_Coal_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_coal_product, variable="Secondary Energy|Heat|Coal")
IEA_CrudeOil_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_crude_oil_product, variable="Secondary Energy|Heat|Crude oil")
IEA_LightOil_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_light_oil_product, variable="Secondary Energy|Heat|Light oil")
IEA_NatGas_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product="NATGAS", variable="Secondary Energy|Heat|Gas")
IEA_Nuclear_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product="NUCLEAR", variable="Secondary Energy|Heat|Nuclear")
IEA_Hydro_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product="HYDRO", variable="Secondary Energy|Heat|Hydro")
IEA_OtherREN_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_other_ren_product, variable="Secondary Energy|Heat|Other Renewables")
IEA_Biomass_Heat <- ConvertIEA2IAM(IEA_energy, flow=IEA_heat_flows, product=IEA_biomass_product, variable="Secondary Energy|Heat|Biomass")
# Final energy, excluding bunkers (TJ to EJ)
IEA_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_products, variable="Final Energy")
IEA_Electricity_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product="ELECTR", variable="Final Energy|Electricity")
IEA_Heat_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product="HEAT", variable="Final Energy|Heat")
IEA_Coal_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_coal_product, variable="Final Energy|Coal")
IEA_CrudeOil_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_crude_oil_product, variable="Final Energy|Crude oil")
IEA_LightOil_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_light_oil_product, variable="Final Energy|Light oil")
IEA_NatGas_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product="NATGAS", variable="Final Energy|Gas")
IEA_Nuclear_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product="NUCLEAR", variable="Final Energy|Nuclear")
IEA_Hydro_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product="HYDRO", variable="Final Energy|Hydro")
IEA_OtherREN_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_other_ren_product, variable="Final Energy|Other Renewables")
IEA_Biomass_FinalEnergy <- ConvertIEA2IAM(IEA_energy, flow=IEA_tfc_flow, product=IEA_biomass_product, variable="Final Energy|Biomass")
# Final energy per sector (TJ to EJ)
# industry
IEA_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product=IEA_products, variable="Final Energy|Industry")
IEA_Electricity_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product="ELECTR", variable="Final Energy|Industry|Electricity")
IEA_Heat_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product="HEAT", variable="Final Energy|Industry|Heat")
IEA_Nuclear_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product="NUCLEAR", variable="Final Energy|Industry|Nuclear")
IEA_Hydro_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product="HYDRO", variable="Final Energy|Industry|Hydro")
IEA_OtherREN_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product=IEA_other_ren_product, variable="Final Energy|Industry|Other Renewables")
IEA_Biomass_FinalEnergy_Industry <- ConvertIEA2IAM(IEA_energy, flow=IEA_industry_flow, product=IEA_biomass_product, variable="Final Energy|Industry|Biomass")
# transport, excluding bunkers
IEA_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product=IEA_products, variable="Final Energy|Transportation")
IEA_Electricity_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product="ELECTR", variable="Final Energy|Transportation|Electricity")
IEA_Heat_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product="HEAT", variable="Final Energy|Transportation|Heat")
IEA_Nuclear_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product="NUCLEAR", variable="Final Energy|Transportation|Nuclear")
IEA_Hydro_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product="HYDRO", variable="Final Energy|Transportation|Hydro")
IEA_OtherREN_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product=IEA_other_ren_product, variable="Final Energy|Transportation|Other Renewables")
IEA_Biomass_FinalEnergy_Transport <- ConvertIEA2IAM(IEA_energy, flow=IEA_transport_flow, product=IEA_biomass_product, variable="Final Energy|Transportation|Biomass")
# bunkers (only global number is used in 'all')
#IEA_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_products, variable="Final Energy|Bunkers")
#IEA_Electricity_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product="ELECTR", variable="Final Energy|Bunkers|Electricity")
#IEA_Heat_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product="HEAT", variable="Final Energy|Bunkers|Heat")
#IEA_Coal_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_coal_product, variable="Final Energy|Bunkers|Coal")
#IEA_CrudeOil_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_crude_oil_product, variable="Final Energy|Bunkers|Crude oil")
#IEA_LightOil_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_light_oil_product, variable="Final Energy|Bunkers|Light oil")
#IEA_NatGas_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product="NATGAS", variable="Final Energy|Bunkers|Gas")
#IEA_Nuclear_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product="NUCLEAR", variable="Final Energy|Bunkers|Nuclear")
#IEA_Hydro_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product="HYDRO", variable="Final Energy|Bunkers|Hydro")
#IEA_OtherREN_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_other_ren_product, variable="Final Energy|Bunkers|Other Renewables")
#IEA_Biomass_FinalEnergy_Bunkers <- ConvertIEA2IAM(IEA_energy, flow=IEA_bunkers_flow, product=IEA_biomass_product, variable="Final Energy|Bunkers|Biomass")
# buildings
IEA_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_products, variable="Final Energy|Residential and Commercial")
IEA_Electricity_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product="ELECTR", variable="Final Energy|Residential and Commercial|Electricity")
IEA_Heat_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product="HEAT", variable="Final Energy|Residential and Commercial|Heat")

IEA_Coal_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_coal_product, variable="Final Energy|Residential and Commercial|Coal")
IEA_CrudeOil_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_crude_oil_product, variable="Final Energy|Residential and Commercial|Crude oil")
IEA_LightOil_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_light_oil_product, variable="Final Energy|Residential and Commercial|Light oil")
IEA_NatGas_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product="NATGAS", variable="Final Energy|Residential and Commercial|Gas")

IEA_Nuclear_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product="NUCLEAR", variable="Final Energy|Residential and Commercial|Nuclear")
IEA_Hydro_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product="HYDRO", variable="Final Energy|Residential and Commercial|Hydro")
IEA_OtherREN_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_other_ren_product, variable="Final Energy|Residential and Commercial|Other Renewables")
IEA_Biomass_FinalEnergy_Buildings <- ConvertIEA2IAM(IEA_energy, flow=IEA_buildings_flow, product=IEA_biomass_product, variable="Final Energy|Residential and Commercial|Biomass")
# other
IEA_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product=IEA_products, variable="Final Energy|Other")
IEA_Electricity_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product="ELECTR", variable="Final Energy|Other|Electricity")
IEA_Heat_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product="HEAT", variable="Final Energy|Other|Heat")
IEA_Nuclear_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product="NUCLEAR", variable="Final Energy|Other|Nuclear")
IEA_Hydro_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product="HYDRO", variable="Final Energy|Other|Hydro")
IEA_OtherREN_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product=IEA_other_ren_product, variable="Final Energy|Other|Other Renewables")
IEA_Biomass_FinalEnergy_Other <- ConvertIEA2IAM(IEA_energy, flow=IEA_other_flow, product=IEA_biomass_product, variable="Final Energy|Other|Biomass")
# Primary energy (TJ to EJ)
IEA_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_products, variable="Primary Energy")
IEA_Electricity_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product="ELECTR", variable="Primary Energy|Electricity")
IEA_Heat_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product="HEAT", variable="Primary Energy|Heat")
IEA_Coal_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_coal_product, variable="Primary Energy|Coal")
IEA_CrudeOil_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_crude_oil_product, variable="Primary Energy|Crude oil")
IEA_LightOil_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_light_oil_product, variable="Primary Energy|Light oil")
IEA_NatGas_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product="NATGAS", variable="Primary Energy|Gas")
IEA_Nuclear_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product="NUCLEAR", variable="Primary Energy|Nuclear")
IEA_Hydro_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product="HYDRO", variable="Primary Energy|Hydro")
IEA_OtherREN_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_other_ren_product, variable="Primary Energy|Other Renewables")
IEA_Biomass_PrimaryEnergy <- ConvertIEA2IAM(IEA_energy, flow="TPES", product=IEA_biomass_product, variable="Primary Energy|Biomass")

all_IEA <- rbind(IEA_Electricity, IEA_Coal_Electricity) %>% rbind(IEA_CrudeOil_Electricity) %>% rbind(IEA_LightOil_Electricity) %>% rbind(IEA_NatGas_Electricity) %>%
           rbind(IEA_Hydro_Electricity) %>% rbind(IEA_OtherREN_Electricity) %>%
           rbind(IEA_Biomass_Electricity) %>% rbind(IEA_Nuclear_Electricity) %>%
           rbind(IEA_Heat) %>% rbind(IEA_Coal_Heat) %>% rbind(IEA_CrudeOil_Heat) %>% rbind(IEA_LightOil_Heat) %>% rbind(IEA_NatGas_Heat) %>%
           rbind(IEA_Hydro_Heat) %>% rbind(IEA_OtherREN_Heat) %>%
           rbind(IEA_Biomass_Heat) %>% rbind(IEA_Nuclear_Heat) %>%
           rbind(IEA_FinalEnergy) %>% rbind(IEA_Electricity_FinalEnergy) %>% rbind(IEA_Heat_FinalEnergy) %>%
           rbind(IEA_Coal_FinalEnergy) %>% rbind(IEA_CrudeOil_FinalEnergy) %>% rbind(IEA_LightOil_FinalEnergy) %>% rbind(IEA_NatGas_FinalEnergy) %>%
           rbind(IEA_Hydro_FinalEnergy) %>% rbind(IEA_OtherREN_FinalEnergy) %>%
           rbind(IEA_Biomass_FinalEnergy) %>% rbind(IEA_Nuclear_FinalEnergy) %>%
           rbind(IEA_FinalEnergy_Industry) %>% rbind(IEA_Electricity_FinalEnergy_Industry) %>% rbind(IEA_Heat_FinalEnergy_Industry) %>%
           rbind(IEA_Hydro_FinalEnergy_Industry) %>% rbind(IEA_OtherREN_FinalEnergy_Industry) %>%
           rbind(IEA_Biomass_FinalEnergy_Industry) %>% rbind(IEA_Nuclear_FinalEnergy_Industry) %>%
           rbind(IEA_FinalEnergy_Transport) %>% rbind(IEA_Electricity_FinalEnergy_Transport) %>% rbind(IEA_Heat_FinalEnergy_Transport) %>%
           rbind(IEA_Hydro_FinalEnergy_Transport) %>% rbind(IEA_OtherREN_FinalEnergy_Transport) %>%
           rbind(IEA_Biomass_FinalEnergy_Transport) %>% rbind(IEA_Nuclear_FinalEnergy_Transport) %>%
           
           rbind(IEA_FinalEnergy_Buildings) %>% rbind(IEA_Electricity_FinalEnergy_Buildings) %>% rbind(IEA_Heat_FinalEnergy_Buildings) %>%
           rbind(IEA_Coal_FinalEnergy_Buildings) %>% rbind(IEA_CrudeOil_FinalEnergy_Buildings) %>% rbind(IEA_LightOil_FinalEnergy_Buildings) %>% rbind(IEA_NatGas_FinalEnergy_Buildings) %>%
           rbind(IEA_Hydro_FinalEnergy_Buildings) %>% rbind(IEA_OtherREN_FinalEnergy_Buildings) %>%
           rbind(IEA_Biomass_FinalEnergy_Buildings) %>% rbind(IEA_Nuclear_FinalEnergy_Buildings) %>%
           
           rbind(IEA_FinalEnergy_Other) %>% rbind(IEA_Electricity_FinalEnergy_Other) %>% rbind(IEA_Heat_FinalEnergy_Other) %>%
           rbind(IEA_Hydro_FinalEnergy_Other) %>% rbind(IEA_OtherREN_FinalEnergy_Other) %>%
           rbind(IEA_Biomass_FinalEnergy_Other) %>% rbind(IEA_Nuclear_FinalEnergy_Other) %>%
           rbind(IEA_PrimaryEnergy) %>% rbind(IEA_Electricity_PrimaryEnergy) %>% rbind(IEA_Heat_PrimaryEnergy) %>%
           rbind(IEA_Coal_PrimaryEnergy) %>% rbind(IEA_CrudeOil_PrimaryEnergy) %>% rbind(IEA_LightOil_PrimaryEnergy) %>% rbind(IEA_NatGas_PrimaryEnergy) %>%
           rbind(IEA_Hydro_PrimaryEnergy) %>% rbind(IEA_OtherREN_PrimaryEnergy) %>%
           rbind(IEA_Biomass_PrimaryEnergy) %>% rbind(IEA_Nuclear_PrimaryEnergy)
  
all_hist <- rbind(all_hist, all_IEA)
all_hist$value <- as.numeric(all_hist$value)

# make one oil variable
all_hist <- calcVariable(all_hist,'`Secondary Energy|Heat|Oil` ~ (`Secondary Energy|Heat|Crude oil`)+(`Secondary Energy|Heat|Light oil`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Oil` ~ (`Final Energy|Crude oil`)+(`Final Energy|Light oil`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Primary Energy|Oil` ~ (`Primary Energy|Crude oil`)+(`Primary Energy|Light oil`)' , newUnit='EJ/yr')

# add renewable/non-fossil electricity
all_hist <- calcVariable(all_hist,'`Secondary Energy|Electricity|Renewable` ~ (`Secondary Energy|Electricity|Biomass`)+(`Secondary Energy|Electricity|Other Renewables`)+(`Secondary Energy|Electricity|Hydro`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Electricity|Non-fossil` ~ (`Secondary Energy|Electricity|Biomass`)+(`Secondary Energy|Electricity|Other Renewables`)+(`Secondary Energy|Electricity|Hydro`)+(`Secondary Energy|Electricity|Nuclear`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Electricity|Renewable share` ~ ifelse(`Secondary Energy|Electricity`==0,0,100*((`Secondary Energy|Electricity|Renewable`))/(`Secondary Energy|Electricity`))' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Electricity|Non-fossil share` ~ ifelse(`Secondary Energy|Electricity`==0,0,100*((`Secondary Energy|Electricity|Non-fossil`))/(`Secondary Energy|Electricity`))' , newUnit='%')

# add renewable/non-fossil heat
all_hist <- calcVariable(all_hist,'`Secondary Energy|Heat|Renewable` ~ (`Secondary Energy|Heat|Biomass`)+(`Secondary Energy|Heat|Other Renewables`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Heat|Non-fossil` ~ (`Secondary Energy|Heat|Biomass`)+(`Secondary Energy|Heat|Other Renewables`)+(`Secondary Energy|Heat|Nuclear`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Heat|Renewable share` ~ ifelse(`Secondary Energy|Heat`==0,0,100*((`Secondary Energy|Heat|Renewable`))/(`Secondary Energy|Heat`))' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Secondary Energy|Heat|Non-fossil share` ~ ifelse(`Secondary Energy|Heat`==0,0,100*((`Secondary Energy|Heat|Non-fossil`))/(`Secondary Energy|Heat`))' , newUnit='%')

# add renewable final energy
all_hist <- calcVariable(all_hist,'`Final Energy|Solids|Biomass|Traditional` ~ (`Final Energy|Residential and Commercial|Biomass`)', newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Renewable` ~ (`Final Energy|Biomass`)-(`Final Energy|Solids|Biomass|Traditional`)+ 
                                                              (`Final Energy|Other Renewables`)+
                                                              (1/100)*(`Secondary Energy|Electricity|Renewable share`)*(`Final Energy|Electricity`)+
                                                              (1/100)*(`Secondary Energy|Heat|Renewable share`)*(`Final Energy|Heat`)', 
                                                              newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Non-fossil` ~ (`Final Energy|Biomass`)-(`Final Energy|Solids|Biomass|Traditional`)+ 
                                                               #nuclear is zero (`Final Energy|Nuclear`)+
                                                               (`Final Energy|Other Renewables`)+
                                                               (1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Electricity`)+
                                                               (1/100)*(`Secondary Energy|Heat|Non-fossil share`)*(`Final Energy|Heat`)', 
                                                               newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Fossil` ~ (`Final Energy`)-(`Final Energy|Non-fossil`)' , newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Renewable share` ~ 100*((`Final Energy|Renewable`))/(`Final Energy`)' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Final Energy|Non-fossil share` ~ 100*((`Final Energy|Non-fossil`))/(`Final Energy`)' , newUnit='%')

# add renewable final energy per sector
# industry
all_hist <- calcVariable(all_hist,'`Final Energy|Industry|Renewable` ~ (`Final Energy|Industry|Biomass`)+(`Final Energy|Industry|Other Renewables`)+
                                  (1/100)*(`Secondary Energy|Electricity|Renewable share`)*(`Final Energy|Industry|Electricity`)+
                                  (1/100)*(`Secondary Energy|Heat|Renewable share`)*(`Final Energy|Industry|Heat`)', 
                                  newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Industry|Non-fossil` ~ (`Final Energy|Industry|Biomass`)+(`Final Energy|Industry|Other Renewables`)+
                                  (1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Industry|Electricity`)+
                                  (1/100)*(`Secondary Energy|Heat|Non-fossil share`)*(`Final Energy|Industry|Heat`)' , 
                                  newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Industry|Renewable share` ~ 100*((`Final Energy|Industry|Renewable`))/(`Final Energy|Industry`)' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Final Energy|Industry|Non-fossil share` ~ 100*((`Final Energy|Industry|Non-fossil`))/(`Final Energy|Industry`)' , newUnit='%')
# transport
all_hist <- calcVariable(all_hist,'`Final Energy|Transportation|Renewable` ~ (`Final Energy|Transportation|Biomass`)+(`Final Energy|Transportation|Other Renewables`)+
                                  (1/100)*(`Secondary Energy|Electricity|Renewable share`)*(`Final Energy|Transportation|Electricity`)', newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Transportation|Non-fossil` ~ (`Final Energy|Transportation|Biomass`)+(`Final Energy|Transportation|Other Renewables`)+
                                  (1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Transportation|Electricity`)', newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Transportation|Renewable share` ~ 100*((`Final Energy|Transportation|Renewable`))/(`Final Energy|Transportation`)', newUnit='%')
all_hist <- calcVariable(all_hist,'`Final Energy|Transportation|Non-fossil share` ~ 100*((`Final Energy|Transportation|Non-fossil`))/(`Final Energy|Transportation`)',newUnit='%')
# buildings
all_hist <- calcVariable(all_hist,'`Final Energy|Residential and Commercial|Renewable` ~ (`Final Energy|Residential and Commercial|Biomass`)+(`Final Energy|Residential and Commercial|Other Renewables`)+
                                                                                       (1/100)*(`Secondary Energy|Electricity|Renewable share`)*(`Final Energy|Residential and Commercial|Electricity`)+
                                                                                       (1/100)*(`Secondary Energy|Heat|Renewable share`)*(`Final Energy|Residential and Commercial|Heat`)', 
                                                                                       newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Residential and Commercial|Non-fossil` ~ (`Final Energy|Residential and Commercial|Biomass`)+(`Final Energy|Residential and Commercial|Other Renewables`)+
                                                                                        (1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Residential and Commercial|Electricity`)+
                                                                                        (1/100)*(`Secondary Energy|Heat|Non-fossil share`)*(`Final Energy|Residential and Commercial|Heat`)', 
                                                                                        newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Residential and Commercial|Renewable share` ~ 100*((`Final Energy|Residential and Commercial|Renewable`))/(`Final Energy|Residential and Commercial`)' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Final Energy|Residential and Commercial|Non-fossil share` ~ 100*((`Final Energy|Residential and Commercial|Non-fossil`))/(`Final Energy|Residential and Commercial`)' , newUnit='%')
# other
all_hist <- calcVariable(all_hist,'`Final Energy|Other|Renewable` ~ (`Final Energy|Other|Biomass`)+(`Final Energy|Other|Other Renewables`)+
                                    (1/100)*(`Secondary Energy|Electricity|Renewable share`)*(`Final Energy|Other|Electricity`)+
                                    (1/100)*(`Secondary Energy|Heat|Renewable share`)*(`Final Energy|Other|Heat`)', 
                                    newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Other|Non-fossil` ~ (`Final Energy|Other|Biomass`)+(`Final Energy|Other|Other Renewables`)+
                                    (1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Other|Electricity`)+
                                    (1/100)*(`Secondary Energy|Heat|Non-fossil share`)*(`Final Energy|Other|Heat`)', 
                                    newUnit='EJ/yr')
all_hist <- calcVariable(all_hist,'`Final Energy|Other|Renewable share` ~ 100*((`Final Energy|Other|Renewable`))/(`Final Energy|Other`)', newUnit='%')
all_hist <- calcVariable(all_hist,'`Final Energy|Other|Non-fossil share` ~ 100*((`Final Energy|Other|Non-fossil`))/(`Final Energy|Other`)',newUnit='%')

vars_IEA_balance <- c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Renewable", "Secondary Energy|Electricity|Non-fossil",
                      "Secondary Energy|Electricity|Coal", "Secondary Energy|Electricity|Crude oil", "Secondary Energy|Electricity|Light oil", "Secondary Energy|Electricity|Gas", "Secondary Energy|Electricity|Nuclear", "Secondary Energy|Electricity|Hydro", "Secondary Energy|Electricity|Other Renewables", "Secondary Energy|Electricity|Biomass",
                      "Secondary Energy|Electricity|Renewable share", "Secondary Energy|Electricity|Non-fossil share",
                      "Secondary Energy|Heat", "Secondary Energy|Heat|Renewable", "Secondary Energy|Heat|Non-fossil",
                      "Secondary Energy|Heat|Coal", "Secondary Energy|Heat|Crude oil", "Secondary Energy|Heat|Light oil", "Secondary Energy|Heat|Gas", "Secondary Energy|Heat|Nuclear", "Secondary Energy|Heat|Hydro", "Secondary Energy|Heat|Other Renewables", "Secondary Energy|Heat|Biomass",
                      "Secondary Energy|Heat|Renewable share", "Secondary Energy|Heat|Non-fossil share",
                      "Final Energy|Coal", "Final Energy|Crude oil", "Final Energy|Light oil", "Final Energy|Gas", "Final Energy|Nuclear", "Final Energy|Hydro", "Final Energy|Other Renewables", "Final Energy|Biomass", "Final Energy|Electricity", "Final Energy|Heat",
                      "Final Energy", "Final Energy|Renewable", "Final Energy|Non-fossil",
                      "Final Energy|Renewable share", "Final Energy|Non-fossil share",
                      "Final Energy|Industry", "Final Energy|Industry|Renewable", "Final Energy|Industry|Non-fossil", "Final Energy|Industry|Biomass",
                      "Final Energy|Industry|Renewable share", "Final Energy|Industry|Non-fossil share",
                      "Final Energy|Transportation", "Final Energy|Transportation|Renewable", "Final Energy|Transportation|Non-fossil", "Final Energy|Transportation|Biomass",
                      "Final Energy|Transportation|Renewable share", "Final Energy|Transportation|Non-fossil share",
                      "Final Energy|Residential and Commercial|Coal", "Final Energy|Residential and Commercial|Crude oil", "Final Energy|Residential and Commercial|Light oil", "Final Energy|Residential and Commercial|Gas", "Final Energy|Residential and Commercial|Nuclear", "Final Energy|Residential and Commercial|Hydro", "Final Energy|Residential and Commercial|Other Renewables", "Final Energy|Residential and Commercial|Biomass", "Final Energy|Residential and Commercial|Electricity", "Final Energy|Residential and Commercial|Heat",
                      "Final Energy|Residential and Commercial", "Final Energy|Residential and Commercial|Renewable", "Final Energy|Residential and Commercial|Non-fossil",
                      #"Final Energy|Residential and Commercial", "Final Energy|Residential and Commercial|Renewable", "Final Energy|Residential and Commercial|Non-fossil",
                      "Final Energy|Residential and Commercial|Renewable share", "Final Energy|Residential and Commercial|Non-fossil share",
                      "Final Energy|Other", "Final Energy|Other|Renewable", "Final Energy|Other|Non-fossil", "Final Energy|Other|Biomass",
                      "Final Energy|Other|Renewable share", "Final Energy|Other|Non-fossil share")

IEA_balance <- filter(all_hist, model=="History", variable %in% vars_IEA_balance) %>% 
               select(Category, region, period, value, unit, variable)
IEA_balance$value <- format(IEA_balance$value, scientific=FALSE)
write.table(IEA_balance, file="data/IEA_balance.csv", sep=";", row.names=F)  

# CO2-intensity (MtCO2e/US(2005))
source("../../TIMER_output/functions/mym2r.R")
regions_IMAGE = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","IND","KOR","CHN","SEAS","INDO","JPN","OCE","RSAS","RSAF","dummy","World")
regions_IMAGE_EU = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","IND","KOR","CHN","SEAS","INDO","JPN","OCE","RSAS","RSAF","EU","World")
OECD_GDP_MER = read.mym2r.nice(mym.folder="data", 
                               filename='gdptot.out', varname=NULL, 
                               collist=list(regions_IMAGE), 
                               namecols=c('region'), novarname = TRUE)
# Convert to from million $(2005) to billion $(2010) dollars (see supermapping)
OECD_GDP_MER$value <- 0.00110774*OECD_GDP_MER$value
# Add EU
OECD_GDP_MER <- subset(OECD_GDP_MER, region != "dummy")
EU <- inner_join(filter(OECD_GDP_MER, region=='WEU'), filter(OECD_GDP_MER, region=='CEU'), by=c("year"))
EU$region <- "EU"
EU <- EU %>% mutate(value=value.x+value.y) %>% select(year, region, value)
#EU$region = factor(EU$region, levels=regions_IMAGE_EU)
OECD_GDP_MER <- rbind(OECD_GDP_MER, EU)
#OECD_GDP_MER$region = factor(OECD_GDP_MER$region,labels=regions_IMAGE_EU)
OECD_GDP_MER <- mutate(OECD_GDP_MER, unit="billion US$2010/yr")
# convert to IAM structure (all)
OECD_GDP_MER <- filter(OECD_GDP_MER, region%in%regions_IAM)
OECD_GDP_MER <- rename(OECD_GDP_MER, period=year)
OECD_GDP_MER <- mutate(OECD_GDP_MER, scenario="") %>% mutate(Category="Historical") %>% mutate(Baseline="") %>% mutate(model="History") %>% mutate(Scope="") %>% 
                mutate(variable="GDP|MER")
OECD_GDP_MER <- select(OECD_GDP_MER, scenario, Category, Baseline, model, region, period, Scope, value, unit, variable)
OECD_GDP_MER <- as.data.frame(OECD_GDP_MER)
all_hist <- rbind(all_hist, OECD_GDP_MER)
# calculate co2-intensity
all_hist <- calcVariable(all_hist,'`Carbon Intensity of GDP|MER` ~ (`Emissions|CO2`)/(`GDP|MER`)' , newUnit='kg CO2/$US 2010')
all_hist[variable=="Carbon Intensity of GDP|MER"]$model <- "PRIMAP/OECD"

# Kaya indicators
all_hist <- calcVariable(all_hist,'`Energy intensity of GDP` ~ 1000*(`Primary Energy`)/(`GDP|MER`)' , newUnit='TJ/$US(2010)')
all_hist <- calcVariable(all_hist,'`Conversion efficiency` ~ (`Final Energy`)/(`Primary Energy`)' , newUnit='%')
all_hist <- calcVariable(all_hist,'`Carbon intensity of fossil-fuel use` ~ (`Emissions|CO2|Energy`)/(`Final Energy|Fossil`)' , newUnit='Mt CO2/EJ')
all_hist <- calcVariable(all_hist, '`Primary Energy|Fossil` ~ (`Primary Energy|Coal`)+(`Primary Energy|Oil`)+(`Primary Energy|Gas`)' , newUnit='EJ')
all_hist <- calcVariable(all_hist, '`Fossil fuel utilisation rate` ~ `Primary Energy|Fossil`/`Final Energy|Fossil`' , newUnit='%')
all_hist <- calcVariable(all_hist, '`Energy utilisation rate` ~ `Final Energy`/`Primary Energy`' , newUnit='%')

