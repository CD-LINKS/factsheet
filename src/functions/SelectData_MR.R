# create data

#data_cd_V3 <- read.csv('data/cdlinks_compare_20170818-104743.csv')
data_cd_V3 <- all
data_cd_v4_org <- all_original

# specify regions, scenario and variables that will be used for presentation of results carbon budget
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "RoW","World")
scens_emis <- c("NoPolicy_V3", "NPi_V3", "INDCi_V3", "2030_low_V3", "2020_low_V3", "2020_verylow_V3")
category_emis <- c("No policy", "National policies", "NDC", "2030_low", "Carbon budget 1000", "Carbon budget 400")
models <- c("AIM/CGE", "COPPE-COFFEE 1.0", "DNE21+ V.14", "GEM-E3", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "REMIND-MAgPIE 1.7-3.0", "POLES CDL", "WITCH2016")

# get emissions data per sector
vars_emis_co2 <- c("Emissions|CO2", "Emissions|CO2|Energy", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Energy|Supply", "Emissions|CO2|Energy|Supply|Other Sector", "Emissions|CO2|Energy|Demand", "Emissions|CO2|Energy|Demand|AFOFI", "Emissions|CO2|Energy|Demand|Industry", "Emissions|CO2|Energy|Demand|Residential and Commercial", "Emissions|CO2|Energy|Demand|Transportation", "Emissions|CO2|Industrial Processes", "Emissions|CO2|AFOLU")
vars_emis_ch4 <- c("Emissions|CH4", "Emissions|CH4|Energy", "Emissions|CH4|Energy|Supply", "Emissions|CH4|Energy|Demand|Industry", "Emissions|CH4|Energy|Demand|Residential and Commercial", "Emissions|CH4|Energy|Demand|Transportation", "Emissions|CH4|AFOLU", "Emissions|CH4|Waste")
vars_emis_n2o <- c("Emissions|N2O", "Emissions|N2O|Energy", "Emissions|N2O|AFOLU", "Emissions|N2O|Other", "Emissions|N2O|Waste")
vars_emis_fgases <- c("Emissions|F-Gases", "Emissions|HFC", "Emissions|HFC|HFC125", "Emissions|HFC|HFC134a", "Emissions|HFC|HFC143a", "Emissions|HFC|HFC227ea", "Emissions|HFC|HFC245fa", "Emissions|HFC|HFC32", "Emissions|HFC|HFC23", "Emissions|HFC|HFC43-10", "Emissions|C6F14", "Emissions|C2F6", "Emissions|CF4", "Emissions|PFC", "Emissions|SF6")
vars_emis_kyoto <- c("Emissions|Kyoto Gases")
vars_emis_allGHG <- c("Emissions|CO2", "Emissions|CO2|Energy", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|Energy|Supply", "Emissions|CO2|Energy|Supply|Other Sector", "Emissions|CO2|Energy|Demand", "Emissions|CO2|Energy|Demand|AFOFI", "Emissions|CO2|Energy|Demand|Industry", "Emissions|CO2|Energy|Demand|Residential and Commercial", "Emissions|CO2|Energy|Demand|Transportation", "Emissions|CO2|Industrial Processes", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|CH4|Energy", "Emissions|CH4|Energy|Supply", "Emissions|CH4|Energy|Demand|Industry", "Emissions|CH4|Energy|Demand|Residential and Commercial", "Emissions|CH4|Energy|Demand|Transportation", "Emissions|CH4|AFOLU", "Emissions|N2O", "Emissions|N2O|Energy", "Emissions|N2O|AFOLU", "Emissions|F-Gases", "Emissions|HFC", "Emissions|HFC|HFC125", "Emissions|HFC|HFC134a", "Emissions|HFC|HFC143a", "Emissions|HFC|HFC227ea", "Emissions|HFC|HFC245fa", "Emissions|HFC|HFC32", "Emissions|HFC|HFC23", "Emissions|HFC|HFC43-10", "Emissions|C6F14", "Emissions|C2F6", "Emissions|CF4", "Emissions|PFC", "Emissions|SF6", "Emissions|Kyoto Gases", "Emissions|CH4|Waste", "Emissions|N2O|Other", "Emissions|N2O|Waste", "Emissions|Non-CO2", "Emissions|CO2|Industry")
vars_indicator = c("Energy Intensity of GDP|MER|rel2010","Carbon Intensity of FE|rel2010", "Mitigation Costs")


# get GHG data
data_co2 <- subset(data_cd_V3, model %in% models)
data_co2 <- subset(data_co2, region %in% regs)
data_co2 <- subset(data_co2, Category %in% category_emis)
data_co2 <- subset(data_co2, variable %in% vars_emis_co2)
write.table(data_co2, file = "data/WP3_3/data_co2.csv", sep=";", row.names = F)
data_ch4 <- subset(data_cd_V3, model %in% models)
data_ch4 <- subset(data_ch4, region %in% regs)
data_ch4 <- subset(data_ch4, Category %in% category_emis)
data_ch4 <- subset(data_ch4, variable %in% vars_emis_ch4)
write.table(data_ch4, file = "data/WP3_3/data_ch4.csv", sep=";", row.names = F)
data_n20 <- subset(data_cd_V3, model %in% models)
data_n2o <- subset(data_n2o, region %in% regs)
data_n2o <- subset(data_n2o, Category %in% category_emis)
data_n2o <- subset(data_n2o, variable %in% vars_emis_n2o)
write.table(data_n2o, file = "data/WP3_3/data_n20.csv", sep=";", row.names = F)
data_fgases <- subset(data_cd_V3, model %in% models)
data_fgases <- subset(data_fgases, region %in% regs)
data_fgases <- subset(data_fgases, Category %in% category_emis)
data_fgases <- subset(data_fgases, variable %in% vars_emis_fgases)
write.table(data_fgases, file = "data/WP3_3/data_fgases.csv", sep=";", row.names = F)
data_allGHG <- subset(data_cd_V3, model %in% models)
data_allGHG <- subset(data_allGHG, region %in% regs)
data_allGHG <- subset(data_allGHG, Category %in% category_emis)
data_allGHG <- subset(data_allGHG, variable %in% vars_emis_allGHG)
setcolorder(data_allGHG, c("scenario", "Category", "Baseline", "model", "region", "variable", "unit", "period", "value", "Scope"))
write.table(data_allGHG, file = "data/WP3_3/data_allGHG.csv", sep=";", row.names = F)
#original
data_GHG <- subset(data_cd_v4_org, model %in% models)
data_GHG <- subset(data_GHG, region %in% regs)
data_GHG <- subset(data_GHG, scenario %in% scens_emis)
data_GHG <- subset(data_GHG, variable %in% vars_emis_allGHG)
write.table(data_GHG, file = "data/WP3_3/data_GHG.csv", sep=";", row.names = F)

data_national <- subset(data_cd_V3, Scope == 'national')
data_national <- subset(data_national, region %in% regs)
data_national <- subset(data_national, Category %in% category_emis)
data_national <- subset(data_national, variable %in% vars_emis_allGHG)
write.table(data_national, file = "data/WP3_3/data_national.csv", sep=";", row.names = F)
data_indicator <- subset(data_cd_V3, model %in% models)
data_indicator <- subset(data_indicator, region %in% regs)
data_indicator <- subset(data_indicator, Category %in% category_emis)
data_indicator <- subset(data_indicator, variable %in% vars_indicator)
write.table(data_indicator, file = "data/WP3_3/data_indicator.csv", sep=";", row.names = F)

####################################################################################

# determine values in database for models, scenario, regions
models <- unique(data_cd_V3[1])
scenarios <- unique(data_cd_V3[2])
regions <- unique(data_cd_V3[3])

scens <- c("NoPolicy", "INDCi", "NPi", "NPip")
scens_V3 <- c("NoPolicy_V3", "INDCi_V3", "NPi_V3", "NPip_V3", "2020_low", "2030_very_low")
scens_ext_V3 <- c("NoPolicy_V3", "INDCi_V3", "NPi_V3", "NPip_V3", "NPi2020_400_V3", "NPi2020_1000_V3", "NPi2020_1600_V3", "INDC2030i_1000_V3", "INDC2030i_1600_V3")
vars <- c("Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|Kyoto Gases")
models <- c("AIM/CGE", "COPPE-COFFEE 1.0", "DNE21+ V.14", "GEM-E3_V1", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "REMIND-MAgPIE 1.7-3.0", "POLES CDL", "WITCH2016")

# specify regions, scenario and variables that will be used for presentation of results Kaya indicators
regs_kaya <- c("CHN","IND","EU","USA","World")
scens_kaya <- c("INDCi_V3", "NPi_V3", "NPi2020_1000_V3", "INDC2030i_1000_V3")
vars_kaya <- c("Population", "GDP|MER", "GDP|PPP", "Emissions|CO2", "Emissions|CO2|AFOLU", "Emissions|Kyoto Gases", "Final Energy")

regs_REN <- c("CHN", "IND")
scens_REN <- c("NoPolicy_V3", "NPi_V3", "NPip_V3", "INDCi_V3")
vars_electricity <- c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Biomass|w/o CCS", "Secondary Energy|Electricity|Coal", "Secondary Energy|Electricity|Coal|w/ CCS", "Secondary Energy|Electricity|Coal|w/o CCS", "Secondary Energy|Electricity|Fossil", "Secondary Energy|Electricity|Fossil|w/ CCS", "Secondary Energy|Electricity|Fossil|w/o CCS", "Secondary Energy|Electricity|Gas", "Secondary Energy|Electricity|Gas|w/ CCS", "Secondary Energy|Electricity|Gas|w/o CCS", "Secondary Energy|Electricity|Hydro", "Secondary Energy|Electricity|Non-Biomass Renewables", "Secondary Energy|Electricity|Nuclear", "Secondary Energy|Electricity|Oil", "Secondary Energy|Electricity|Oil|w/ CCS", "Secondary Energy|Electricity|Oil|w/o CCS", "Secondary Energy|Electricity|Secondary Energy Trade", "Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity|Wind")
vars_transport <- c("Final Energy|Transportation", "Emissions|CO2|Energy|Demand|Transportation")
vars_capacity <- c("Capacity|Electricity", "Capacity|Electricity|Biomass|w/ CCS", "Capacity|Electricity|Biomass|w/o CCS", "Capacity|Electricity|Coal|w/ CCS", "Capacity|Electricity|Coal|w/o CCS", "Capacity|Electricity|Gas|w/ CCS", "Capacity|Electricity|Gas|w/o CCS", "Capacity|Electricity|Hydro", "Capacity|Electricity|Nuclear", "Capacity|Electricity|Solar|PV", "Capacity|Electricity|Wind", "Capacity|Electricity|Wind|Onshore")
vars_primary <- c("Primary Energy", "Primary Energy|Biomass", "Primary Energy|Biomass|w/ CCS", "Primary Energy|Biomass|w/o CCS", "Primary Energy|Coal", "Primary Energy|Coal|w/ CCS", "Primary Energy|Coal|w/o CCS", "Primary Energy|Fossil", "Primary Energy|Fossil|w/ CCS", "Primary Energy|Fossil|w/o CCS", "Primary Energy|Gas", "Primary Energy|Gas|w/ CCS", "Primary Energy|Gas|w/o CCS", "Primary Energy|Hydro", "Primary Energy|Non-Biomass Renewables", "Primary Energy|Nuclear", "Primary Energy|Oil", "Primary Energy|Oil|w/ CCS", "Primary Energy|Oil|w/o CCS", "Primary Energy|Secondary Energy Trade", "Primary Energy|Solar", "Primary Energy|Wind")
vars_AFOLU <- c("Emissions|CO2|AFOLU", "Emissions|CH4|AFOLU", "Emissions|N2O|AFOLU")

# make separate csv files for each model
data_AIM <- subset(data_cd_V3, MODEL=="AIM/CGE")
data_COPPE <- subset(data_cd_V3, MODEL=="COPPE-COFFEE 1.0")
data_DNE <- subset(data_cd_V3, MODEL=="DNE21+ V.14")
#data_DNE_proc <- subset(data_cd_V3_proc, model=="DNE21+ V.14")
data_GEM3 <- subset(data_cd_V3, MODEL=="GEM-E3_V1")
data_IMAGE <- subset(data_cd_V3, MODEL=="IMAGE 3.0")
data_MESSAGE <- subset(data_cd_V3, MODEL=="MESSAGEix-GLOBIOM_1.0" | MODEL=="MESSAGE-GLOBIOM_1.0")
data_REMIND <- subset(data_cd_V3, MODEL=="REMIND-MAgPIE 1.7-3.0")
data_WITCH <- subset(data_cd_V3, MODEL=="WITCH2016")
write.table(data_AIM, file = "data/WP3_3/data_AIM.csv", sep=";", row.names = F)
write.table(data_DNE, file = "data/WP3_3/data_DNE.csv", sep=";", row.names = F)
write.table(data_COPPE, file = "data/WP3_3/data_COPPE.csv", sep=";", row.names = F)
write.table(data_DNE, file = "data/WP3_3/data_DNE.csv", sep=";", row.names = F)
write.table(data_GEM3, file = "data/WP3_3/data_GEM3.csv", sep=";", row.names = F)
write.table(data_IMAGE, file = "data/WP3_3/data_IMAGE.csv", sep=";", row.names = F)
write.table(data_MESSAGE, file = "data/WP3_3/data_MESSAGE.csv", sep=";", row.names = F)
write.table(data_REMIND, file = "data/WP3_3/data_REMIND.csv", sep=";", row.names = F)
write.table(data_WITCH, file = "data/WP3_3/data_WITCH.csv", sep=";", row.names = F)

# make separate csv files for each scenario
data_NPi_V3 <- subset(data_cd_V3, scenario=="NPi_V3" & variable=="Emissions|CO2")
data_NPip_V3 <- subset(data_cd_V3, scenario=="NPip_V3" & variable=="Emissions|CO2")
data_INDCi_V3 <- subset(data_cd_V3, scenario=="INDCi_V3")
write.table(data_NPi_V3, file = "data/WP3_3/data_NPi_V3.csv", sep=";", row.names = F)
write.table(data_NPip_V3, file = "data/WP3_3/data_NPip_V3.csv", sep=";", row.names = F)
write.table(data_INDCi_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)

data_NPi2020_400_V3 <- subset(data_cd_V3, scenario=="NPi2020_400_V3" & variable=="Emissions|CO2")
data_NPi2020_1000_V3 <- subset(data_cd_V3, scenario=="NPi2020_1000_V3" & variable=="Emissions|CO2")
data_NPi2020_1600_V3 <- subset(data_cd_V3, scenario=="NPi2020_1600_V3" & variable=="Emissions|CO2")
data_INDCi2030_1000_V3 <- subset(data_cd_V3, scenario=="INDCi2030_1000_V3" & variable=="Emissions|CO2")
data_INDCi2030_1600_V3 <- subset(data_cd_V3, scenario=="INDCi2030_1600_V3" & variable=="Emissions|CO2")
write.table(data_NPi2020_400_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)
write.table(data_NPi2020_1000_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)
write.table(data_NPi2020_1600_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)
write.table(data_INDCi2030_1000_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)
write.table(data_INDCi2030_1600_V3, file = "data/WP3_3/data_INDCi_V3.csv", sep=";", row.names = F)

# first round, includes CO2, CO2|AFOLU and Kyoto emissions
data_carbon_budget <- subset(data_cd_V3, region %in% regs)
data_carbon_budget <- subset(data_carbon_budget, scenario %in% scens)
data_carbon_budget <- subset(data_carbon_budget, variable %in% vars)
write.table(data_carbon_budget, file = "data/WP3_3/data_carbon_budget.csv", sep=";", row.names = F)

# second round, includes CO2, CO2|AFOLU and Kyoto emissions
data_carbon_budget_V3 <- subset(data_cd_V3, region %in% regs)
data_carbon_budget_V3 <- subset(data_carbon_budget_V3, scenario %in% scens_V3)
data_carbon_budget_V3 <- subset(data_carbon_budget_V3, variable %in% vars)
write.table(data_carbon_budget_V3, file = "data/WP3_3/data_carbon_budget_V3.csv", sep=";", row.names = F)

# this file also include the mitigation scenario
data_carbon_budget_ext_V3 <- subset(data_cd_V3, region %in% regs)
data_carbon_budget_ext_V3 <- subset(data_carbon_budget_ext_V3, scenario %in% scens_ext_V3)
data_carbon_budget_ext_V3 <- subset(data_carbon_budget_ext_V3, variable %in% vars)
write.table(data_carbon_budget_ext_V3, file = "data/WP3_3/data_carbon_budget_ext_V3.csv", sep=";", row.names = F)

# kaya data for export
data_kaya <- subset(data_cd_V3, region %in% regs_kaya)
data_kaya <- subset(data_kaya, scenario %in% scens_kaya)
data_kaya <- subset(data_kaya, variable %in% vars_kaya)
#data_kaya <- subset(data_kaya, MODEL %in% models)
write.table(data_kaya, file = "data/WP3_3/data_kaya.csv", sep=";", row.names = F)

# get electricity supply
data_electricity <- subset(data_cd_V3, region %in% regs)
data_electricity <- subset(data_electricity, scenario %in% scens_REN)
data_electricity <- subset(data_electricity, variable %in% vars_transport)
write.table(data_electricity, file = "data/WP3_3/data_electricity.csv", sep=";", row.names = F)

# get transport data
data_transport <- subset(data_cd_V3, region %in% regs)
data_transport <- subset(data_transport, scenario %in% scens_REN)
data_transport <- subset(data_transport, variable %in% vars_transport)
write.table(data_transport, file = "data/WP3_3/data_transport.csv", sep=";", row.names = F)

# get capacity data to check IND NF target
data_capacity <- subset(data_cd_V3, region %in% regs_REN)
data_capacity <- subset(data_capacity, scenario %in% scens_REN)
data_capacity <- subset(data_capacity, variable %in% vars_capacity)
write.table(data_capacity, file = "data/WP3_3/data_capacity.csv", sep=";", row.names = F)
# AFOLU
data_AFOLU <- subset(data_cd_V3, region %in% regs_REN)
data_AFOLU <- subset(data_AFOLU, scenario %in% scens_REN)
data_AFOLU <- subset(data_AFOLU, variable %in% vars_AFOLU)
write.table(data_AFOLU, file = "data/WP3_3/data_AFOLU.csv", sep=";", row.names = F)

# get primary energy data to check CHN NF target
data_primary <- subset(data_cd_V3, region %in% regs_REN)
data_primary <- subset(data_primary, scenario %in% scens_REN)
data_primary <- subset(data_primary, variable %in% vars_primary)
write.table(data_primary, file = "data/WP3_3/data_primary.csv", sep=";", row.names = F)

