
##################################
##### cross-cut plotting
##################################

source("functions/plot_functions_xcut_wp2.R")



# regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA",  "World")
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

source("functions/calcRegion.R")

# vars <- c("Reduction rel to 2010", "relative Abatement|CO2", "Mitigation Costs", "Price|Carbon"  )
# var_labels <- c("Red. rel to 2010 [%]","Red. rel to Base [%]","Migation Costs [% of GDP]","CO2 Price [$/tCO2]" )

# #Scenario overview
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="2030_low",year=2050,file_pre="Comp2050_2030_low", var.labels = var_labels, b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="2030_high",year=2050,file_pre="Comp2050_2030_high", var.labels = var_labels, b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="2020_low",year=2050,file_pre="Comp2050_2020_low", var.labels = var_labels, b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="2020_high",year=2050,file_pre="Comp2050_2020_high", var.labels = var_labels, b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="NPi",year=2050,file_pre="Comp2050_NPi", var.labels = var_labels, b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=vars,cats="INDC",year=2050,file_pre="Comp2050_INDC", var.labels = var_labels, b.multivar = T)

# #Final energy + carbon intensity of FE
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_low",
#              year=2050,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_high",
#              year=2050,file_pre="Comp2050_FE_CI_2030_high", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2020_low",
#              year=2050,file_pre="Comp2050_FE_CI_2020_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2020_high",
#              year=2050,file_pre="Comp2050_FE_CI_2020_high", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="NPi",
#              year=2050,file_pre="Comp2050_FE_CI_NPi", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="INDC",
#              year=2050,file_pre="Comp2050_FE_CI_INDC", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)

#Share of electricity in final energy and transport
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2030_low",
             year=2050,file_pre="Elec_FE_Trans2050_2030_low", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2030_high",
             year=2050,file_pre="Elec_FE_Trans2050_2030_high", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2020_low",
             year=2050,file_pre="Elec_FE_Trans2050_2020_low", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2020_high",
             year=2050,file_pre="Elec_FE_Trans2050_2020_high", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="NPi",
             year=2050,file_pre="Elec_FE_Trans2050_NPi", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="INDC",
             year=2050,file_pre="Elec_FE_Trans2050_INDC", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)

# #Share of electricity in final energy
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2030_low",
#              year=2050,file_pre="ShElec_FE2050_2030_low", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2030_high",
#              year=2050,file_pre="ShElec_FE2050_2030_high", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2020_low",
#              year=2050,file_pre="ShElec_FE2050_2020_low", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2020_high",
#              year=2050,file_pre="ShElec_FE2050_2020_high", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="NPi",
#              year=2050,file_pre="ShElec_FE2050_NPi", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="INDC",
#              year=2050,file_pre="ShElec_FE2050_INDC", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
# 
# #Share of electricity in transport
# all[model == "WITCH" & variable == "Share of Elec in Transport" ]$value = NA
# plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in Transport"),cats="2030_low",
#              year=2050,file_pre="ShElec_FETrans2050_2030_low", var.labels = c("Share of Electricity in FE [%]") , b.multivar = T)

#Shares of wind, solar, nuclear
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="2030_low",year=2050,file_pre="Nuc_WS_share2050_2030_low", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="2030_high",year=2050,file_pre="Nuc_WS_share2050_2030_high", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="2020_low",year=2050,file_pre="Nuc_WS_share2050_2020_low", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="2020_high",year=2050,file_pre="Nuc_WS_share2050_2020_high", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="NPi",year=2050,file_pre="Nuc_WS_share2050_NPi", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
#              cats="INDC",year=2050,file_pre="Nuc_WS_share2050_INDC", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)

# #Other GHG emissions per capita 
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2030_low",
#              year=2050,file_pre="oGHG_2050_2030_low", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2030_high",
#              year=2050,file_pre="oGHG_2050_2030_high", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2020_low",
#              year=2050,file_pre="oGHG_2050_2020_low", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2020_high",
#              year=2050,file_pre="oGHG_2050_2020_high", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="NPi",
#              year=2050,file_pre="oGHG_2050_NPi", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="INDC",
#              year=2050,file_pre="oGHG_2050_INDC", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
# 
# #Total GHG emissions
# regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
#              year=2050,file_pre="GHGEmiMult2050_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_high",
#              year=2050,file_pre="GHGEmiMult2050_2030_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_low",
#              year=2050,file_pre="GHGEmiMult2050_2020_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_high",
#              year=2050,file_pre="GHGEmiMult2050_2020_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
#              year=2050,file_pre="GHGEmiMult2050_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC",
#              year=2050,file_pre="GHGEmiMult2050_INDC", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# 
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
#              year=2030,file_pre="GHGEmiMult2030_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_high",
#              year=2030,file_pre="GHGEmiMult2030_2030_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_low",
#              year=2030,file_pre="GHGEmiMult2030_2020_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_high",
#              year=2030,file_pre="GHGEmiMult2030_2020_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
#              year=2030,file_pre="GHGEmiMult2030_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC",
#              year=2030,file_pre="GHGEmiMult2030_INDC", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# 
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
#              year=2010,file_pre="GHGEmiMult2010_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_high",
#              year=2010,file_pre="GHGEmiMult2010_2030_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_low",
#              year=2010,file_pre="GHGEmiMult2010_2020_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2020_high",
#              year=2010,file_pre="GHGEmiMult2010_2020_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
#              year=2010,file_pre="GHGEmiMult2010_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC",
#              year=2010,file_pre="GHGEmiMult2010_INDC", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
#              year=2015,file_pre="GHGEmiMult2015_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)

#oASIA <-  calcRegion(all[Scope == "global"], ' `oASIA` ~ `R5ASIA` - `IND` - `CHN` ', b.append = F)

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

#Carbon intensity of final energy
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_low",year=2020,file_pre="CI_2020_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_low",year=2030,file_pre="CI_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_low",year=2050,file_pre="CI_2050_2030_low")
# 
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_high",year=2020,file_pre="CI_2020_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_high",year=2030,file_pre="CI_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_high",year=2050,file_pre="CI_2050_2030_high")
# 
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_low",year=2020,file_pre="CI_2020_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_low",year=2030,file_pre="CI_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_low",year=2050,file_pre="CI_2050_2020_low")
# 
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_high",year=2020,file_pre="CI_2020_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_high",year=2030,file_pre="CI_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2020_high",year=2050,file_pre="CI_2050_2020_high")
# 
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2020,file_pre="CI_2020_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2030,file_pre="CI_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2050,file_pre="CI_2050_NPi")
# 
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC",year=2020,file_pre="CI_2020_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC",year=2030,file_pre="CI_2030_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC",year=2050,file_pre="CI_2050_INDC")


# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of Electricity",cats="2030_low",year=2050,file_pre="CI_Elec_2050")

#Shares of wind, solar, nuclear 2030 and 2050
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_low",year=2030,file_pre="WSshare_Elec_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_low",year=2050,file_pre="WSshare_Elec_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_low",year=2050,file_pre="Nuclearshare_Elec_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_low",year=2030,file_pre="Nuclearshare_Elec_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_high",year=2030,file_pre="WSshare_Elec_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_high",year=2050,file_pre="WSshare_Elec_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_high",year=2050,file_pre="Nuclearshare_Elec_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_high",year=2030,file_pre="Nuclearshare_Elec_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2020_low",year=2030,file_pre="WSshare_Elec_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2020_low",year=2050,file_pre="WSshare_Elec_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2020_low",year=2050,file_pre="Nuclearshare_Elec_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2020_low",year=2030,file_pre="Nuclearshare_Elec_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2020_high",year=2030,file_pre="WSshare_Elec_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2020_high",year=2050,file_pre="WSshare_Elec_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2020_high",year=2050,file_pre="Nuclearshare_Elec_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2020_high",year=2030,file_pre="Nuclearshare_Elec_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi",year=2030,file_pre="WSshare_Elec_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi",year=2050,file_pre="WSshare_Elec_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi",year=2050,file_pre="Nuclearshare_Elec_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi",year=2030,file_pre="Nuclearshare_Elec_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC",year=2030,file_pre="WSshare_Elec_2030_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC",year=2050,file_pre="WSshare_Elec_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC",year=2050,file_pre="Nuclearshare_Elec_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC",year=2030,file_pre="Nuclearshare_Elec_2030_INDC")

#Energy intensity of GDP
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2020,file_pre="EI_2020_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2030,file_pre="EI_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2050,file_pre="EI_2050_2030_low")
# 
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_high",year=2020,file_pre="EI_2020_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_high",year=2030,file_pre="EI_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_high",year=2050,file_pre="EI_2050_2030_high")
# 
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_low",year=2020,file_pre="EI_2020_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_low",year=2030,file_pre="EI_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_low",year=2050,file_pre="EI_2050_2020_low")
# 
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_high",year=2020,file_pre="EI_2020_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_high",year=2030,file_pre="EI_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2020_high",year=2050,file_pre="EI_2050_2020_high")
# 
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2020,file_pre="EI_2020_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2030,file_pre="EI_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2050,file_pre="EI_2050_NPi")
# 
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC",year=2020,file_pre="EI_2020_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC",year=2030,file_pre="EI_2030_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC",year=2050,file_pre="EI_2050_INDC")
# 
# #Emissions per capita
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2010,file_pre="EmiCap_2010_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2030,file_pre="EmiCap_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2050,file_pre="EmiCap_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_high",year=2010,file_pre="EmiCap_2010_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_high",year=2030,file_pre="EmiCap_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_high",year=2050,file_pre="EmiCap_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_low",year=2010,file_pre="EmiCap_2010_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_low",year=2030,file_pre="EmiCap_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_low",year=2050,file_pre="EmiCap_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_high",year=2010,file_pre="EmiCap_2010_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_high",year=2030,file_pre="EmiCap_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2020_high",year=2050,file_pre="EmiCap_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2010,file_pre="EmiCap_2010_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2030,file_pre="EmiCap_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2050,file_pre="EmiCap_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC",year=2010,file_pre="EmiCap_2010_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC",year=2030,file_pre="EmiCap_2030_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC",year=2050,file_pre="EmiCap_2050_INDC")
# 
# #Final energy per capita
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="2030_low",year=2050,file_pre="FE per Cap_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="2030_high",year=2050,file_pre="FE per Cap_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="2020_low",year=2050,file_pre="FE per Cap_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="2020_high",year=2050,file_pre="FE per Cap_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="NPi",year=2050,file_pre="FE per Cap_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="INDC",year=2050,file_pre="FE per Cap_2050_INDC")
# 
# #Fossil emissions per capita
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="2030_low",year=2050,file_pre="FossEmiCap_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="2030_high",year=2050,file_pre="FossEmiCap_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="2020_low",year=2050,file_pre="FossEmiCap_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="2020_high",year=2050,file_pre="FossEmiCap_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="NPi",year=2050,file_pre="FossEmiCap_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="INDC",year=2050,file_pre="FossEmiCap_2050_INDC")
# 
# #Emissions per capita reference vs. mitigation
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_low"),
#              year=2030,b.multicat = T, file_pre="EmiCap_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_low"),
#              year=2050,b.multicat = T, file_pre="EmiCap_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_high"),
#              year=2030,b.multicat = T, file_pre="EmiCap_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_high"),
#              year=2050,b.multicat = T, file_pre="EmiCap_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2020_low"),
#              year=2030,b.multicat = T, file_pre="EmiCap_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2020_low"),
#              year=2050,b.multicat = T, file_pre="EmiCap_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2020_high"),
#              year=2030,b.multicat = T, file_pre="EmiCap_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2020_high"),
#              year=2050,b.multicat = T, file_pre="EmiCap_2050_2020_high")
# 
# #BECCS per capita
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="2030_low",year=2050,file_pre="BECCSCap_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="2030_high",year=2050,file_pre="BECCSCap_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="2020_low",year=2050,file_pre="BECCSCap_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="2020_high",year=2050,file_pre="BECCSCap_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="NPi",year=2050,file_pre="BECCSCap_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="INDC",year=2050,file_pre="BECCSCap_2050_INDC")
# 
# # plot rel. Abatement
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="2030_low",year=2050,file_pre="relEmi_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="2030_high",year=2050,file_pre="relEmi_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="2020_low",year=2050,file_pre="relEmi_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="2020_high",year=2050,file_pre="relEmi_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="NPi",year=2050,file_pre="relEmi_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="INDC",year=2050,file_pre="relEmi_2050_INDC")
# 
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_low",year=2030,file_pre="relAb_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_low",year=2050,file_pre="relAb_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_high",year=2030,file_pre="relAb_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_high",year=2050,file_pre="relAb_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2020_low",year=2030,file_pre="relAb_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2020_low",year=2050,file_pre="relAb_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2020_high",year=2030,file_pre="relAb_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2020_high",year=2050,file_pre="relAb_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi",year=2030,file_pre="relAb_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi",year=2050,file_pre="relAb_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC",year=2030,file_pre="relAb_2030_INDC")
# plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC",year=2050,file_pre="relAb_2050_INDC")

# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_low",year=2050,file_pre="EmiRel2010_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_low",year=2030,file_pre="EmiRel2010_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_high",year=2050,file_pre="EmiRel2010_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_high",year=2030,file_pre="EmiRel2010_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2020_low",year=2050,file_pre="EmiRel2010_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2020_low",year=2030,file_pre="EmiRel2010_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2020_high",year=2050,file_pre="EmiRel2010_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2020_high",year=2030,file_pre="EmiRel2010_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi",year=2050,file_pre="EmiRel2010_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi",year=2030,file_pre="EmiRel2010_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC",year=2050,file_pre="EmiRel2010_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC",year=2030,file_pre="EmiRel2010_2030_INDC")

#plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_low",year=2050,file_pre="relAb_2050")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_low",year=2050,file_pre="RedRel2010_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_high",year=2050,file_pre="RedRel2010_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2020_low",year=2050,file_pre="RedRel2010_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2020_high",year=2050,file_pre="RedRel2010_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="NPi",year=2050,file_pre="RedRel2010_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDC",year=2050,file_pre="RedRel2010_2050_INDC")

# plot carbon prices



# multi facet plot of


regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA","World")
#Mitigation costs
dt=all[!model=="IMAGE 3.0"] #add later when IMAGE costs are ok
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2030_low",year=2050,file_pre="MitiCosts_2050_2030_low",b.multivar = T,var.labels = "Mitigation costs 2050 - 2030_low")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2030_high",year=2050,file_pre="MitiCosts_2050_2030_high",b.multivar = T,var.labels = "Mitigation costs 2050 - 2030_high")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2020_low",year=2050,file_pre="MitiCosts_2050_2020_low",b.multivar = T,var.labels = "Mitigation costs 2050 - 2020_low")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2020_high",year=2050,file_pre="MitiCosts_2050_2020_high",b.multivar = T,var.labels = "Mitigation costs 2050 - 2020_high")

plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2030_low",year=2100,file_pre="MitiCosts_2100_2030_low",b.multivar = T,var.labels = "Mitigation costs 2100 - 2030_low")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2030_high",year=2100,file_pre="MitiCosts_2100_2030_high",b.multivar = T,var.labels = "Mitigation costs 2100 - 2030_high")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2020_low",year=2100,file_pre="MitiCosts_2100_2020_low",b.multivar = T,var.labels = "Mitigation costs 2100 - 2020_low")
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats="2020_high",year=2100,file_pre="MitiCosts_2100_2020_high",b.multivar = T,var.labels = "Mitigation costs 2100 - 2020_high")

plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="MitiCosts_2100_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=dt,vars="Mitigation Costs",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2050,file_pre="MitiCosts_2050_mitigscens",b.multicat = T)

plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("INDC2030ip_1000","2030_low"),year=2050,file_pre="MitiCosts_2050_INDCip",b.multicat = T)
plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("2020_low","2030_low"),year=2050,file_pre="MitiCosts_2050_low",b.multicat = T)
plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("2020_low","2020_verylow"),year=2050,file_pre="MitiCosts_2050_verylow",b.multicat = T)
plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("INDC2030ip_1000","2030_low"),year=2100,file_pre="MitiCosts_2100_INDCip",b.multicat = T)
plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("2020_low","2030_low"),year=2100,file_pre="MitiCosts_2100_low",b.multicat = T)
plot_boxplot(regs="World",dt=dt,vars="Mitigation Costs",cats=c("2020_low","2020_verylow"),year=2100,file_pre="MitiCosts_2100_verylow",b.multicat = T)

#Carbon price
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="2030_low",year=2050,file_pre="CO2price_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="2030_high",year=2050,file_pre="CO2price_2050_2030_high")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="2020_low",year=2050,file_pre="CO2price_2050_2020_low")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="2020_high",year=2050,file_pre="CO2price_2050_2020_high")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="NPi",year=2050,file_pre="CO2price_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="INDC",year=2050,file_pre="CO2price_2050_INDC")

plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="CO2price_2100_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2050,file_pre="CO2_2050_mitigscens",b.multicat = T)

# blank carbon price results for EU

# plot CI over EI indicator
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2030_low",year=2050,file_pre="ci_ei_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2030_low",year=2030,file_pre="ci_ei_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2030_high",year=2050,file_pre="ci_ei_2050_2030_high")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2030_high",year=2030,file_pre="ci_ei_2030_2030_high")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2020_low",year=2050,file_pre="ci_ei_2050_2020_low")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2020_low",year=2030,file_pre="ci_ei_2030_2020_low")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2020_high",year=2050,file_pre="ci_ei_2050_2020_high")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2020_high",year=2030,file_pre="ci_ei_2030_2020_high")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi",year=2050,file_pre="ci_ei_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi",year=2030,file_pre="ci_ei_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC",year=2050,file_pre="ci_ei_2050_INDC")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC",year=2030,file_pre="ci_ei_2030_INDC")

#regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA")
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA",  "World")

# #BECCS per capita
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="2030_low",year=2050,file_pre="BECCS_EmiAndPE_2030_low", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="2030_high",year=2050,file_pre="BECCS_EmiAndPE_2030_high", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="2020_low",year=2050,file_pre="BECCS_EmiAndPE_2020_low", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="2020_high",year=2050,file_pre="BECCS_EmiAndPE_2020_high", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="NPi",year=2050,file_pre="BECCS_EmiAndPE_NPi", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
#              cats="INDC",year=2050,file_pre="BECCS_EmiAndPE_INDC", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)


# BECCS
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="2030_low",year=2050,file_pre="BECCS_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2030_low",year=2050,file_pre="PE_BECCS_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2030_low",year=2050,file_pre="PE_Bio_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="2030_high",year=2050,file_pre="BECCS_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2030_high",year=2050,file_pre="PE_BECCS_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2030_high",year=2050,file_pre="PE_Bio_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="2020_low",year=2050,file_pre="BECCS_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2020_low",year=2050,file_pre="PE_BECCS_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2020_low",year=2050,file_pre="PE_Bio_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="2020_high",year=2050,file_pre="BECCS_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2020_high",year=2050,file_pre="PE_BECCS_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2020_high",year=2050,file_pre="PE_Bio_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="NPi",year=2050,file_pre="BECCS_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="NPi",year=2050,file_pre="PE_BECCS_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="NPi",year=2050,file_pre="PE_Bio_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="INDC",year=2050,file_pre="BECCS_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="INDC",year=2050,file_pre="PE_BECCS_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="INDC",year=2050,file_pre="PE_Bio_2050_INDC")

#Population
# plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2010,file_pre="Pop_2010")
# plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2030,file_pre="Pop_2030")
# plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2050,file_pre="Pop_2050")

# #Carbon budget
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_low",year=2050,file_pre="Cbudget_2011-2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_low",year=2100,file_pre="Cbudget_2011-2100_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_high",year=2050,file_pre="Cbudget_2011-2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_high",year=2100,file_pre="Cbudget_2011-2100_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2020_low",year=2050,file_pre="Cbudget_2011-2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2020_low",year=2100,file_pre="Cbudget_2011-2100_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2020_high",year=2050,file_pre="Cbudget_2011-2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2020_high",year=2100,file_pre="Cbudget_2011-2100_2020_high")

#Carbon budget
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget",cats="2030_low",year=2100,file_pre="CO2budget_2011-2100_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget",cats="2030_high",year=2100,file_pre="CO2budget_2011-2100_2030_high")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget",cats="2020_low",year=2100,file_pre="CO2budget_2011-2100_2020_low")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget",cats="2020_high",year=2100,file_pre="CO2budget_2011-2100_2020_high")

# #Emissions intensity GDP
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA","World")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_low",year=2050,file_pre="Emis_int2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_low",year=2030,file_pre="Emis_int2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_high",year=2050,file_pre="Emis_int2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_high",year=2030,file_pre="Emis_int2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2020_low",year=2050,file_pre="Emis_int2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2020_low",year=2030,file_pre="Emis_int2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2020_high",year=2050,file_pre="Emis_int2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2020_high",year=2030,file_pre="Emis_int2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi",year=2050,file_pre="Emis_int2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi",year=2030,file_pre="Emis_int2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC",year=2050,file_pre="Emis_int2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC",year=2030,file_pre="Emis_int2030_INDC")

#Rates of change
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2030_low",
#              year="2030-2050",file_pre="Rates2030-2050_2030_low", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2030_high",
#              year="2030-2050",file_pre="Rates2030-2050_2030_high", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2020_low",
#              year="2030-2050",file_pre="Rates2030-2050_2020_low", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2020_high",
#              year="2030-2050",file_pre="Rates2030-2050_2020_high", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi",
#              year="2030-2050",file_pre="Rates2030-2050_NPi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC",
#              year="2030-2050",file_pre="Rates2030-2050_INDC", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# 
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2030_low",
#              year="2050-2100",file_pre="Rates2050-2100_2030_low", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2030_high",
#              year="2050-2100",file_pre="Rates2050-2100_2030_high", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2020_low",
#              year="2050-2100",file_pre="Rates2050-2100_2020_low", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2020_high",
#              year="2050-2100",file_pre="Rates2050-2100_2020_high", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi",
#              year="2050-2100",file_pre="Rates2050-2100_NPi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
# plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC",
#              year="2050-2100",file_pre="Rates2050-2100_INDC", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)

#Primary energy
#regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_low",year=2050,file_pre="PE_2050_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_low",year=2030,file_pre="PE_2030_2030_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_high",year=2050,file_pre="PE_2050_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_high",year=2030,file_pre="PE_2030_2030_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2020_low",year=2050,file_pre="PE_2050_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2020_low",year=2030,file_pre="PE_2030_2020_low")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2020_high",year=2050,file_pre="PE_2050_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2020_high",year=2030,file_pre="PE_2030_2020_high")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi",year=2050,file_pre="PE_2050_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi",year=2030,file_pre="PE_2030_NPi")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC",year=2050,file_pre="PE_2050_INDC")
# plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC",year=2030,file_pre="PE_2030_INDC")

#Peak year
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA","World")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="2030_low",file_pre="Peak_year_CO2_2030_low")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="2030_high",file_pre="Peak_year_CO2_2030_high")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="2020_low",file_pre="Peak_year_CO2_2020_low")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="2020_high",file_pre="Peak_year_CO2_2020_high")
# 
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="2030_low",file_pre="Peak_year_Kyoto_2030_low")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="2030_high",file_pre="Peak_year_Kyoto_2030_high")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="2020_low",file_pre="Peak_year_Kyoto_2020_low")
# plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="2020_high",file_pre="Peak_year_Kyoto_2020_high")

#Regional emissions
plot_boxplot3(regs="BRA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_BRA")
plot_boxplot3(regs="BRA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_BRA")
plot_boxplot3(regs="JPN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_JPN")
plot_boxplot3(regs="JPN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_JPN")
plot_boxplot3(regs="IND",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_IND")
plot_boxplot3(regs="IND",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_IND")
plot_boxplot3(regs="CHN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_CHN")
plot_boxplot3(regs="CHN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_CHN")
plot_boxplot3(regs="RUS",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_RUS")
plot_boxplot3(regs="RUS",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_RUS")
plot_boxplot3(regs="EU",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_EU")
plot_boxplot3(regs="EU",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_EU")
plot_boxplot3(regs="USA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_USA")
plot_boxplot3(regs="USA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_USA")
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_World")
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_World")

plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_US_CHN_IND_EU",ylim=T)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_JPN_BRA_RUS",ylim=T)
plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2050,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2050_US_CHN_IND_EU",ylim=T)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2050,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2050_JPN_BRA_RUS",ylim=T)
plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2100,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2100_US_CHN_IND_EU",ylim=T)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2100,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2100_JPN_BRA_RUS",ylim=T)

plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2030_US_CHN_IND_EU",ylim=T)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2030_JPN_BRA_RUS",ylim=T)
plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2050,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2050_US_CHN_IND_EU",ylim=F)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2050,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2050_JPN_BRA_RUS",ylim=F)
plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2100,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2100_US_CHN_IND_EU",ylim=F)
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("2020_high","2020_low","2030_high","2030_low","2020_verylow"),
              year=2100,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_mitig_2100_JPN_BRA_RUS",ylim=F)

#Regional energy
plot_boxplot3(regs="BRA",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_BRA",ylim=F)
plot_boxplot3(regs="JPN",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_JPN",ylim=F)
plot_boxplot3(regs="IND",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_IND",ylim=F)
plot_boxplot3(regs="CHN",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_CHN",ylim=F)
plot_boxplot3(regs="RUS",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_RUS",ylim=F)
plot_boxplot3(regs="EU",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_EU",ylim=F)
plot_boxplot3(regs="USA",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_USA",ylim=F)

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")
plot_boxplot(regs=regs,dt=all,vars="Renewables Share|TPES|Excl. Nuclear & Geothermal",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2050,file_pre="REN_TPES_2050_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Renewables Share|TPES|Excl. Nuclear & Geothermal",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="REN_TPES_2100_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Low-carbon Electricity Share|All excl. Fossil w/o CCS",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2050,file_pre="LowC_Elec_2050_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Low-carbon Electricity Share|All excl. Fossil w/o CCS",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="LowC_Elec_2100_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Renewables Share|Excl. Nuclear",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2050,file_pre="REN_Elec_2050_mitigscens",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Renewables Share|Excl. Nuclear",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="REN_Elec_2100_mitigscens",b.multicat = T)


#Sectoral emissions
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_World",ylim=T)
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2020,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2020_World",ylim=T)
plot_boxplot3(regs="BRA",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_BRA",ylim=T)
plot_boxplot3(regs="CHN",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_CHN",ylim=T)
plot_boxplot3(regs="IND",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_IND",ylim=T)
plot_boxplot3(regs="JPN",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_JPN",ylim=T)
plot_boxplot3(regs="RUS",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_RUS",ylim=T)
plot_boxplot3(regs="USA",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_USA",ylim=T)
plot_boxplot3(regs="EU",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NoPOL","NPi","INDC"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_EU",ylim=T)

###Sectoral energy
#Energy supply
plot_boxplot3(regs="World",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2020_World")
#Electricity only
plot_boxplot3(regs="World",dt=all,vars=c("Secondary Energy|Electricity","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Low-carbon Electricity Share|All excl. Fossil w/o CCS"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Wind & Solar Share","Renewables Share","Low-carbon share"),file_pre="Electricity_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Secondary Energy|Electricity","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Low-carbon Electricity Share|All excl. Fossil w/o CCS"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Wind & Solar Share","Renewables Share","Low-carbon share"),file_pre="Electricity_2020_World")

#AFOLU
plot_boxplot3(regs="World",dt=all,vars=c("Land Cover","Land Cover|Cropland","Land Cover|Cropland|Energy Crops","Primary Energy|Biomass|Traditional"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Land Cover","Cropland","Energy Crops","PE|Traditional biomass"),file_pre="AFOLU_indicators_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Land Cover","Land Cover|Cropland","Land Cover|Cropland|Energy Crops","Primary Energy|Biomass|Traditional"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Land Cover","Cropland","Energy Crops","PE|Traditional biomass"),file_pre="AFOLU_indicators_2020_World")

#FE all sectors
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial"),file_pre="FE_sectors_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial"),file_pre="FE_sectors_2020_World")

plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Solids|Biomass","Final Energy|Transportation|Liquids|Biomass"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("FE|Solids|Biomass","FE|Transport|Liquids|Biomass"),file_pre="FE_biomass_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Solids|Biomass","Final Energy|Transportation|Liquids|Biomass"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("FE|Solids|Biomass","FE|Transport|Liquids|Biomass"),file_pre="FE_biomass_2020_World")

#Transport
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation|Electricity","Final Energy|Transportation|Gases","Final Energy|Transportation|Hydrogen","Final Energy|Transportation|Liquids","Final Energy|Transportation|Liquids|Biomass","Final Energy|Transportation|Other"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Liquids-biomass","Other"),file_pre="FE_transport_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation|Electricity","Final Energy|Transportation|Gases","Final Energy|Transportation|Hydrogen","Final Energy|Transportation|Liquids","Final Energy|Transportation|Liquids|Biomass","Final Energy|Transportation|Other"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Liquids-biomass","Other"),file_pre="FE_transport_2020_World")
# Check availability / calculation new snapshot
# plot_boxplot3(regs="World",dt=all,vars=c("FE passenger/pkm","FE freight/tkm"),cats=c("Historical","NoPOL","NPi","INDC"),
#               year=2020,b.multivar=T,var.labels=c("FE passenger/pkm","FE freight/tkm"),file_pre="FE_km_transport_2020_World")
# plot_boxplot3(regs="World",dt=all,vars=c("FE passenger/pkm","FE freight/tkm"),cats=c("Historical","NoPOL","NPi","INDC"),
#               year=2030,b.multivar=T,var.labels=c("FE passenger/pkm","FE freight/tkm"),file_pre="FE_km_transport_2030_World")

#Industry
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Industry|Electricity","Final Energy|Industry|Gases","Final Energy|Industry|Hydrogen","Final Energy|Industry|Liquids","Final Energy|Industry|Other","Final Energy|Industry|Heat","Final Energy|Industry|Solids"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_industry_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Industry|Electricity","Final Energy|Industry|Gases","Final Energy|Industry|Hydrogen","Final Energy|Industry|Liquids","Final Energy|Industry|Other","Final Energy|Industry|Heat","Final Energy|Industry|Solids"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_industry_2020_World")

#Buildings
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial|Gases","Final Energy|Residential and Commercial|Hydrogen","Final Energy|Residential and Commercial|Liquids","Final Energy|Residential and Commercial|Other","Final Energy|Residential and Commercial|Heat","Final Energy|Residential and Commercial|Solids"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_buildings_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial|Gases","Final Energy|Residential and Commercial|Hydrogen","Final Energy|Residential and Commercial|Liquids","Final Energy|Residential and Commercial|Other","Final Energy|Residential and Commercial|Heat","Final Energy|Residential and Commercial|Solids"),cats=c("Historical","NoPOL","NPi","INDC"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_buildings_2020_World")

# dt=all
# var="Emissions per capita"
# cats=c("2030_low", "NPi")
# b_basepol = T


# ### scatter plot based on diagnostics data
# # Please specify exactly two variables and assign them to x and y
# vars <- c(x="Price|Carbon",y="relative Abatement|CO2")
# cats <- c("2030_low")
# # regs <- "BRA"
# regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")
# plot_scatter_xcut(reg=regs,dt=all[as.numeric(period)<=2050],vars_to_spread=vars,cats=cats,title="Relative Abatement",file_pre="relAb_co2pr_scatter",xlog=T,xlim=c(10,1000))
# 
# 
# 
# 
# cats <- c("DIAG-C80-gr5")
# vars <- c(x="Price|Carbon",y="relative Abatement|CO2")
# 
# # vars <- c(x="Price|Carbon",y="rel. Abatatement")
# # diag <- calcRel2Base(diag,var="Emissions|CO2",baseEq1=F,"rel. Abatatement",diag_scens)
# 
# 
# plot_scatter_xcut(reg="BRA",dt=diag[period<=2050 ],vars_to_spread=vars,cats=cats,title="Relative Abatement",file_pre="d_relAb_co2pr_scatter",xlog=F,xlim=c(10,150))
# 

### Using other plot functions
source("functions/plot_functions.R")
#Sectoral emissions
plot_bar_facet(reg="World",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="BRA",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="CHN",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("IND","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="JPN",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="EU",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="RUS",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="USA",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   
plot_bar_facet(reg="IND",dt=all[period==2030],vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                cats=c("NoPOL","NPi"),lab="Sectoral CO2 emissions",file_pre="Sector_CO2_bar_facet")   

# Kaya indicators
vars <- c("GDP per capita|MER","Energy Intensity of GDP|MER","Carbon Intensity of FE","Emissions per capita")
cats <- c("INDC","2020_low","2030_low","2020_verylow")
plot_funnel(reg="World",dt=all[period<=2100],vars=vars,cats=cats,title="Kaya factors",file_pre="kaya_funnel3",glob_lines=T,xlim=c(2000,2100))  

vars <- c("GDP per capita|MER","Energy Intensity of GDP|MER","Carbon Intensity of FE","Emissions per capita")
cats <- c("NPi","INDC","2020_verylow","2020_low","2030_low")
#With CO2_FFI
plot_bar_facet2(reg="World",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2030",file_pre="kaya_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="World",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2050",file_pre="kaya_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="World",dt=all,year=2100,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2100",file_pre="kaya_2100_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="CHN",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2030",file_pre="kaya_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="CHN",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2050",file_pre="kaya_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="USA",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2030",file_pre="kaya_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="USA",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - CO2|FFI; 2050",file_pre="kaya_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))

#With Kyoto gases
vars <- c("GDP per capita|MER","Energy Intensity of GDP|MER","GHG Intensity of FE","GHG emissions per capita")
plot_bar_facet2(reg="World",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - GHG; 2030",file_pre="kaya_GHG_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="World",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - GHG; 2050",file_pre="kaya_GHG_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="World",dt=all,year=2100,vars=vars,cats=cats,lab="Kaya factors - GHG; 2100",file_pre="kaya_GHG_2100_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow")) 
plot_bar_facet2(reg="CHN",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - GHG; 2030",file_pre="kaya_GHG_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="CHN",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - GHG; 2050",file_pre="kaya_GHG_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="USA",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - GHG; 2030",file_pre="kaya_GHG_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))
plot_bar_facet2(reg="USA",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - GHG; 2050",file_pre="kaya_GHG_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2020_low","2030_low","2020_verylow"))

# GHG emissions funnel
dt=all
dt1=dt[Category %in% c("NoPOL","NPi","INDC")&period<2035]
dt2=dt[Category %in% c("2030_low","2020_verylow","2020_low")&period<2055]
dt=rbind(dt1,dt2)

cats <- c("NoPOL")
plot_funnel("World",dt=all,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_LT_NoPOL",glob_lines=T,xlim=c(2000,2100))   
plot_funnel("World",dt=all,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_ST_NoPOL",glob_lines=T,xlim=c(2000,2030),ylim=c(0,70000))   
cats <- c("NoPOL","NPi")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_ST_NoPOL_NPi",glob_lines=T,xlim=c(2000,2030),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_ST_NoPOL_NPi_INDC",glob_lines=T,xlim=c(2000,2030),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC","2020_low")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_MT_NoPOL_NPi_INDC_NPilow",glob_lines=T,xlim=c(2000,2050),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC","2020_low")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_ST_NoPOL_NPi_INDC_NPilow",glob_lines=T,xlim=c(2000,2030),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_MT_NoPOL_NPi_INDC_verylow",glob_lines=T,xlim=c(2000,2050),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_ST_NoPOL_NPi_INDC_verylow",glob_lines=T,xlim=c(2000,2030),ylim=c(0,70000))   
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow","2030_low")
plot_funnel("World",dt=dt,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_MT_NoPOL_NPi_INDC_low",glob_lines=T,xlim=c(2000,2050),ylim=c(0,70000))   
cats <- c("2030_low","INDC2030ip_1000","INDC")
plot_funnel("World",dt=all,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto gas emissions",file_pre="GHG_funnel_LT_INDCs",glob_lines=T,xlim=c(2000,2100),ylim=c(0,70000))   

cats <- c("NoPOL","NPi","INDC")
plot_funnel("World",dt=dt,vars=c("Emissions|CO2"),cats=cats,title="CO2 emissions",file_pre="CO2_funnel_ST_NoPOL_NPi_INDC",glob_lines=T,xlim=c(2000,2030),ylim=c(0,60000))   
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow","2030_low")
plot_funnel("World",dt=dt,vars=c("Emissions|CO2"),cats=cats,title="CO2 emissions",file_pre="CO2_funnel_MT_NoPOL_NPi_INDC_low",glob_lines=T,xlim=c(2000,2050),ylim=c(0,60000))   

cats <- c("NoPOL","NPi","INDC")
plot_funnel("World",dt=dt,vars=c("Emissions|CO2|AFOLU"),cats=cats,title="AFOLU CO2 emissions",file_pre="AFOLU_CO2_funnel_ST_NoPOL_NPi_INDC",glob_lines=T,xlim=c(2000,2030),ylim=c(0,10000))   
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow","2030_low")
plot_funnel("World",dt=dt,vars=c("Emissions|CO2|AFOLU"),cats=cats,title="AFOLU CO2 emissions",file_pre="AFOLU_CO2_funnel_MT_NoPOL_NPi_INDC_low",glob_lines=T,xlim=c(2000,2050),ylim=c(-7500,10000))   
