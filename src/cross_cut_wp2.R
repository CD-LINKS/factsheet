
##################################
##### cross-cut plotting
##################################

source("functions/plot_functions_xcut.R")



# regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA",  "World")
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

source("functions/calcRegion.R")

vars <- c("Reduction rel to 2010", "relative Abatement|CO2", "Mitigation Costs", "Price|Carbon"  )
var_labels <- c("Red. rel to 2010 [%]","Red. rel to Base [%]","Migation Costs [% of GDP]","CO2 Price [$/tCO2]" )

#Scenario overview
plot_boxplot(regs=regs,dt=all,vars=vars,cats="INDC2030i_1000",year=2050,file_pre="Comp2050_INDC2030i_1000", var.labels = var_labels, b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="INDC2030i_1600",year=2050,file_pre="Comp2050_INDC2030i_1600", var.labels = var_labels, b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="NPi2020_1000",year=2050,file_pre="Comp2050_NPi2020_1000", var.labels = var_labels, b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="NPi2020_1600",year=2050,file_pre="Comp2050_NPi2020_1600", var.labels = var_labels, b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="NPi",year=2050,file_pre="Comp2050_NPi", var.labels = var_labels, b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="INDCi",year=2050,file_pre="Comp2050_INDCi", var.labels = var_labels, b.multivar = T)

#Final energy + carbon intensity of FE
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="INDC2030i_1000",
             year=2050,file_pre="Comp2050_FE_CI_INDC2030i_1000", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="INDC2030i_1600",
             year=2050,file_pre="Comp2050_FE_CI_INDC2030i_1600", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="NPi2020_1000",
             year=2050,file_pre="Comp2050_FE_CI_NPi2020_1000", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="NPi2020_1600",
             year=2050,file_pre="Comp2050_FE_CI_NPi2020_1600", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="NPi",
             year=2050,file_pre="Comp2050_FE_CI_NPi", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="INDCi",
             year=2050,file_pre="Comp2050_FE_CI_INDCi", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)

#Share of electricity in final energy and transport
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="INDC2030i_1000",
             year=2050,file_pre="Elec_FE_Trans2050_INDC2030i_1000", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="INDC2030i_1600",
             year=2050,file_pre="Elec_FE_Trans2050_INDC2030i_1600", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="NPi2020_1000",
             year=2050,file_pre="Elec_FE_Trans2050_NPi2020_1000", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="NPi2020_1600",
             year=2050,file_pre="Elec_FE_Trans2050_NPi2020_1600", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="NPi",
             year=2050,file_pre="Elec_FE_Trans2050_NPi", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="INDCi",
             year=2050,file_pre="Elec_FE_Trans2050_INDCi", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)

#Share of electricity in final energy
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="INDC2030i_1000",
             year=2050,file_pre="ShElec_FE2050_INDC2030i_1000", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="INDC2030i_1600",
             year=2050,file_pre="ShElec_FE2050_INDC2030i_1600", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="NPi2020_1000",
             year=2050,file_pre="ShElec_FE2050_NPi2020_1000", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="NPi2020_1600",
             year=2050,file_pre="ShElec_FE2050_NPi2020_1600", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="NPi",
             year=2050,file_pre="ShElec_FE2050_NPi", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="INDCi",
             year=2050,file_pre="ShElec_FE2050_INDCi", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)

#Share of electricity in transport
all[model == "WITCH" & variable == "Share of Elec in Transport" ]$value = NA
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in Transport"),cats="INDC2030i_1000",
             year=2050,file_pre="ShElec_FETrans2050_INDC2030i_1000", var.labels = c("Share of Electricity in FE [%]") , b.multivar = T)

#Shares of wind, solar, nuclear
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="INDC2030i_1000",year=2050,file_pre="Nuc_WS_share2050_INDC2030i_1000", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="INDC2030i_1600",year=2050,file_pre="Nuc_WS_share2050_INDC2030i_1600", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="NPi2020_1000",year=2050,file_pre="Nuc_WS_share2050_NPi2020_1000", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="NPi2020_1600",year=2050,file_pre="Nuc_WS_share2050_NPi2020_1600", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="NPi",year=2050,file_pre="Nuc_WS_share2050_NPi", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="INDCi",year=2050,file_pre="Nuc_WS_share2050_INDCi", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)

#Other GHG emissions per capita 
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="INDC2030i_1000",
             year=2050,file_pre="oGHG_2050_INDC2030i_1000", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="INDC2030i_1600",
             year=2050,file_pre="oGHG_2050_INDC2030i_1600", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="NPi2020_1000",
             year=2050,file_pre="oGHG_2050_NPi2020_1000", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="NPi2020_1600",
             year=2050,file_pre="oGHG_2050_NPi2020_1600", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="NPi",
             year=2050,file_pre="oGHG_2050_NPi", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="INDCi",
             year=2050,file_pre="oGHG_2050_INDCi", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)

#Total GHG emissions
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1000",
             year=2050,file_pre="GHGEmiMult2050_INDC2030i_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1600",
             year=2050,file_pre="GHGEmiMult2050_INDC2030i_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1000",
             year=2050,file_pre="GHGEmiMult2050_NPi2020_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1600",
             year=2050,file_pre="GHGEmiMult2050_NPi2020_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2050,file_pre="GHGEmiMult2050_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDCi",
             year=2050,file_pre="GHGEmiMult2050_INDCi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1000",
             year=2030,file_pre="GHGEmiMult2030_INDC2030i_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1600",
             year=2030,file_pre="GHGEmiMult2030_INDC2030i_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1000",
             year=2030,file_pre="GHGEmiMult2030_NPi2020_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1600",
             year=2030,file_pre="GHGEmiMult2030_NPi2020_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2030,file_pre="GHGEmiMult2030_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDCi",
             year=2030,file_pre="GHGEmiMult2030_INDCi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1000",
             year=2010,file_pre="GHGEmiMult2010_INDC2030i_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC2030i_1600",
             year=2010,file_pre="GHGEmiMult2010_INDC2030i_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1000",
             year=2010,file_pre="GHGEmiMult2010_NPi2020_1000", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi2020_1600",
             year=2010,file_pre="GHGEmiMult2010_NPi2020_1600", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2010,file_pre="GHGEmiMult2010_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDCi",
             year=2010,file_pre="GHGEmiMult2010_INDCi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2015,file_pre="GHGEmiMult2015_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)

#oASIA <-  calcRegion(all[Scope == "global"], ' `oASIA` ~ `R5ASIA` - `IND` - `CHN` ', b.append = F)

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

#Carbon intensity of final energy
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1000",year=2020,file_pre="CI_2020_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1000",year=2030,file_pre="CI_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1000",year=2050,file_pre="CI_2050_INDC2030i_1000")

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1600",year=2020,file_pre="CI_2020_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1600",year=2030,file_pre="CI_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDC2030i_1600",year=2050,file_pre="CI_2050_INDC2030i_1600")

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1000",year=2020,file_pre="CI_2020_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1000",year=2030,file_pre="CI_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1000",year=2050,file_pre="CI_2050_NPi2020_1000")

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1600",year=2020,file_pre="CI_2020_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1600",year=2030,file_pre="CI_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi2020_1600",year=2050,file_pre="CI_2050_NPi2020_1600")

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2020,file_pre="CI_2020_NPi")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2030,file_pre="CI_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="NPi",year=2050,file_pre="CI_2050_NPi")

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDCi",year=2020,file_pre="CI_2020_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDCi",year=2030,file_pre="CI_2030_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="INDCi",year=2050,file_pre="CI_2050_INDCi")


# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of Electricity",cats="INDC2030i_1000",year=2050,file_pre="CI_Elec_2050")

#Shares of wind, solar, nuclear 2030 and 2050
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC2030i_1000",year=2030,file_pre="WSshare_Elec_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC2030i_1000",year=2050,file_pre="WSshare_Elec_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC2030i_1000",year=2050,file_pre="Nuclearshare_Elec_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC2030i_1000",year=2030,file_pre="Nuclearshare_Elec_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC2030i_1600",year=2030,file_pre="WSshare_Elec_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDC2030i_1600",year=2050,file_pre="WSshare_Elec_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC2030i_1600",year=2050,file_pre="Nuclearshare_Elec_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDC2030i_1600",year=2030,file_pre="Nuclearshare_Elec_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi2020_1000",year=2030,file_pre="WSshare_Elec_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi2020_1000",year=2050,file_pre="WSshare_Elec_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi2020_1000",year=2050,file_pre="Nuclearshare_Elec_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi2020_1000",year=2030,file_pre="Nuclearshare_Elec_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi2020_1600",year=2030,file_pre="WSshare_Elec_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi2020_1600",year=2050,file_pre="WSshare_Elec_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi2020_1600",year=2050,file_pre="Nuclearshare_Elec_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi2020_1600",year=2030,file_pre="Nuclearshare_Elec_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi",year=2030,file_pre="WSshare_Elec_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="NPi",year=2050,file_pre="WSshare_Elec_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi",year=2050,file_pre="Nuclearshare_Elec_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="NPi",year=2030,file_pre="Nuclearshare_Elec_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDCi",year=2030,file_pre="WSshare_Elec_2030_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="INDCi",year=2050,file_pre="WSshare_Elec_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDCi",year=2050,file_pre="Nuclearshare_Elec_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="INDCi",year=2030,file_pre="Nuclearshare_Elec_2030_INDCi")

#Energy intensity of GDP
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1000",year=2020,file_pre="EI_2020_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1000",year=2030,file_pre="EI_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1000",year=2050,file_pre="EI_2050_INDC2030i_1000")

plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1600",year=2020,file_pre="EI_2020_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1600",year=2030,file_pre="EI_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDC2030i_1600",year=2050,file_pre="EI_2050_INDC2030i_1600")

plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1000",year=2020,file_pre="EI_2020_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1000",year=2030,file_pre="EI_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1000",year=2050,file_pre="EI_2050_NPi2020_1000")

plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1600",year=2020,file_pre="EI_2020_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1600",year=2030,file_pre="EI_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi2020_1600",year=2050,file_pre="EI_2050_NPi2020_1600")

plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2020,file_pre="EI_2020_NPi")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2030,file_pre="EI_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="NPi",year=2050,file_pre="EI_2050_NPi")

plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDCi",year=2020,file_pre="EI_2020_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDCi",year=2030,file_pre="EI_2030_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="INDCi",year=2050,file_pre="EI_2050_INDCi")

#Emissions per capita
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1000",year=2010,file_pre="EmiCap_2010_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1000",year=2030,file_pre="EmiCap_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1000",year=2050,file_pre="EmiCap_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1600",year=2010,file_pre="EmiCap_2010_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1600",year=2030,file_pre="EmiCap_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDC2030i_1600",year=2050,file_pre="EmiCap_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1000",year=2010,file_pre="EmiCap_2010_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1000",year=2030,file_pre="EmiCap_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1000",year=2050,file_pre="EmiCap_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1600",year=2010,file_pre="EmiCap_2010_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1600",year=2030,file_pre="EmiCap_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi2020_1600",year=2050,file_pre="EmiCap_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2010,file_pre="EmiCap_2010_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2030,file_pre="EmiCap_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="NPi",year=2050,file_pre="EmiCap_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDCi",year=2010,file_pre="EmiCap_2010_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDCi",year=2030,file_pre="EmiCap_2030_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="INDCi",year=2050,file_pre="EmiCap_2050_INDCi")

#Final energy per capita
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="INDC2030i_1000",year=2050,file_pre="FE per Cap_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="INDC2030i_1600",year=2050,file_pre="FE per Cap_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="NPi2020_1000",year=2050,file_pre="FE per Cap_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="NPi2020_1600",year=2050,file_pre="FE per Cap_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="NPi",year=2050,file_pre="FE per Cap_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="INDCi",year=2050,file_pre="FE per Cap_2050_INDCi")

#Fossil emissions per capita
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="INDC2030i_1000",year=2050,file_pre="FossEmiCap_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="INDC2030i_1600",year=2050,file_pre="FossEmiCap_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="NPi2020_1000",year=2050,file_pre="FossEmiCap_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="NPi2020_1600",year=2050,file_pre="FossEmiCap_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="NPi",year=2050,file_pre="FossEmiCap_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="INDCi",year=2050,file_pre="FossEmiCap_2050_INDCi")

#Emissions per capita reference vs. mitigation
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","INDC2030i_1000"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","INDC2030i_1000"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","INDC2030i_1600"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","INDC2030i_1600"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","NPi2020_1000"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","NPi2020_1000"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","NPi2020_1600"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","NPi2020_1600"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050_NPi2020_1600")

#BECCS per capita
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="INDC2030i_1000",year=2050,file_pre="BECCSCap_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="INDC2030i_1600",year=2050,file_pre="BECCSCap_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="NPi2020_1000",year=2050,file_pre="BECCSCap_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="NPi2020_1600",year=2050,file_pre="BECCSCap_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="NPi",year=2050,file_pre="BECCSCap_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="INDCi",year=2050,file_pre="BECCSCap_2050_INDCi")

# plot rel. Abatement
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="INDC2030i_1000",year=2050,file_pre="relEmi_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="INDC2030i_1600",year=2050,file_pre="relEmi_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="NPi2020_1000",year=2050,file_pre="relEmi_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="NPi2020_1600",year=2050,file_pre="relEmi_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="NPi",year=2050,file_pre="relEmi_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="INDCi",year=2050,file_pre="relEmi_2050_INDCi")

plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC2030i_1000",year=2030,file_pre="relAb_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC2030i_1000",year=2050,file_pre="relAb_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC2030i_1600",year=2030,file_pre="relAb_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDC2030i_1600",year=2050,file_pre="relAb_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi2020_1000",year=2030,file_pre="relAb_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi2020_1000",year=2050,file_pre="relAb_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi2020_1600",year=2030,file_pre="relAb_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi2020_1600",year=2050,file_pre="relAb_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi",year=2030,file_pre="relAb_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="NPi",year=2050,file_pre="relAb_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDCi",year=2030,file_pre="relAb_2030_INDCi")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="INDCi",year=2050,file_pre="relAb_2050_INDCi")

plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC2030i_1000",year=2050,file_pre="EmiRel2010_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC2030i_1000",year=2030,file_pre="EmiRel2010_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC2030i_1600",year=2050,file_pre="EmiRel2010_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDC2030i_1600",year=2030,file_pre="EmiRel2010_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi2020_1000",year=2050,file_pre="EmiRel2010_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi2020_1000",year=2030,file_pre="EmiRel2010_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi2020_1600",year=2050,file_pre="EmiRel2010_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi2020_1600",year=2030,file_pre="EmiRel2010_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi",year=2050,file_pre="EmiRel2010_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="NPi",year=2030,file_pre="EmiRel2010_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDCi",year=2050,file_pre="EmiRel2010_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="INDCi",year=2030,file_pre="EmiRel2010_2030_INDCi")

#plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDC2030i_1000",year=2050,file_pre="relAb_2050")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDC2030i_1000",year=2050,file_pre="RedRel2010_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDC2030i_1600",year=2050,file_pre="RedRel2010_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="NPi2020_1000",year=2050,file_pre="RedRel2010_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="NPi2020_1600",year=2050,file_pre="RedRel2010_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="NPi",year=2050,file_pre="RedRel2010_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDCi",year=2050,file_pre="RedRel2010_2050_INDCi")

# plot carbon prices



# multi facet plot of


regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA","World")
#Mitigation costs
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="INDC2030i_1000",year=2050,file_pre="MitiCosts_2050_INDC2030i_1000",b.multivar = T,var.labels = "Mitigation costs 2050 - INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="INDC2030i_1600",year=2050,file_pre="MitiCosts_2050_INDC2030i_1600",b.multivar = T,var.labels = "Mitigation costs 2050 - INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="NPi2020_1000",year=2050,file_pre="MitiCosts_2050_NPi2020_1000",b.multivar = T,var.labels = "Mitigation costs 2050 - NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="NPi2020_1600",year=2050,file_pre="MitiCosts_2050_NPi2020_1600",b.multivar = T,var.labels = "Mitigation costs 2050 - NPi2020_1600")

plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="INDC2030i_1000",year=2100,file_pre="MitiCosts_2100_INDC2030i_1000",b.multivar = T,var.labels = "Mitigation costs 2100 - INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="INDC2030i_1600",year=2100,file_pre="MitiCosts_2100_INDC2030i_1600",b.multivar = T,var.labels = "Mitigation costs 2100 - INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="NPi2020_1000",year=2100,file_pre="MitiCosts_2100_NPi2020_1000",b.multivar = T,var.labels = "Mitigation costs 2100 - NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="NPi2020_1600",year=2100,file_pre="MitiCosts_2100_NPi2020_1600",b.multivar = T,var.labels = "Mitigation costs 2100 - NPi2020_1600")

plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats=c("NPi2020_1600","NPi2020_1000","INDC2030i_1600","INDC2030i_1000"),year=2100,file_pre="MitiCosts_2100_mitigscens",b.multicat = T)

#Carbon price
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="INDC2030i_1000",year=2050,file_pre="CO2price_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="INDC2030i_1600",year=2050,file_pre="CO2price_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="NPi2020_1000",year=2050,file_pre="CO2price_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="NPi2020_1600",year=2050,file_pre="CO2price_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="NPi",year=2050,file_pre="CO2price_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="INDCi",year=2050,file_pre="CO2price_2050_INDCi")

# blank carbon price results for EU

# plot CI over EI indicator
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC2030i_1000",year=2050,file_pre="ci_ei_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC2030i_1000",year=2030,file_pre="ci_ei_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC2030i_1600",year=2050,file_pre="ci_ei_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDC2030i_1600",year=2030,file_pre="ci_ei_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi2020_1000",year=2050,file_pre="ci_ei_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi2020_1000",year=2030,file_pre="ci_ei_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi2020_1600",year=2050,file_pre="ci_ei_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi2020_1600",year=2030,file_pre="ci_ei_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi",year=2050,file_pre="ci_ei_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="NPi",year=2030,file_pre="ci_ei_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDCi",year=2050,file_pre="ci_ei_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="INDCi",year=2030,file_pre="ci_ei_2030_INDCi")

regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA")
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA",  "World")

#BECCS per capita
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="INDC2030i_1000",year=2050,file_pre="BECCS_EmiAndPE_INDC2030i_1000", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="INDC2030i_1600",year=2050,file_pre="BECCS_EmiAndPE_INDC2030i_1600", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="NPi2020_1000",year=2050,file_pre="BECCS_EmiAndPE_NPi2020_1000", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="NPi2020_1600",year=2050,file_pre="BECCS_EmiAndPE_NPi2020_1600", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="NPi",year=2050,file_pre="BECCS_EmiAndPE_NPi", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="INDCi",year=2050,file_pre="BECCS_EmiAndPE_INDCi", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)


# BECCS
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="INDC2030i_1000",year=2050,file_pre="BECCS_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="INDC2030i_1000",year=2050,file_pre="PE_BECCS_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="INDC2030i_1000",year=2050,file_pre="PE_Bio_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="INDC2030i_1600",year=2050,file_pre="BECCS_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="INDC2030i_1600",year=2050,file_pre="PE_BECCS_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="INDC2030i_1600",year=2050,file_pre="PE_Bio_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="NPi2020_1000",year=2050,file_pre="BECCS_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="NPi2020_1000",year=2050,file_pre="PE_BECCS_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="NPi2020_1000",year=2050,file_pre="PE_Bio_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="NPi2020_1600",year=2050,file_pre="BECCS_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="NPi2020_1600",year=2050,file_pre="PE_BECCS_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="NPi2020_1600",year=2050,file_pre="PE_Bio_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="NPi",year=2050,file_pre="BECCS_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="NPi",year=2050,file_pre="PE_BECCS_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="NPi",year=2050,file_pre="PE_Bio_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="INDCi",year=2050,file_pre="BECCS_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="INDCi",year=2050,file_pre="PE_BECCS_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="INDCi",year=2050,file_pre="PE_Bio_2050_INDCi")

#Population
plot_boxplot(regs=regs,dt=all,vars="Population",cats="INDC2030i_1000",year=2010,file_pre="Pop_2010")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="INDC2030i_1000",year=2030,file_pre="Pop_2030")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="INDC2030i_1000",year=2050,file_pre="Pop_2050")

#Carbon budget
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="INDC2030i_1000",year=2050,file_pre="Cbudget_2011-2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="INDC2030i_1000",year=2100,file_pre="Cbudget_2011-2100_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="INDC2030i_1600",year=2050,file_pre="Cbudget_2011-2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="INDC2030i_1600",year=2100,file_pre="Cbudget_2011-2100_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="NPi2020_1000",year=2050,file_pre="Cbudget_2011-2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="NPi2020_1000",year=2100,file_pre="Cbudget_2011-2100_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="NPi2020_1600",year=2050,file_pre="Cbudget_2011-2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="NPi2020_1600",year=2100,file_pre="Cbudget_2011-2100_NPi2020_1600")

#Emissions intensity GDP
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA","World")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC2030i_1000",year=2050,file_pre="Emis_int2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC2030i_1000",year=2030,file_pre="Emis_int2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC2030i_1600",year=2050,file_pre="Emis_int2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDC2030i_1600",year=2030,file_pre="Emis_int2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi2020_1000",year=2050,file_pre="Emis_int2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi2020_1000",year=2030,file_pre="Emis_int2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi2020_1600",year=2050,file_pre="Emis_int2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi2020_1600",year=2030,file_pre="Emis_int2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi",year=2050,file_pre="Emis_int2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="NPi",year=2030,file_pre="Emis_int2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDCi",year=2050,file_pre="Emis_int2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="INDCi",year=2030,file_pre="Emis_int2030_INDCi")

#Rates of change
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC2030i_1000",
             year="2030-2050",file_pre="Rates2030-2050_INDC2030i_1000", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC2030i_1600",
             year="2030-2050",file_pre="Rates2030-2050_INDC2030i_1600", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi2020_1000",
             year="2030-2050",file_pre="Rates2030-2050_NPi2020_1000", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi2020_1600",
             year="2030-2050",file_pre="Rates2030-2050_NPi2020_1600", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi",
             year="2030-2050",file_pre="Rates2030-2050_NPi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDCi",
             year="2030-2050",file_pre="Rates2030-2050_INDCi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC2030i_1000",
             year="2050-2100",file_pre="Rates2050-2100_INDC2030i_1000", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDC2030i_1600",
             year="2050-2100",file_pre="Rates2050-2100_INDC2030i_1600", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi2020_1000",
             year="2050-2100",file_pre="Rates2050-2100_NPi2020_1000", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi2020_1600",
             year="2050-2100",file_pre="Rates2050-2100_NPi2020_1600", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="NPi",
             year="2050-2100",file_pre="Rates2050-2100_NPi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="INDCi",
             year="2050-2100",file_pre="Rates2050-2100_INDCi", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)

#Primary energy
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC2030i_1000",year=2050,file_pre="PE_2050_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC2030i_1000",year=2030,file_pre="PE_2030_INDC2030i_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC2030i_1600",year=2050,file_pre="PE_2050_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDC2030i_1600",year=2030,file_pre="PE_2030_INDC2030i_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi2020_1000",year=2050,file_pre="PE_2050_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi2020_1000",year=2030,file_pre="PE_2030_NPi2020_1000")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi2020_1600",year=2050,file_pre="PE_2050_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi2020_1600",year=2030,file_pre="PE_2030_NPi2020_1600")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi",year=2050,file_pre="PE_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="NPi",year=2030,file_pre="PE_2030_NPi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDCi",year=2050,file_pre="PE_2050_INDCi")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="INDCi",year=2030,file_pre="PE_2030_INDCi")

#Peak year
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA","World")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="INDC2030i_1000",file_pre="Peak_year_CO2_INDC2030i_1000")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="INDC2030i_1600",file_pre="Peak_year_CO2_INDC2030i_1600")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="NPi2020_1000",file_pre="Peak_year_CO2_NPi2020_1000")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|CO2",cats="NPi2020_1600",file_pre="Peak_year_CO2_NPi2020_1600")

plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="INDC2030i_1000",file_pre="Peak_year_Kyoto_INDC2030i_1000")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="INDC2030i_1600",file_pre="Peak_year_Kyoto_INDC2030i_1600")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="NPi2020_1000",file_pre="Peak_year_Kyoto_NPi2020_1000")
plot_boxplot2(regs=regs,dt=all,vars="Peak year|Kyoto Gases",cats="NPi2020_1600",file_pre="Peak_year_Kyoto_NPi2020_1600")

#Regional emissions
plot_boxplot3(regs="BRA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_BRA")
plot_boxplot3(regs="BRA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_BRA")
plot_boxplot3(regs="JPN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_JPN")
plot_boxplot3(regs="JPN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_JPN")
plot_boxplot3(regs="IND",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_IND")
plot_boxplot3(regs="IND",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_IND")
plot_boxplot3(regs="CHN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_CHN")
plot_boxplot3(regs="CHN",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_CHN")
plot_boxplot3(regs="RUS",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_RUS")
plot_boxplot3(regs="RUS",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_RUS")
plot_boxplot3(regs="EU",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_EU")
plot_boxplot3(regs="EU",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_EU")
plot_boxplot3(regs="USA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_USA")
plot_boxplot3(regs="USA",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_USA")
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2020_World")
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_World")

plot_boxplot4(regs=c("USA","CHN","IND","EU"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_US_CHN_IND_EU")
plot_boxplot4(regs=c("JPN","BRA","RUS"),dt=all,vars=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2"),file_pre="Emis_2030_JPN_BRA_RUS")

#Regional energy
plot_boxplot3(regs="BRA",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_BRA")
plot_boxplot3(regs="JPN",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_JPN")
plot_boxplot3(regs="IND",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_IND")
plot_boxplot3(regs="CHN",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_CHN")
plot_boxplot3(regs="RUS",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_RUS")
plot_boxplot3(regs="EU",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_EU")
plot_boxplot3(regs="USA",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_USA")

#Sectoral emissions
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NPi","INDCi"),year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
              cats=c("Historical","NPi","INDCi"),year=2020,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial","Energy Supply","AFOLU"),file_pre="Sector_Emis_CO2_2020_World")

###Sectoral energy
#Energy supply
plot_boxplot3(regs="World",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Primary Energy","Energy Intensity of GDP|MER","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Secondary Energy|Electricity"),cats=c("NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Primary Energy","Energy Intensity","Wind & Solar Share","Renewables Share","Electricity"),file_pre="Energy_2020_World")
#Electricity only
plot_boxplot3(regs="World",dt=all,vars=c("Secondary Energy|Electricity","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Low-carbon Electricity Share|All excl. Fossil w/o CCS"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Wind & Solar Share","Renewables Share","Low-carbon share"),file_pre="Electricity_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Secondary Energy|Electricity","Wind and Solar Share","Renewables Share|Incl. Hydro and Nuclear","Low-carbon Electricity Share|All excl. Fossil w/o CCS"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Wind & Solar Share","Renewables Share","Low-carbon share"),file_pre="Electricity_2020_World")

#AFOLU
plot_boxplot3(regs="World",dt=all,vars=c("Land Cover","Land Cover|Cropland","Land Cover|Cropland|Energy Crops","Primary Energy|Biomass|Traditional"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Land Cover","Cropland","Energy Crops","PE|Traditional biomass"),file_pre="AFOLU_indicators_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Land Cover","Land Cover|Cropland","Land Cover|Cropland|Energy Crops","Primary Energy|Biomass|Traditional"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Land Cover","Cropland","Energy Crops","PE|Traditional biomass"),file_pre="AFOLU_indicators_2020_World")

#FE all sectors
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial"),file_pre="FE_sectors_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Transportation","Industry","Residential and Commercial"),file_pre="FE_sectors_2020_World")

plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Solids|Biomass","Final Energy|Transportation|Liquids|Biomass"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("FE|Solids|Biomass","FE|Transport|Liquids|Biomass"),file_pre="FE_biomass_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Solids|Biomass","Final Energy|Transportation|Liquids|Biomass"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("FE|Solids|Biomass","FE|Transport|Liquids|Biomass"),file_pre="FE_biomass_2020_World")

#Transport
# plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation|Electricity","Final Energy|Transportation|Gases","Final Energy|Transportation|Hydrogen","Final Energy|Transportation|Liquids","Final Energy|Transportation|Liquids|Biomass","Final Energy|Transportation|Other"),cats=c("NPi","INDCi"),
#               year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Liquids-biomass","Other"),file_pre="FE_transport_2030_World")
# plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Transportation|Electricity","Final Energy|Transportation|Gases","Final Energy|Transportation|Hydrogen","Final Energy|Transportation|Liquids","Final Energy|Transportation|Liquids|Biomass","Final Energy|Transportation|Other"),cats=c("NPi","INDCi"),
#               year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Liquids-biomass","Other"),file_pre="FE_transport_2020_World")
plot_boxplot3(regs="World",dt=all,vars=c("FE passenger/pkm","FE freight/tkm"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("FE passenger/pkm","FE freight/tkm"),file_pre="FE_km_transport_2020_World")
plot_boxplot3(regs="World",dt=all,vars=c("FE passenger/pkm","FE freight/tkm"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("FE passenger/pkm","FE freight/tkm"),file_pre="FE_km_transport_2030_World")

#Industry
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Industry|Electricity","Final Energy|Industry|Gases","Final Energy|Industry|Hydrogen","Final Energy|Industry|Liquids","Final Energy|Industry|Other","Final Energy|Industry|Heat","Final Energy|Industry|Solids"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_industry_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Industry|Electricity","Final Energy|Industry|Gases","Final Energy|Industry|Hydrogen","Final Energy|Industry|Liquids","Final Energy|Industry|Other","Final Energy|Industry|Heat","Final Energy|Industry|Solids"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_industry_2020_World")

#Buildings
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial|Gases","Final Energy|Residential and Commercial|Hydrogen","Final Energy|Residential and Commercial|Liquids","Final Energy|Residential and Commercial|Other","Final Energy|Residential and Commercial|Heat","Final Energy|Residential and Commercial|Solids"),cats=c("Historical","NPi","INDCi"),
              year=2030,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_buildings_2030_World")
plot_boxplot3(regs="World",dt=all,vars=c("Final Energy|Residential and Commercial|Electricity","Final Energy|Residential and Commercial|Gases","Final Energy|Residential and Commercial|Hydrogen","Final Energy|Residential and Commercial|Liquids","Final Energy|Residential and Commercial|Other","Final Energy|Residential and Commercial|Heat","Final Energy|Residential and Commercial|Solids"),cats=c("Historical","NPi","INDCi"),
              year=2020,b.multivar=T,var.labels=c("Electricity","Gases","Hydrogen","Liquids","Other","Heat","Solids"),file_pre="FE_buildings_2020_World")

# dt=all
# var="Emissions per capita"
# cats=c("INDC2030i_1000", "NPi")
# b_basepol = T


# ### scatter plot based on diagnostics data
# # Please specify exactly two variables and assign them to x and y
# vars <- c(x="Price|Carbon",y="relative Abatement|CO2")
# cats <- c("INDC2030i_1000")
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
