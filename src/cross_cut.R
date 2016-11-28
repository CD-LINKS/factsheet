
##################################
##### cross-cut plotting
##################################

source("functions/plot_functions_xcut.R")



# regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA",  "World")
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

source("functions/calcRegion.R")

#emission year plot script
source("functions/script_RegionalBudgets.R")


#stackedbar plots
regs <- c("IND","CHN", "EU","USA")
vars <- c("Primary Energy","Primary Energy|Coal|w/o CCS","Primary Energy|Coal|w/ CCS","Primary Energy|Gas|w/o CCS","Primary Energy|Gas|w/ CCS","Primary Energy|Oil|w/o CCS","Primary Energy|Oil|w/ CCS","Primary Energy|Biomass|w/o CCS","Primary Energy|Biomass|w/ CCS","Primary Energy|Nuclear","Primary Energy|Hydro","Primary Energy|Wind","Primary Energy|Solar","Primary Energy|Geothermal","Primary Energy|Other","Primary Energy|Secondary Energy Trade")
cats <- c("INDC","2030_high","2030_low")
plot_stackbar(regs=regs,dt=all,vars=vars,cats=cats,per=2030,lab="Primary Energy (EJ/yr)",file_pre="pe_stackbar")   
plot_stackbar <- function(regs, dt, vars, cats, out=cfg$outdir, lab="Title", file_pre="stackbar",ylim=NA,ybreaks=NA){
  


vars <- c("Reduction rel to 2010", "relative Abatement|CO2", "Mitigation Costs", "Price|Carbon"  )
var_labels <- c("Red. rel to 2010 [%]","Red. rel to Base [%]","Migation Costs [% of GDP]","CO2 Price [$/tCO2]" )

#Scenario overview - example without model identifiers
plot_boxplot(regs=regs,dt=all,vars=vars,cats="2030_low",year=2050,file_pre="Comp2050_2030_low", var.labels = var_labels, b.multivar = T,globpoints=F)

#Final energy + carbon intensity of FE
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_low",
             year=2050,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)

#Share of electricity in final energy and transport
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2030_low",
             year=2050,file_pre="Elec_FE_Trans2050_2030_low", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)

#Share of electricity in final energy
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2030_low",
             year=2050,file_pre="ShElec_FE2050_2030_low", var.labels = c("Share of Elec in FE [%]"))

 #Share of electricity in transport
all[model == "WITCH" & variable == "Share of Elec in Transport" ]$value = NA
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in Transport"),cats="2030_low",
             year=2050,file_pre="ShElec_FETrans2050_2030_low", var.labels = c("Share of Electricity in FE [%]") , b.multivar = T)

#Shares of wind, solar, nuclear
plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="2030_low",year=2050,file_pre="Nuc_WS_share2050_2030_low", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)

#Other GHG emissions per capita 
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2030_low",
             year=2050,file_pre="oGHG_2050_2030_low", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)

#Total GHG emissions
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
             year=2050,file_pre="GHGEmiMult2050_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)

#oASIA <-  calcRegion(all[Scope == "global"], ' `oASIA` ~ `R5ASIA` - `IND` - `CHN` ', b.append = F)

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

#Carbon intensity of final energy
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_low",year=2020,file_pre="CI_2020_2030_low")


# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of Electricity",cats="2030_low",year=2050,file_pre="CI_Elec_2050")

#Shares of wind, solar, nuclear 2030 and 2050
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_low",year=2030,file_pre="WSshare_Elec_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="2030_low",year=2050,file_pre="WSshare_Elec_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_low",year=2050,file_pre="Nuclearshare_Elec_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="2030_low",year=2030,file_pre="Nuclearshare_Elec_2030_2030_low")

#Energy intensity of GDP
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2020,file_pre="EI_2020_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2030,file_pre="EI_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="2030_low",year=2050,file_pre="EI_2050_2030_low")


#Emissions per capita
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2010,file_pre="EmiCap_2010_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2030,file_pre="EmiCap_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="2030_low",year=2050,file_pre="EmiCap_2050_2030_low")

#Final energy per capita
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="2030_low",year=2050,file_pre="FE per Cap_2050_2030_low")

#Fossil emissions per capita
plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="2030_low",year=2050,file_pre="FossEmiCap_2050_2030_low")

#Emissions per capita reference vs. mitigation
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_low"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("NPi","2030_low"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050_2030_low")


# plot rel. Abatement
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="2030_low",year=2050,file_pre="relEmi_2050_2030_low")

plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_low",year=2030,file_pre="relAb_2030_2030_low")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="2030_low",year=2050,file_pre="relAb_2050_2030_low")

plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_low",year=2050,file_pre="EmiRel2010_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="2030_low",year=2030,file_pre="EmiRel2010_2030_2030_low")

#plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_low",year=2050,file_pre="relAb_2050")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_low",year=2050,file_pre="RedRel2010_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2030_high",year=2050,file_pre="RedRel2010_2050_2030_high")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2020_low",year=2050,file_pre="RedRel2010_2050_2020_low")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="2020_high",year=2050,file_pre="RedRel2010_2050_2020_high")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="NPi",year=2050,file_pre="RedRel2010_2050_NPi")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="INDCi",year=2050,file_pre="RedRel2010_2050_INDCi")

# plot carbon prices



# multi facet plot of


regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA","World")
#Mitigation costs
plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="2030_low",year=2050,file_pre="MitiCosts_2050_2030_low",b.multivar = T,var.labels = "Mitigation costs 2050 - 2030_low")

plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats=c("2020_high","2020_low","2030_high","2030_low"),year=2100,file_pre="MitiCosts_2100_mitigscens",b.multicat = T)

#Carbon price
plot_boxplot(regs=regs,dt=all,vars="Price|Carbon",cats="2030_low",year=2050,file_pre="CO2price_2050_2030_low")

# blank carbon price results for EU

# plot CI over EI indicator
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="2030_low",year=2050,file_pre="ci_ei_2050_2030_low")

regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA")
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA",  "World")
#BECCS per capita
plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="2030_low",year=2050,file_pre="BECCSCap_2050_2030_low")

plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="2030_low",year=2050,file_pre="BECCS_EmiAndPE_2030_low", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)


# BECCS
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="2030_low",year=2050,file_pre="BECCS_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2030_low",year=2050,file_pre="PE_BECCS_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2030_low",year=2050,file_pre="PE_Bio_2050_2030_low")

#Population
plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2010,file_pre="Pop_2010")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2030,file_pre="Pop_2030")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="2030_low",year=2050,file_pre="Pop_2050")

#Carbon budget
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_low",year=2050,file_pre="Cbudget_2011-2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Carbon budget|Energy and Industry",cats="2030_low",year=2100,file_pre="Cbudget_2011-2100_2030_low")

#Emissions intensity GDP
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA","World")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_low",year=2050,file_pre="Emis_int2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Emissions Intensity of GDP|MER",cats="2030_low",year=2030,file_pre="Emis_int2030_2030_low")

#Rates of change
plot_boxplot(regs=regs,dt=all,vars=c("Rate of Change| Carbon Intensity of FE", "Rate of Change| Emissions Intensity of GDP|MER", "Rate of Change| Energy Intensity of GDP|MER", "Rate of Change| Emissions|CO2|FFI" ),cats="2030_low",
             year="2030-2050",file_pre="Rates2030-2050_2030_low", var.labels = c("Carbon Intensity of FE", "Emissions Intensity of GDP|MER", "Energy Intensity of GDP|MER", "Emissions|CO2|FFI" ) , b.multivar = T)

#Primary energy
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_low",year=2050,file_pre="PE_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy",cats="2030_low",year=2030,file_pre="PE_2030_2030_low")



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
