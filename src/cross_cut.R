
##################################
##### cross-cut plotting
##################################

source("functions/plot_functions_xcut.R")



# regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA",  "World")
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

source("functions/calcRegion.R")

vars <- c("Reduction rel to 2010", "relative Abatement|CO2", "Mitigation Costs", "Price|Carbon"  )
var_labels <- c("Red. rel to 2010 [%]","Red. rel to Base [%]","Migation Costs [% of GDP]","CO2 Price [$/tCO2]" )


plot_boxplot(regs=regs,dt=all,vars=vars,cats="Global 450 / S2-3",year=2050,file_pre="Comp", var.labels = var_labels, b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="Global 450 / S2-3",
             year=2050,file_pre="Comp_FE_CI", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="Global 450 / S2-3",
             year=2050,file_pre="Elec_FE_Trans", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="Global 450 / S2-3",
             year=2050,file_pre="ShElec_FE", var.labels = c("Share of Elec in FE [%]") , b.multivar = T)

all[model == "WITCH" & variable == "Share of Elec in Transport" ]$value = NA
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in Transport"),cats="Global 450 / S2-3",
             year=2050,file_pre="ShElec_FETrans", var.labels = c("Share of Electricity in FE [%]") , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),
             cats="Global 450 / S2-3",year=2050,file_pre="Nuc_WS_share_", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]") , b.multivar = T)


plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="Global 450 / S2-3",
             year=2050,file_pre="oGHG", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)

plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="Global 450 / S2-3",
             year=2050,file_pre="GHGEmiMult", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)


oASIA <-  calcRegion(all[Scope == "global"], ' `oASIA` ~ `R5ASIA` - `IND` - `CHN` ', b.append = F)

plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="DIAG-C80-gr5",year=2030,file_pre="CI_2030")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="Global 450 / S2-3",year=2050,file_pre="CI_2050")

# plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of Electricity",cats="Global 450 / S2-3",year=2050,file_pre="CI_Elec_2050")

plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="Global 450 / S2-3",year=2030,file_pre="WSshare_Elec_2030")
plot_boxplot(regs=regs,dt=all,vars="Wind and Solar Share",cats="Global 450 / S2-3",year=2050,file_pre="WSshare_Elec_2050")
plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="Global 450 / S2-3",year=2050,file_pre="Nuclearshare_Elec_2050")

plot_boxplot(regs=regs,dt=all,vars="Nuclear Share",cats="Global 450 / S2-3",year=2030,file_pre="Nuclearshare_Elec_2030")


plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="Global 450 / S2-3",year=2030,file_pre="EI_2030")
plot_boxplot(regs=regs,dt=all,vars="Energy Intensity of GDP|MER",cats="Global 450 / S2-3",year=2050,file_pre="EI_2050")

plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="Global 450 / S2-3",year=2010,file_pre="EmiCap_2010")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="Global 450 / S2-3",year=2030,file_pre="EmiCap_2030")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats="Global 450 / S2-3",year=2050,file_pre="EmiCap_2050")

plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats="Global 450 / S2-3",year=2050,file_pre="FE per Cap_2050")

plot_boxplot(regs=regs,dt=all,vars="Fossil emissions per cap",cats="Global 450 / S2-3",year=2050,file_pre="FossEmiCap_2050")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("Reference","Global 450 / S2-3"),
             year=2030,b.multicat = T, file_pre="EmiCap_2030")
plot_boxplot(regs=regs,dt=all,vars="Emissions per capita",cats=c("Reference","Global 450 / S2-3"),
             year=2050,b.multicat = T, file_pre="EmiCap_2050")

plot_boxplot(regs=regs,dt=all,vars="BECCS per capita",cats="Global 450 / S2-3",year=2050,file_pre="BECCSCap_2050")

# plot rel. Abatement
plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2Base",cats="Global 450 / S2-3",year=2050,file_pre="relEmi_2050")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="Global 450 / S2-3",year=2030,file_pre="relAb_2030")
plot_boxplot(regs=regs,dt=all,vars="relative Abatement|CO2",cats="Global 450 / S2-3",year=2050,file_pre="relAb_2050")

plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="Global 450 / S2-3",year=2050,file_pre="EmiRel2010_2050")

plot_boxplot(regs=regs,dt=all,vars="Emissions|CO2|rel2010",cats="Global 450 / S2-3",year=2030,file_pre="EmiRel2010_2030")


plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="Global 450 / S2-3",year=2050,file_pre="relAb_2050")
plot_boxplot(regs=regs,dt=all,vars="Reduction rel to 2010",cats="Global 450 / S2-3",year=2050,file_pre="RedRel2010_2050")

# plot carbon prices



# multi facet plot of


regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA")

plot_boxplot(regs=regs,dt=all,vars="Mitigation Costs",cats="Global 450 / S2-3",year=2050,file_pre="MitiCosts_2050")
plot_boxplot(regs=regs,dt=all,vars="Price Carbon",cats="Global 450 / S2-3",year=2050,file_pre="CO2price_2050")

# blank carbon price results for EU

# plot CI over EI indicator
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="Global 450 / S2-3",year=2050,file_pre="ci_ei_2050")
plot_boxplot(regs=regs,dt=all,vars="CI over EI indicator",cats="Global 450 / S2-3",year=2030,file_pre="ci_ei_2030")

regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA")
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
# regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA",  "World")

plot_boxplot(regs=regs,dt=all,vars=c("BECCS per capita", "Primary Energy|Biomass|w/ CCS"),
             cats="Global 450 / S2-3",year=2050,file_pre="BECCS_EmiAndPE", var.labels = c("BECCS per capita [tCO2]", "Bionergy for CCS [EJ]") , b.multivar = T)


# BECCS
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats="Global 450 / S2-3",year=2050,file_pre="BECCS_2050")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="Global 450 / S2-3",year=2050,file_pre="PE_BECCS_2050")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="Global 450 / S2-3",year=2050,file_pre="PE_Bio_2050")

plot_boxplot(regs=regs,dt=all,vars="Population",cats="Global 450 / S2-3",year=2010,file_pre="Pop_2010")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="Global 450 / S2-3",year=2030,file_pre="Pop_2030")
plot_boxplot(regs=regs,dt=all,vars="Population",cats="Global 450 / S2-3",year=2050,file_pre="Pop_2050")

dt=all
var="Emissions per capita"
cats=c("Global 450 / S2-3", "Reference")
b_basepol = T


### scatter plot based on diagnostics data
# Please specify exactly two variables and assign them to x and y
vars <- c(x="Price|Carbon",y="relative Abatement|CO2")
cats <- c("Global 450 / S2-3")
# regs <- "BRA"
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")
plot_scatter_xcut(reg=regs,dt=all[as.numeric(period)<=2050],vars_to_spread=vars,cats=cats,title="Relative Abatement",file_pre="relAb_co2pr_scatter",xlog=T,xlim=c(10,1000))




cats <- c("DIAG-C80-gr5")
vars <- c(x="Price|Carbon",y="relative Abatement|CO2")

# vars <- c(x="Price|Carbon",y="rel. Abatatement")
# diag <- calcRel2Base(diag,var="Emissions|CO2",baseEq1=F,"rel. Abatatement",diag_scens)


plot_scatter_xcut(reg="BRA",dt=diag[period<=2050 ],vars_to_spread=vars,cats=cats,title="Relative Abatement",file_pre="d_relAb_co2pr_scatter",xlog=F,xlim=c(10,150))

