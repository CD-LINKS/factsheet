
##################################
##### cross-cut plotting
##################################

source("functions/plot_functions_xcut.R")
source("functions/mipStackedBarDiscreteX.R")


# regs <- c("BRA","R5LAM","CHN","IND","R5MAF","R5ASIA", "RUS", "EU","JPN","USA",  "World")
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

source("functions/calcRegion.R")

#emission year plot script
source("functions/order.levels.R")
source("functions/script_RegionalBudgets.R")
source("functions/plot_LineNationalScens.R")



source("BarStackedNatGlob_script.R")


# National Emission pathways for different scenarios ----------------------

scensglob = c("INDCi_V2",  "NPi2020_1000_V2")

scensnat <- c("NPi_V2", "INDC_V2", "NPi2020_high_V2", "NPi2020_low_V2",  "INDC2030_high_V2",  "INDC2030_low_V2")


regs = c("BRA", "CHN", "IND", "RUS", "EU", "JPN", "USA")

vars = "Emissions|CO2|Energy"

#exemplary line plot: 
tmp_p <- all[model == "MESSAGEix-GLOBIOM_1.0" & scenario %in% c("NoPolicy_V2","NPi_V2","NPi2020_1600_V2","NPi2020_1000_V2","INDCi_V2","INDC2030i_1600_V2","INDC2030i_1000_V2") &
               region == "World" & variable==vars & period < 2051,]
tmp_p[period==2030 & scenario == "NPi_V2"]$value <- 0.95*tmp_p[period==2030 & scenario == "NPi_V2"]$value
tmp_p[period>2030 & scenario =="INDCi_V2"]$value <- 1.05 * tmp_p[period>2030 & scenario =="INDCi_V2"]$value 
tmp_p[period==2045 & scenario %in% c("INDC2030i_1600_V2","INDC2030i_1000_V2"),]$value <- 0.8*tmp_p[period==2045 & scenario %in% c("INDC2030i_1600_V2","INDC2030i_1000_V2"),]$value 
tmp_p[period==2050 & scenario %in% c("INDC2030i_1600_V2","INDC2030i_1000_V2"),]$value <- 0.6*tmp_p[period==2050 & scenario %in% c("INDC2030i_1600_V2","INDC2030i_1000_V2"),]$value 
tmp_p$scenario <- factor(tmp_p$scenario, levels=c("NoPolicy_V2","NPi_V2","NPi2020_1600_V2","NPi2020_1000_V2","INDCi_V2","INDC2030i_1600_V2","INDC2030i_1000_V2"))
tmp_p$value <- tmp_p$value /tmp_p[period==2005]$value[1]
ggplot(tmp_p) +
  geom_path(aes(x=period,y=value,group=(scenario),color=scenario,linetype=scenario),size=1) +
  scale_linetype_manual(values=c(1,1,1,1,2,2,2),name="scenario",
                        breaks= c("NoPolicy_V2","NPi_V2","NPi2020_1600_V2","NPi2020_1000_V2","INDCi_V2","INDC2030i_1600_V2","INDC2030i_1000_V2"),
                        labels= c("NoPOL","NPi","NPi2020_high","NPi2020_low","INDC","INDC2030_high","INDC2030_low"))+
  scale_color_manual(values=(c("#000000","#aa3333","#3333aa","#33aa33","#ff6666","#6666ff","#44dd44")),name="scenario",
                     breaks= c("NoPolicy_V2","NPi_V2","NPi2020_1600_V2","NPi2020_1000_V2","INDCi_V2","INDC2030i_1600_V2","INDC2030i_1000_V2"),
                     labels= c("NoPOL","NPi","NPi2020_high","NPi2020_low","INDC","INDC2030_high","INDC2030_low"))+
  ggtitle(label="Illustrative emission trajectories for national scenarios")+
  ylab("Emissions (rel. to 2005)")+xlab("year")+
  theme_bw()
ggsave(filename=paste0(cfg$outdir,"/Illustrative_natscen_trajectories.png"),width = 6,height = 4)

for (reg in regs)
{

    plot_lineNationalScens(reg = reg, dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                           ylab = "Energy CO2 [MtCO2]", file_pre = "EneCO2")
}


# boxplots with different national scenarios ------------------------------


  regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
  catsnat <- c("INDC", "NPi", "2020_high", "2020_low",  "2030_low")
  catglob <- "2020_low"

  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions|CO2|FFI|rel2010",catglob = catglob, catsnat = catsnat,
                            year=2010,file_pre="RedRel2010_2010", var.labels = "Energy CO2 [indexed 2010 = 1]", ylim = c(0,1.8))
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions|CO2|FFI|rel2010",catglob = catglob, catsnat = catsnat,
                            year=2020,file_pre="RedRel2010_2020", var.labels = "Energy CO2 [indexed 2010 = 1]", ylim = c(0,1.8))
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions|CO2|FFI|rel2010",catglob = catglob, catsnat = catsnat,
                            year=2030,file_pre="RedRel2010_2030", var.labels = "Energy CO2 [indexed 2010 = 1]", ylim = c(0,1.8))
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions|CO2|FFI|rel2010",catglob = catglob, catsnat = catsnat,
                            year=2050,file_pre="RedRel2010_2050", var.labels = "Energy CO2 [indexed 2010 = 1]", ylim = c(0,1.8))
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|FFI|rel2010"),catglob = catglob, catsnat = catsnat, ylim = c(-2,3),
                               years=c(2030,2050),file_pre="RedRel2010_2030_2050", var.labels = c("Energy CO2 [indexed 2010 = 1]"),b.multiyear = T)
  

  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions per capita",catglob = catglob, catsnat = catsnat,
                            year=2050,file_pre="CO2perCap2050", var.labels = "Per capita CO2 [tCO2]")
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars="Emissions per capita",catglob = catglob, catsnat = catsnat,
                            year=2030,file_pre="CO2perCap2030", var.labels = "Per capita CO2 [tCO2]")
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions per capita"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="CO2perCap2030", var.labels = c("Per capita CO2 [tCO2]"),b.multiyear = T)

  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,
                            year=2030,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]"), b.multivar = T)
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,
                            year=2050,file_pre="Comp2050_FE_CI_2050_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]"), b.multivar = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Final Energy per capita"),catglob = catglob, catsnat = catsnat,
                            years=c(2030,2050),file_pre="2030_2050_FE", var.labels = c("Final Energy per Capita [GJ]"),b.multiyear = T)
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Final Energy per capita"),catglob = catglob, catsnat = catsnat, 
                               years=c(2030,2050),file_pre="2030_2050_FE", var.labels = c("Final Energy per Capita [GJ]"),b.multiyear = T)

  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_CI", var.labels = c("Carbon Intensity of FE [kgCO2/GJ]"),b.multiyear = T)
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_CI", var.labels = c("Carbon Intensity of FE [kgCO2/GJ]"),b.multiyear = T)
  
  
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Wind and Solar Share", "Nuclear Share"),catglob = catglob, catsnat = catsnat,
                            year=2050,file_pre="ElecLowCarb", var.labels = c("Wind and Solar Share [%]", "Nuclear Share [%]"), b.multivar = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Wind and Solar Share"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_ElecWS", var.labels = c("Wind and Solar Share [%]"),b.multiyear = T)
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Wind and Solar Share"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_ElecWS", var.labels = c("Wind and Solar Share [%]"),b.multiyear = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Nuclear Share"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_ElecNuc", var.labels = c("Nuclear Share [%]"),b.multiyear = T)
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Nuclear Share"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_ElecNuc", var.labels = c("Nuclear Share [%]"),b.multiyear = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Low-carbon Electricity Share|All excl. Fossil w/o CCS"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_Elec_lowC", var.labels = c("Low-carbon Share [%]"),b.multiyear = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Renewables Share|Excl. Nuclear"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_Elec_REN_excl.nuc", var.labels = c("REN Share excl. nuclear [%]"),b.multiyear = T)
  plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Renewables Share|Incl. Hydro and Nuclear"),catglob = catglob, catsnat = catsnat,
                               years=c(2030,2050),file_pre="2030_2050_Elec_REN_incl_hydro_nuc", var.labels = c("REN Share incl. hydro/nuclear/biomass [%]"),b.multiyear = T)
  
  
  regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA")

  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Carbon Sequestration|CCS","Carbon Sequestration|CCS|Biomass","BECCS per capita"),catglob = catglob, catsnat = catsnat,
                            year=2050,file_pre="CCS_2030", var.labels = c("CCS [GtCO2/yr]","BECCS [GtCO2/yr]","BECCS per cap. [tCO2/yr]"), b.multivar = T)


  vars <- c("Emissions|CO2|FFI|rel2010","Emissions per capita")
  var_labels <- c("CO2 [indexed to 2010]","CO2 per capita" )
  # all[ (model == "*India MARKAL" & variable %in% vars), ]$value <- NA

  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=vars,catglob = catglob, catsnat = catsnat,
                            year=2030,file_pre="Emi2030", var.labels = var_labels, b.multivar = T)

  vars <- c(  "Price|Carbon", "Mitigation Costs"  )
  var_labels <- c("CO2 Price [$/tCO2]","Migation Costs [% of GDP]" )
  plot_boxplot_multiScenNat(regs=regs,dt=all,vars=vars,catglob = catglob, catsnat = catsnat,
                            year=2030,file_pre="Cost2030", var.labels = var_labels, b.multivar = T)
  plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=vars,catglob = catglob, catsnat = catsnat,
                               years=c(2030),file_pre="Cost2030", var.labels = var_labels,b.multivar = T)
  


  # plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Primary Energy|Biomass"),catglob = catglob, catsnat = catsnat,
  #                           year=2050,file_pre="PEBio2050", var.labels = "PE Biomass [EJ]", b.multivar = T)


plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_low",
             year=2050,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)



#Final energy + carbon intensity of FE
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_low",
             year=2050,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)



vars <- c("Reduction rel to 2010", "relative Abatement|CO2", "Mitigation Costs", "Price|Carbon"  )
var_labels <- c("Red. rel to 2010 [%]","Red. rel to Base [%]","Migation Costs [% of GDP]","CO2 Price [$/tCO2]" )

#Scenario overview - example without model identifiers
plot_boxplot(regs=regs,dt=all,vars=vars,cats="2030_low",year=2050,file_pre="Comp2050_2030_low", var.labels = var_labels, b.multivar = T,globpoints=F)
plot_boxplot(regs=regs,dt=all,vars=vars,cats="2030_low",year=2030,file_pre="Comp2030_2030_low", var.labels = var_labels, b.multivar = T,globpoints=F)
vars <- c("Reduction rel to 2010", "relative Abatement|CO2")
plot_boxplot_yr(regs=regs,dt=all,vars=vars,cats="2030_low",years=c(2030,2050),file_pre="Comp2030_2050_2030_low", var.labels = var_labels, b.multiyear=T,globpoints=F)
plot_boxplot_yr(regs=regs,dt=all,vars=vars,cats="2030_high",years=c(2030,2050),file_pre="Comp2030_2050_2030_high", var.labels = var_labels, b.multiyear=T,globpoints=F)
plot_boxplot_yr(regs=regs,dt=all,vars=vars,cats="2020_low",years=c(2030,2050),file_pre="Comp2030_2050_2020_low", var.labels = var_labels, b.multiyear=T,globpoints=F)
plot_boxplot_yr(regs=regs,dt=all,vars=vars,cats="2020_high",years=c(2030,2050),file_pre="Comp2030_2050_2020_high", var.labels = var_labels, b.multiyear=T,globpoints=F)


#Final energy + carbon intensity of FE
plot_boxplot(regs=regs,dt=all,vars=c("Final Energy per capita","Carbon Intensity of FE"),cats="2030_low",
             year=2050,file_pre="Comp2050_FE_CI_2030_low", var.labels = c("Final Energy per Capita [GJ]","Carbon Intensity of FE [kgCO2/GJ]") , b.multivar = T)

#Share of electricity in final energy and transport
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),cats="2030_low",
             year=2050,file_pre="Elec_FE_Trans2050_2030_low", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]") , b.multivar = T)

#Share of electricity in final energy
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in FE"),cats="2030_low",
             year=2050,file_pre="ShElec_FE2050_2030_low", var.labels = c("Share of Elec in FE [%]"))
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Share of Elec in FE"),catglob = catglob, catsnat = catsnat,
                             years=c(2020,2030,2050),file_pre="2020_2030_2050_ElecFE", var.labels = c("Share of electricity in FE [%]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Share of Elec in FE"),catglob = catglob, catsnat = catsnat, 
                             years=c(2020,2030,2050),file_pre="2020_2030_2050_ElecFE", var.labels = c("Share of electricity in FE [%]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Share of Elec in FE"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_ElecFE", var.labels = c("Share of electricity in FE [%]"),b.multiyear = T)

 #Share of electricity in transport
all[model == "WITCH" & variable == "Share of Elec in Transport" ]$value = NA
plot_boxplot(regs=regs,dt=all,vars=c("Share of Elec in Transport"),cats="2030_low",
             year=2050,file_pre="ShElec_FETrans2050_2030_low", var.labels = c("Share of Electricity in FE [%]") , b.multivar = T)


#Other GHG emissions per capita
plot_boxplot(regs=regs,dt=all,vars=c("Non-CO2 GHG per capita", "LU Emissions per capita" ),cats="2030_low",
             year=2050,file_pre="oGHG_2050_2030_low", var.labels = c("Non-CO2 GHG [tCO2e/cap]", "AFOLU CO2 Emissions [tCO2/cap]") , b.multivar = T)

#Total GHG emissions
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
             year=2030,file_pre="GHGEmiMult2030_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_low",
             year=2050,file_pre="GHGEmiMult2050_2030_low", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_high",
             year=2030,file_pre="GHGEmiMult2030_2030_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="2030_high",
             year=2050,file_pre="GHGEmiMult2050_2030_high", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC",
             year=2030,file_pre="GHGEmiMult2030_INDC", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="INDC",
             year=2050,file_pre="GHGEmiMult2050_INDC", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2030,file_pre="GHGEmiMult2030_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ),cats="NPi",
             year=2050,file_pre="GHGEmiMult2050_NPi", var.labels = c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O" ) , b.multivar = T)
plot_boxplot_yr(regs=regs,dt=all,vars="Emissions|Kyoto Gases",cats="2030_low",years=c(2030,2050),file_pre="Kyoto_2030_2050_2030_low",b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),catglob = catglob, catsnat = catsnat,
                             years=c(2020,2030,2050),file_pre="2020_2030_2050_Kyoto", var.labels = c("Kyoto gas emissions [Mt CO2eq/year]"),b.multiyear = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                          year=2020,file_pre="Comp2020_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"), b.multivar = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                          year=2030,file_pre="Comp2030_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"), b.multivar = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                          year=2050,file_pre="Comp2050_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"), b.multivar = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="2050_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"),b.multivar = T)

# Sectoral emissions & energy
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                          catglob = catglob, catsnat = catsnat,year=2030,file_pre="Comp2030_sectorCO2_national", var.labels = c("Transport CO2 [MtCO2/yr]","Industry CO2 [MtCO2/yr]","Buildings CO2 [MtCO2/yr]","Energy supply CO2 [Mt CO2/yr]","AFOLU CO2 [Mt CO2/yr]"), b.multivar = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
                          catglob = catglob, catsnat = catsnat,year=2050,file_pre="Comp2050_sectorCO2_national", var.labels = c("Transport CO2 [MtCO2/yr]","Industry CO2 [MtCO2/yr]","Buildings CO2 [MtCO2/yr]","Energy supply CO2 [Mt CO2/yr]","AFOLU CO2 [Mt CO2/yr]"), b.multivar = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),
                          catglob = catglob, catsnat = catsnat,year=2030,file_pre="Comp2030_sectorFE_national", var.labels = c("Transport FE [EJ/yr]","Industry FE [EJ/yr]","Buildings FE [EJ/yr]"), b.multivar = T)
plot_boxplot_multiScenNat(regs=regs,dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),
                          catglob = catglob, catsnat = catsnat,year=2050,file_pre="Comp2050_sectorFE_national", var.labels = c("Transport FE [EJ/yr]","Industry FE [EJ/yr]","Buildings FE [EJ/yr]"), b.multivar = T)

plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CO2_transport", var.labels = c("Transport CO2 emissions [MtCO2/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CO2_industry", var.labels = c("Industry CO2 emissions [MtCO2/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Residential and Commercial"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CO2_buildings", var.labels = c("Buildings CO2 emissions [MtCO2/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CO2_supply", var.labels = c("Energy supply CO2 emissions [MtCO2/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CO2_AFOLU", var.labels = c("AFOLU CO2 emissions [MtCO2/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Final Energy|Transportation"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_FE_transport", var.labels = c("Transport final energy [EJ/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Final Energy|Residential and Commercial"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_FE_buildings", var.labels = c("Buildings final energy [EJ/yr]"),b.multiyear = T)
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Final Energy|Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_FE_industry", var.labels = c("Industry final energy [EJ/yr]"),b.multiyear = T)


#oASIA <-  calcRegion(all[Scope == "global"], ' `oASIA` ~ `R5ASIA` - `IND` - `CHN` ', b.append = F)

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

#Carbon intensity of final energy
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats="2030_low",year=2020,file_pre="CI_2020_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats=c("2030_low","2020_low","NPi","INDC"),year=2030,file_pre="CI_2030_multicat",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Carbon Intensity of FE",cats=c("2030_low","2020_low","NPi","INDC"),year=2050,file_pre="CI_2050_multicat",b.multicat = T)

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
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats=c("2030_low","2020_low","INDC","NPi"),year=2050,file_pre="FE per Cap_2050_multicat",b.multicat = T)
plot_boxplot(regs=regs,dt=all,vars="Final Energy per capita",cats=c("2030_low","2020_low","INDC","NPi"),year=2030,file_pre="FE per Cap_2030_multicat",b.multicat = T)

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
plot_boxplot(regs=regs,dt=all,vars="Carbon Sequestration|CCS|Biomass",cats=c("2030_low","2030_high","2020_low","2020_high"),year=2050,file_pre="BECCS_2050_low-high",b.multicat=TRUE)
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2030_low",year=2050,file_pre="PE_BECCS_2050_2030_low")
plot_boxplot(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2030_low",year=2050,file_pre="PE_Bio_2050_2030_low")
regs <- c("BRA","CHN","IND", "RUS", "EU","JPN","USA")
plot_boxplot_yr(regs=regs,dt=all,vars="Primary Energy|Biomass|w/ CCS",cats="2030_low",years=c(2030,2050),file_pre="PE_BECCS_2030_2050_2030_low",b.multiyear = T)
plot_boxplot_yr(regs=regs,dt=all,vars="Primary Energy|Biomass",cats="2030_low",years=c(2030,2050),file_pre="PE_Bio_2030_2050_2030_low",b.multiyear = T)

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
plot_boxplot_multiScenNat_yr(regs=regs,dt=all,vars=c("Primary Energy"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_PE", var.labels = c("Primary Energy [EJ/year]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PE", var.labels = c("Primary Energy [EJ/year]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PEbio", var.labels = c("Primary Energy|Biomass [EJ/year]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass|w/ CCS"),catglob = catglob, catsnat = c("2020_low","2030_low"), 
                             years=c(2030,2050),file_pre="2030_2050_PEbeccs", var.labels = c("Primary Energy|Biomass|w/ CCS [EJ/year]"),b.multiyear = T)


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
