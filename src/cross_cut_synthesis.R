# Source functions --------------------------------------------------------
source("functions/plot_functions_xcut.R")
source("functions/mipStackedBarDiscreteX.R")
source("functions/calcRegion.R")
source("functions/order.levels.R")
source("functions/script_RegionalBudgets_synthesis.R")
source("functions/plot_LineNationalScens.R")
library(grid)
library(gridExtra)

# Setting regions and scenarios -------------------------------------------
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA",  "World")

all$Category=str_replace_all(all$Category,"2020_low","NPi1000")
all$Category=str_replace_all(all$Category,"2020_verylow","NPi400")
all$Category=str_replace_all(all$Category,"2030_low","NDC1000")

all$scenario=str_replace_all(all$scenario,"NPi_V3","NPi")
all$scenario=str_replace_all(all$scenario,"NPi2020_low_V3","NPi1000")
all$scenario=str_replace_all(all$scenario,"NPi2020_1000_V3","NPi1000")
all$scenario=str_replace_all(all$scenario,"INDC2030_low_V3","NDC1000")
all$scenario=str_replace_all(all$scenario,"INDC2030i_1000_V3","NDC1000")
all$scenario=str_replace_all(all$scenario,"NPi2020_verylow_V3","NPi400")
all$scenario=str_replace_all(all$scenario,"NPi2020_400_V3","NPi400")

scensglob = c("NPi",  "NPi1000")
scensnat <- c("NPi","NPi1000","NDC1000","NPi400")

# All national line plots in one grid -------------------------------------
vars = "Emissions|CO2|Energy"
b<-plot_lineNationalScens(reg = "BRA", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="Brazil (MSB)",file_pre = "EneCO2",nolegend=T)
c<-plot_lineNationalScens(reg = "CHN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="China (IPAC: -, CHN-TIMES: --)", file_pre = "EneCO2")
e<-plot_lineNationalScens(reg = "EU", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="EU (PRIMES: -, GEM-E3: --)", file_pre = "EneCO2")
j<-plot_lineNationalScens(reg = "JPN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]",title="Japan (AIM/E-NIES: -, DNE21+: --)", file_pre = "EneCO2")
r<-plot_lineNationalScens(reg = "RUS", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="Russia (RU-TIMES)",file_pre = "EneCO2")
i<-plot_lineNationalScens(reg = "IND", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="India (IND-MARKAL: -, AIM/E-IIM: --)", file_pre = "EneCO2")
u<-plot_lineNationalScens(reg = "USA", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="USA (GCAM_USA)", file_pre = "EneCO2")

library(grid)
library(gridExtra)
tmp<-ggplot_gtable(ggplot_build(b))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
b=b+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
c=c+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
e=e+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
i=i+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
j=j+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
r=r+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
u=u+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
lay<-rbind(c(1,2,3,4),c(5,6,7,8))
h=grid.arrange(b,c,e,i,j,r,u,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/natscens_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

# Pointrange graphs -------------------------------------------------------
# Set regions and categories ----------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400")
catglob <- "NPi1000"


# Emissions ---------------------------------------------------------------
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|FFI|rel2010"),catglob = catglob, catsnat = catsnat, ylim = c(-2,3),
                             years=c(2030,2050),file_pre="RedRel2010_2030_2050", var.labels = c("Energy CO2 [indexed 2010 = 1]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions per capita"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="CO2perCap2030", var.labels = c("Per capita CO2 [tCO2]"),b.multiyear = T)
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="2050_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"),b.multivar = T)

# Energy ------------------------------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Final Energy per capita"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_FE", var.labels = c("Final Energy per Capita [GJ]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_CI", var.labels = c("Carbon Intensity of FE [kgCO2/GJ]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Wind and Solar Share"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_ElecWS", var.labels = c("Wind and Solar Share [%]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Nuclear Share"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_ElecNuc", var.labels = c("Nuclear Share [%]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Renewables Share|Incl. Hydro and Nuclear"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_Elec_REN_incl_hydro_nuc", var.labels = c("REN Share incl. hydro/nuclear/biomass [%]"),b.multiyear = T)
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),catglob = catglob, catsnat = catsnat, 
                             years=c(2050),file_pre="2050_ElecFETrans", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]"),b.multivar = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Share of Elec in FE"),catglob = catglob, catsnat = catsnat, 
                             years=c(2020,2030,2050),file_pre="2020_2030_2050_ElecFE", var.labels = c("Share of electricity in FE [%]"),b.multiyear = T)
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400")
catglob <- "NPi1000"
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PE", var.labels = c("Primary Energy [EJ/year]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PEbio", var.labels = c("Primary Energy|Biomass [EJ/year]"),b.multiyear = T)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass|w/ CCS"),catglob = catglob, catsnat = c("NPi1000","NDC1000"), 
                             years=c(2030,2050),file_pre="2030_2050_PEbeccs", var.labels = c("Primary Energy|Biomass|w/ CCS [EJ/year]"),b.multiyear = T)

# Costs -------------------------------------------------------------------
vars <- c(  "Price|Carbon", "Mitigation Costs"  )
var_labels <- c("CO2 Price [$/tCO2]","Migation Costs [% of GDP]" )
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=vars,catglob = catglob, catsnat = catsnat,
                             years=c(2030),file_pre="Cost2030", var.labels = var_labels,b.multivar = T)

# Carbon budget -----------------------------------------------------------
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
catsnat <- c("NDC1000")
catglob <- "NDC1000"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NDC1000", var.labels = c("Carbon budget [GtCO2] - NDC1000 - 2050"),b.multiyear = F)
catsnat <- c("NPi")
catglob <- "NPi"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi", var.labels = c("Carbon budget [GtCO2] - NPi - 2050"),b.multiyear = F)
catsnat <- c("NPi1000")
catglob <- "NPi1000"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi1000", var.labels = c("Carbon budget [GtCO2] - NPi1000 - 2050"),b.multiyear = F)
catsnat <- c("NPi400")
catglob <- "NPi400"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi400", var.labels = c("Carbon budget [GtCO2] - NPi400 - 2050"),b.multiyear = F)

# Sectoral emissions & energy ---------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA")
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400")
catglob <- "NPi1000"

# plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply","Emissions|CO2|AFOLU"),
#                              catglob = catglob, catsnat = catsnat, years=c(2030, 2050),file_pre="Comp2030-50_sectorCO2_national", noglobrange=T,
#                              var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2","AFOLU CO2"),b.multivar = T,b.multiyear=T)
# 
# plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial"),
#                              catglob = catglob, catsnat = catsnat, years=c(2030, 2050),file_pre="Comp2030-50_sectorFE_national", noglobrange=T,
#                              var.labels = c("Transport FE [EJ/yr]","Industry FE [EJ/yr]","Buildings FE [EJ/yr]"),b.multivar = T,b.multiyear=T)

#### CO2 ####
#2030-CO2
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                             catglob = catglob, catsnat = c("NPi"), years=c(2030),file_pre="Comp2030_sectorCO2_NPi", noglobrange=T,
                             var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2
a2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2030_sectorCO2_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")

a2=a2+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b2=b2+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c2=c2+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d2=d2+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
h=arrangeGrob(a2,b2,c2,d2,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_2050.png",sep=""),h,width=24,height=18,dpi=200)

## Per capita
#2030-CO2/cap
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NPi", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_capita_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2/cap
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_capita_2050.png",sep=""),g,width=24,height=18,dpi=200)


##Relative to 2010
#2030-CO2/2010
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NPi", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NDC1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NPi1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NPi400", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_rel2010_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2/2010
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi")
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NDC1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000")
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000")
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi400", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400")
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_rel2010_2050.png",sep=""),g,width=24,height=18,dpi=200)

#### Final Energy ####
# "Final Energy|Industry|rel2010"
# "Final Energy|Residential and Commercial|rel2010"
# "Final Energy|Transportation|rel2010"

#### To do: other layout? ####

# a1=a1+theme(legend.position = "bottom")
# tmp<-ggplot_gtable(ggplot_build(a1))
# leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
# legend<-tmp$grobs[[leg]]
# a1=a1+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
# b1=b1+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
# c1=c1+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
# d1=d1+theme(legend.position = "none")+theme(axis.text=element_text(size=16),plot.title = element_text(size=18))
# lay<-rbind(c(1,2,1,2),c(3,4,5,5))
# h=grid.arrange(a1,b1,c1,d1,legend,layout_matrix=lay)
# ggsave(file=paste(cfg$outdir,"/sectors_CO2_2030_grid.png",sep=""),h,width=24,height=14,dpi=200)
