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

all$scenario=str_replace_all(all$scenario,"NPi_V4","NPi")
all$scenario=str_replace_all(all$scenario,"NPi2020_low_V4","NPi1000")
all$scenario=str_replace_all(all$scenario,"NPi2020_1000_V4","NPi1000")
all$scenario=str_replace_all(all$scenario,"INDC2030_low_V4","NDC1000")
all$scenario=str_replace_all(all$scenario,"INDC2030i_1000_V4","NDC1000")
all$scenario=str_replace_all(all$scenario,"NPi2020_verylow_V4","NPi400")
all$scenario=str_replace_all(all$scenario,"NPi2020_400_V4","NPi400")

scensglob = c("NPi",  "NPi1000")
scensnat <- c("NPi","NPi1000","NDC1000","NPi400")

# All national line plots in one grid -------------------------------------
vars = "Emissions|CO2|Energy"
b<-plot_lineNationalScens(reg = "BRA", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="Brazil (MSB)",file_pre = "EneCO2",nolegend=T,ylim=c(-1200,1200))
c<-plot_lineNationalScens(reg = "CHN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="China (IPAC: -, CHN-TIMES: --)", file_pre = "EneCO2")
e<-plot_lineNationalScens(reg = "EU", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="EU (PRIMES: -, GEM-E3: --)", file_pre = "EneCO2",ylim=c(0,8000))
j<-plot_lineNationalScens(reg = "JPN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]",title="Japan (AIM/E-NIES: -, DNE21+: --)", file_pre = "EneCO2",ylim=c(-200,1600))
r<-plot_lineNationalScens(reg = "RUS", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="Russia (RU-TIMES)",file_pre = "EneCO2",ylim=c(0,2500))
i<-plot_lineNationalScens(reg = "IND", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="India (IND-MARKAL: -, AIM/E-IIM: --)", file_pre = "EneCO2",ylim=c(0,15000))
u<-plot_lineNationalScens(reg = "USA", dt = filter(all, Category != "Historical"), vars = vars, scensnat = scensnat, scensglob = scensglob,
                          ylab = "Energy CO2 [MtCO2]", title="USA (GCAM_USA)", file_pre = "EneCO2",ylim=c(-500,8000))

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

### also for GDP
vars = "GDP|MER"
b<-plot_lineNationalScens(reg = "BRA", dt = all[!Category=="Historical"], vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="Brazil (MSB)",file_pre = "GDP",nolegend=T)#,ylim=c(-300,1200)
c<-plot_lineNationalScens(reg = "CHN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="China (IPAC: -, CHN-TIMES: --)", file_pre = "GDP")
e<-plot_lineNationalScens(reg = "EU", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="EU (PRIMES: -, GEM-E3: --)", file_pre = "GDP")#,ylim=c(0,8000)
j<-plot_lineNationalScens(reg = "JPN", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)",title="Japan (AIM/E-NIES: -, DNE21+: --)", file_pre = "GDP")#,ylim=c(-200,1600)
r<-plot_lineNationalScens(reg = "RUS", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="Russia (RU-TIMES)",file_pre = "GDP")#,ylim=c(0,2500)
i<-plot_lineNationalScens(reg = "IND", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="India (IND-MARKAL: -, AIM/E-IIM: --)", file_pre = "GDP") #,ylim=c(0,15000)
u<-plot_lineNationalScens(reg = "USA", dt = filter(all, Category != "Historical"), vars = vars, scensnat = "NPi", scensglob = "NPi",
                          ylab = "GDP|MER (billion US$2010/yr)", title="USA (GCAM_USA)", file_pre = "GDP") #,ylim=c(-500,8000)

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
ggsave(file=paste(cfg$outdir,"/natscens_GDP_gridarrange.png",sep=""),h,width=24,height=14,dpi=200)

# Pointrange graphs -------------------------------------------------------
# Set regions and categories ----------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400")
catglob <- "NPi1000"


# Emissions ---------------------------------------------------------------
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|FFI|rel2010"),catglob = catglob, catsnat = catsnat, ylim = c(-2,3),
                             years=c(2030,2050),file_pre="RedRel2010_2030_2050", var.labels = c("Energy CO2 [indexed 2010 = 1]"),b.multiyear = T, quantiles=F)
#Table:
CO2indextable=all[region%in%regs&variable=="Emissions|CO2|FFI|rel2010"&Category%in%catsnat&period%in%c(2030,2050)&Scope=="national"]
CO2indextable$scenario<-NULL
CO2indextable$Baseline<-NULL
CO2indextable=spread(CO2indextable,Category,value)
write.csv(CO2indextable,"CO2index_natmods.csv")
#global model range:
dtg <- all[Scope=="global" & variable =="Emissions|CO2|FFI|rel2010" & period %in% c(2030,2050) & Category %in% catglob & region %in% regs]  %>%
  rename(Global = model ) %>% factor.data.frame()
regions=all[,list(region=unique(region)),by=c("model")]
dtg=data.table(dtg)
for(mod in unique(dtg$model)){
  dtg[model==mod]=dtg[model==mod&region %in% regions[model==mod]$region]
}
dtg1 <-dtg[,list(mean=median(value),min=min(value),max=max(value)),by=c("scenario","Category","Baseline","region","period","Scope","unit","variable")]
write.csv(dtg1,"CO2index_globmods.csv")

plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions per capita"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="CO2perCap2030", var.labels = c("Per capita CO2 [tCO2]"),b.multiyear = T, quantiles=F)
#Table:
CO2indextable=all[region%in%regs&variable=="Emissions per capita"&Category%in%catsnat&period%in%c(2030,2050)&Scope=="national"]
CO2indextable$scenario<-NULL
CO2indextable$Baseline<-NULL
CO2indextable=spread(CO2indextable,Category,value)
write.csv(CO2indextable,"CO2capita_natmods.csv")
#global model range:
dtg <- all[Scope=="global" & variable =="Emissions per capita" & period %in% c(2030,2050) & Category %in% catglob & region %in% regs]  %>%
  rename(Global = model ) %>% factor.data.frame()
regions=all[,list(region=unique(region)),by=c("model")]
dtg=data.table(dtg)
for(mod in unique(dtg$model)){
  dtg[model==mod]=dtg[model==mod&region %in% regions[model==mod]$region]
}
dtg=na.omit(dtg)
dtg1 <-dtg[,list(mean=median(value),min=min(value),max=max(value)),by=c("scenario","Category","Baseline","region","period","Scope","unit","variable")]
write.csv(dtg1,"CO2capita_globmods.csv")

regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU", "Emissions|CH4", "Emissions|N2O"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="2050_GHG_national", var.labels = c("GHG emissions [MtCO2eq/yr]","AFOLU CO2 [MtCO2/yr]","CH4 emissions [MtCH4/yr]","N2O emissions [kt N2O/yr]"),b.multivar = T, quantiles=F)

# Energy ------------------------------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Final Energy per capita"),catglob = catglob, catsnat = catsnat, increasefont=T,
                             years=c(2030,2050),file_pre="2030_2050_FE", var.labels = c("Final Energy per Capita [GJ]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon Intensity of FE"),catglob = catglob, catsnat = catsnat,increasefont=T,
                             years=c(2030,2050),file_pre="2030_2050_CI", var.labels = c("Carbon Intensity of FE [kgCO2/GJ]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Wind and Solar Share"),catglob = catglob, catsnat = catsnat,
                             years=c(2030,2050),file_pre="2030_2050_ElecWS", var.labels = c("Wind and Solar Share [%]"),b.multiyear = T, quantiles=F)
# plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Renewables Share|Incl. Hydro and Nuclear"),catglob = catglob, catsnat = catsnat,
#                              years=c(2030,2050),file_pre="2030_2050_Elec_REN_incl_hydro_nuc", var.labels = c("REN Share incl. hydro/nuclear/biomass [%]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Fossil Share"),catglob = catglob, catsnat = catsnat,ylim=c(0,100),
                             years=c(2030,2050),file_pre="2030_2050_ElecFossil", var.labels = c("Fossil Share (coal, oil, gas) [%]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Renewables Share|Excl. Nuclear"),catglob = catglob, catsnat = catsnat,ylim=c(0,100),
                             years=c(2030,2050),file_pre="2030_2050_ElecREN_excl_nuc", var.labels = c("REN Share excl. nuclear [%]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Nuclear Share"),catglob = catglob, catsnat = catsnat,ylim=c(0,100),
                             years=c(2030,2050),file_pre="2030_2050_ElecNuc", var.labels = c("Nuclear Share [%]"),b.multiyear = T, quantiles=F)

regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Share of Elec in FE","Share of Elec in Transport"),catglob = catglob, catsnat = catsnat, 
                             years=c(2050),file_pre="2050_ElecFETrans", var.labels = c("Share of Elec in FE [%]","Share of Elec in Transport [%]"),b.multivar = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Share of Elec in FE"),catglob = catglob, catsnat = catsnat, increasefont=T,
                             years=c(2020,2030,2050),file_pre="2020_2030_2050_ElecFE", var.labels = c("Share of electricity in FE [%]"),b.multiyear = T, quantiles=F)
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400")
catglob <- "NPi1000"
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PE", var.labels = c("Primary Energy [EJ/year]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass"),catglob = catglob, catsnat = catsnat, 
                             years=c(2030,2050),file_pre="2030_2050_PEbio", var.labels = c("Primary Energy|Biomass [EJ/year]"),b.multiyear = T, quantiles=F)
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Primary Energy|Biomass|w/ CCS"),catglob = catglob, catsnat = c("NPi1000","NDC1000"), 
                             years=c(2030,2050),file_pre="2030_2050_PEbeccs", var.labels = c("Primary Energy|Biomass|w/ CCS [EJ/year]"),b.multiyear = T, quantiles=F)

# Costs -------------------------------------------------------------------
vars <- c(  "Price|Carbon", "Mitigation Costs"  )
var_labels <- c("CO2 Price [$/tCO2]","Migation Costs [% of GDP]" )
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=vars,catglob = catglob, catsnat = catsnat,
                             years=c(2030),file_pre="Cost2030", var.labels = var_labels,b.multivar = T, quantiles=F)

# Carbon budget -----------------------------------------------------------
regs <- c("IND","BRA","CHN", "RUS", "EU","JPN","USA")
catsnat <- c("NDC1000")
catglob <- "NDC1000"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NDC1000", var.labels = c("Carbon budget [GtCO2] - NDC1000 - 2050"),b.multiyear = F, quantiles=F)
catsnat <- c("NPi")
catglob <- "NPi"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi", var.labels = c("Carbon budget [GtCO2] - NPi - 2050"),b.multiyear = F, quantiles=F)
catsnat <- c("NPi1000")
catglob <- "NPi1000"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi1000", var.labels = c("Carbon budget [GtCO2] - NPi1000 - 2050"),b.multiyear = F, quantiles=F)
catsnat <- c("NPi400")
catglob <- "NPi400"
plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Carbon budget|Energy and Industry"),catglob = catglob, catsnat = catsnat,
                             years=c(2050),file_pre="Cbudget_2011-2050_NPi400", var.labels = c("Carbon budget [GtCO2] - NPi400 - 2050"),b.multiyear = F, quantiles=F)

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
                             var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2
a2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2030_sectorCO2_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d2<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Supply"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2030_sectorCO2_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)

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
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2cap_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_capita_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2/cap
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NDC1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi1000", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Transport CO2 per capita","Industry CO2 per capita","Buildings CO2 per capita","Energy supply CO2 per capita"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2050_sectorCO2cap_NPi400", noglobrange=T,
                                 var.labels = c("Transport CO2","Industry CO2","Buildings CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)
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
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NDC1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NPi1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2030),file_pre="Comp2030_sectorCO2rel_NPi400", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)
a1=a1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
b1=b1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
c1=c1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
d1=d1+theme(axis.text=element_text(size=16),legend.text=element_text(size=14),legend.title=element_text(size=16),plot.title=element_text(size=18),strip.text=element_text(size=16))
g=arrangeGrob(a1,b1,c1,d1,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_rel2010_2030.png",sep=""),g,width=24,height=18,dpi=200)

#2050-CO2/2010
a1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi", quantiles=F)
b1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NDC1000"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NDC1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NDC1000", quantiles=F)
c1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi1000"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi1000", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi1000", quantiles=F)
d1<-plot_pointrange_multiScen_yr(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Demand|Industry|rel2010","Emissions|CO2|Energy|Demand|Residential and Commercial|rel2010","Emissions|CO2|Energy|Demand|Transportation|rel2010","Emissions|CO2|Energy|Supply|rel2010"),
                                 catglob = catglob, catsnat = c("NPi400"), years=c(2050),file_pre="Comp2050_sectorCO2rel_NPi400", noglobrange=T,
                                 var.labels = c("Industry CO2","Buildings CO2","Transport CO2","Energy supply CO2"),b.multivar = T,plottitle="NPi400", quantiles=F)
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

# Sectors: waterfall plot -------------------------------------------------
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA")
catsnat <- c("NPi", "NPi1000",  "NDC1000","NPi400","Historical")
vars = c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial",
       "Emissions|CO2|Energy|Supply","Emissions|CO2|Energy")
years = c(2010,2030,2050)
dt = all[region%in%regs & Category%in%catsnat & variable%in%vars &period%in%years &Scope=="national"]
dt$scenario<-NULL
dt$Baseline<-NULL
dt$Scope<-NULL
setnames(dt,"variable","Sector")

# First without the waterfall...
dtw=dt[!Sector=="Emissions|CO2|Energy"]
dtw$region = paste(dtw$region,dtw$period,sep="-")
dtw=dtw[,list(value=mean(value)),by=c("Category","region", "period", "unit","Sector")]

a = ggplot(dtw[Category=="NPi"])
a = a + geom_bar(aes(x=region,y=value,fill=Sector),stat="identity",position="stack")
a = a + scale_fill_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend"),name="Sector")
a = a + theme(axis.text.x=element_text(size=16,angle=90),axis.text.y=element_text(size=16))
a = a + ylab("CO2 emissions (MtCO2/year)") + ggtitle("NPi") + xlab("")

b = ggplot(dtw[Category=="NDC1000"])
b = b + geom_bar(aes(x=region,y=value,fill=Sector),stat="identity",position="stack")
b = b + scale_fill_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend"),name="Sector")
b = b + theme(axis.text.x=element_text(size=16,angle=90),axis.text.y=element_text(size=16))
b = b + ylab("CO2 emissions (MtCO2/year)") + ggtitle("NDC1000")+ xlab("")

c = ggplot(dtw[Category=="NPi1000"])
c = c + geom_bar(aes(x=region,y=value,fill=Sector),stat="identity",position="stack")
c = c + scale_fill_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend"),name="Sector")
c = c + theme(axis.text.x=element_text(size=16,angle=90),axis.text.y=element_text(size=16))
c = c + ylab("CO2 emissions (MtCO2/year)") + ggtitle("NPi1000")+ xlab("")

d = ggplot(dtw[Category=="NPi400"])
d = d + geom_bar(aes(x=region,y=value,fill=Sector),stat="identity",position="stack")
d = d + scale_fill_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend"),name="Sector")
d = d + theme(axis.text.x=element_text(size=16,angle=90),axis.text.y=element_text(size=16))
d = d + ylab("CO2 emissions (MtCO2/year)") + ggtitle("NPi400")+ xlab("")

e=arrangeGrob(a,b,c,d,ncol=2)
ggsave(file=paste(cfg$outdir,"/sectors_CO2_stackbar.png",sep=""),e,width=24,height=18,dpi=200)

# Waterfall - to be done
# # NPi400 vs. NPi, Brazil
# NPi400 = dt[Category%in%c("NPi","NPi400")]
# NPi400 = spread(NPi400,Category,value)
# NPi400 = na.omit(NPi400)
# NPi400 = NPi400%>%mutate(change=`NPi400`-`NPi`)
# NPi400 = data.table(gather(NPi400,Category,value,`NPi`,`NPi400`,`change`))
# NPi400 = NPi400[!Sector=="Emissions|CO2|Energy"]
# NPi400$Category = paste(NPi400$Category,NPi400$period,sep="-")
# NPi400 = NPi400[!Category%in%c("NPi400-2010","change-2010")]
# NPi400 = NPi400[region=="BRA"]
# NPi400$id <- seq_along(NPi400$Category)
# 
# #Plot
# bd = ggplot(NPi400)
# bd = bd + geom_bar(data=NPi400[!Category%in%c("change-2030","change-2050")],aes(x=Category,y=value,fill=Sector),stat="identity",position="stack")
# #bd = bd + geom_rect(data=NPi400[Category%in%c("change-2030","change-2050")],aes(x=Category,y=value,xmin=,xmax=,ymin=,max=,fill=Sector),stat="identity")
# bd = bd + scale_fill_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend"),name="Sector")


### With demo
# source("waterfall.r")
# 
# # Prepare data
# dt = dt[!Sector=="Emissions|CO2|Energy"]
# dt = spread(dt,Sector,value)
# setnames(dt,"Emissions|CO2|Energy|Demand|Transportation","Transport")
# setnames(dt,"Emissions|CO2|Energy|Demand|Industry","Industry")
# setnames(dt,"Emissions|CO2|Energy|Demand|Residential and Commercial","Buildings")
# setnames(dt,"Emissions|CO2|Energy|Supply","Supply")
# dt=na.omit(dt)
# dt = dt%>%mutate(Total=Transport+Industry+Buildings+Supply)
# dt = gather(dt,Sector, value,c(Total,Transport,Industry,Buildings,Supply))
# dt = data.table(dt)
# 
# #Starting with Brazil 2030, NPi400
# NPi400 = dt[Category%in%c("NPi","NPi400")&region=="BRA"&period==2030]
# NPi400 = spread(NPi400,Category,value)
# NPi400 = NPi400%>%mutate(change=`NPi400`-`NPi`)
# NPi400 = data.table(gather(NPi400,Category,value,`NPi`,`NPi400`,`change`))
# NPi400 = rbind(NPi400[Sector%in%c("Transport","Industry","Buildings","Supply")&Category=="change"],
#                NPi400[Sector=="Total"&Category%in%c("NPi","NPi400")])
# NPi400[Sector=="Total"&Category%in%c("NPi")]$Sector<-"Total-NPi"
# NPi400[Sector=="Total"&Category%in%c("NPi400")]$Sector<-"Total-NPi400"
# NPi400$Category=NPi400$Sector
# NPi400$id <- seq_along(NPi400$Sector)
# NPi400$id <- str_replace_all(NPi400$id,"5","0")
# NPi400$id <- str_replace_all(NPi400$id,"4","5")
# NPi400$id <- str_replace_all(NPi400$id,"3","4")
# NPi400$id <- str_replace_all(NPi400$id,"2","3")
# NPi400$id <- str_replace_all(NPi400$id,"1","2")
# NPi400$id <- str_replace_all(NPi400$id,"0","1")
# NPi400 = NPi400[order(id)]
# 
# ## Load the data and set correct column names
# df <- NPi400
# setnames(df,"Sector","category")
# setnames(df,"Category","sector")
# 
# ## ----further-prep--------------------------------------------------------
# ## Tidy the levels
# df$category <- factor(df$category, levels=unique(df$category))
# df$sector <- factor(df$sector, levels=unique(df$sector))
# 
# ## ----prepare-plot, echo=TRUE---------------------------------------------
# 
# ## Determines the spacing between columns in the waterfall chart
# offset <- 0.3
# b3 <- waterfall(df, offset=offset) +
#   coord_cartesian(ylim=c(0,1000)) +
#   scale_fill_manual(values=c("Total-NPi"="#cc0000","Buildings"="#7777ff","Industry"="#bb7700",
#                              "Supply"="#993a44","Transport"="#222288","Total-NPi400"="#008000"),name="Sector") +
#   labs(x="", y="Emissions (Mt CO2)", 
#        title="Brazil, 2030") +
#   theme_classic() +
#   theme(plot.title=element_text(face="bold", size=20,hjust=0, vjust=2)) +
#   theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=16,face="bold")) +
#   theme(legend.text=element_text(size=16,face="bold"),legend.title=element_text(size=18,face="bold"))
# 
# ## ----plot, dev='png', fig.width=12, fig.height=7.5-----------------------
# print(b3)
# 
# # Brazil 2050, NPi400
# NPi400 = dt[Category%in%c("NPi","NPi400")&region=="BRA"&period==2050]
# NPi400 = spread(NPi400,Category,value)
# NPi400 = NPi400%>%mutate(change=`NPi400`-`NPi`)
# NPi400 = data.table(gather(NPi400,Category,value,`NPi`,`NPi400`,`change`))
# NPi400 = rbind(NPi400[Sector%in%c("Transport","Industry","Buildings","Supply")&Category=="change"],
#                NPi400[Sector=="Total"&Category%in%c("NPi","NPi400")])
# NPi400[Sector=="Total"&Category%in%c("NPi")]$Sector<-"Total-NPi"
# NPi400[Sector=="Total"&Category%in%c("NPi400")]$Sector<-"Total-NPi400"
# NPi400$Category=NPi400$Sector
# NPi400$id <- seq_along(NPi400$Sector)
# NPi400$id <- str_replace_all(NPi400$id,"5","0")
# NPi400$id <- str_replace_all(NPi400$id,"4","5")
# NPi400$id <- str_replace_all(NPi400$id,"3","4")
# NPi400$id <- str_replace_all(NPi400$id,"2","3")
# NPi400$id <- str_replace_all(NPi400$id,"1","2")
# NPi400$id <- str_replace_all(NPi400$id,"0","1")
# NPi400 = NPi400[order(id)]
# 
# ## Load the data and set correct column names
# df <- NPi400
# setnames(df,"Sector","category")
# setnames(df,"Category","sector")
# 
# ## ----further-prep--------------------------------------------------------
# ## Tidy the levels
# df$category <- factor(df$category, levels=unique(df$category))
# df$sector <- factor(df$sector, levels=unique(df$sector))
# 
# ## ----prepare-plot, echo=TRUE---------------------------------------------
# 
# ## Determines the spacing between columns in the waterfall chart
# offset <- 0.3
# b5 <- waterfall(df, offset=offset) +
#   coord_cartesian(ylim=c(0,1000)) +
#   scale_fill_manual(values=c("Total-NPi"="#cc0000","Buildings"="#7777ff","Industry"="#bb7700",
#                              "Supply"="#993a44","Transport"="#222288","Total-NPi400"="#008000"),name="Sector") +
#   labs(x="", y="Emissions (Mt CO2)", 
#        title="Brazil, 2050") +
#   theme_classic() +
#   theme(plot.title=element_text(face="bold", size=20,hjust=0, vjust=2))+
#   theme(axis.text=element_text(size=16,face="bold"),axis.title=element_text(size=16,face="bold")) +
#   theme(legend.text=element_text(size=16,face="bold"),legend.title=element_text(size=18,face="bold"))
# 
# ## ----plot, dev='png', fig.width=12, fig.height=7.5-----------------------
# print(b5)
# 
# B=arrangeGrob(b3,b5,ncol=2)
# ggsave(file=paste(cfg$outdir,"/Brazil_waterfalls_easy.png",sep=""),B,width=24,height=14,dpi=200)

# Christoph's script ------------------------------------------------------
### Christoph's script!
source("functions/BarStackedNatGlob_waterfall_script_synthesis.R")
