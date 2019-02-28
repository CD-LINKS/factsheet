# Load data ---------------------------------------------------------------
setwd("~/disks/local/factsheet/src")
config <-"config_xCut"
scencateg <- "scen_categ_V4"
variables <- "variables_neutrality"
adjust <- "adjust_reporting_neutrality" # TODO to decide: remove MESSAGE for China and India due to region definition? and COFFEE & DNE for EU and DNE for China and India?
addvars <- F
source("load_data.R")

outdir <- "Neutrality/graphs"
if(!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

# Change scenario names for paper -----------------------------------------
all$Category=str_replace_all(all$Category,"NoPOL","No policy")
all$Category=str_replace_all(all$Category,"INDC","NDC")
all$Category=str_replace_all(all$Category,"NPip","National policies planned")
all$Category=str_replace_all(all$Category,"NPi","National policies")
all$Category=str_replace_all(all$Category,"2020_low","2 °C")
all$Category=str_replace_all(all$Category,"2020_verylow","1.5 °C")
all$Category=str_replace_all(all$Category,"2030_low","2 °C (2030)")

# And region names
# oecd=all[region=="R5OECD90+EU"]
# oecd$region<-"OECD90+EU"
# all=all[!region=="R5OECD90+EU"]
# all=rbind(all,oecd)
# all$region=str_replace_all(all$region,"R5REF","Reforming")
# all$region=str_replace_all(all$region,"R5MAF","ME+Africa")
# all$region=str_replace_all(all$region,"R5LAM","Latin America")
# all$region=str_replace_all(all$region,"R5ASIA","Asia")

# Data preparation --------------------------------------------------------
np=data.table(all)
np=np[Scope=="global"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5REF","R5OECD90+EU")]

# write output: overview of original scenarios per category, model and region
u=np[,list(unique(scenario)),by=c("model","Category","region")]
write.csv(u,paste("Neutrality","/Scenario overview.csv",sep=""))


# Regional phase-out years ------------------------------------------------
# TODO: Graph like Joeri’s (something better than errorbars?)
# See functions - pbl_colors.r?

### relative to global ###
ghg=np[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Emissions|CO2|Energy and Industrial Processes")]
check=ghg[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
ghg=ghg[model%in%check$model]

poy=ghg[!duplicated(ghg[,list(model,Category,region,variable),with=TRUE]),!c('value','period',"Scope","Baseline","scenario"),with=FALSE]
poy=merge(poy,ghg[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1=2105
world=poy[region=="World"]
poy=merge(poy,world, by=c("model","Category","variable","unit"))
setnames(poy,"V1.x","poy")
setnames(poy,"V1.y","world")
poy$region.y<-NULL
setnames(poy,"region.x","region")
poy$diff=ifelse(poy$poy<poy$world,"earlier",ifelse(poy$poy>poy$world,"later","same"))
poy$years=poy$poy-poy$world

models=poy[,list(number=length(unique(model))),by=c('region','variable')]
poy=merge(poy, models, by=c('region','variable'))
poy$region <- paste(poy$region,' [',poy$number,' models]',sep="")
poy=poy[!number<2]

poyrange=data.table(poy[,list(median=median(years),min=min(years),max=max(years)),by=c("Category","region","unit","variable")])

S = ggplot()
S = S + geom_errorbar(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [7 models]"], aes(ymin=min,ymax=max, x=region, colour=variable)) #variable as fill?
S = S + geom_point(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [7 models]"], aes(y=median,x=region,colour=variable))
S = S + coord_flip()
S = S + facet_grid(.~Category, scales="free_y")
S = S + geom_hline(yintercept=0)
S = S + ylab("Phase out year relative to world (years)")
S = S + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_diffworld.png",sep=""),S,width=11, height=8, dpi=120)

# Effect of LULUCF definitions --------------------------------------------
#	Land CO2 in models vs. in inventories: effect on neutrality of different definitions 
# TODO: new inventory data?
  # Phase-out year (model AFOLU data) ---------------------------------------
poyrange1=poyrange[variable=="Emissions|Kyoto Gases"]
poyrange1$variable<-"Model AFOLU data"
poyrange1$unit<-NULL

  # Phase-out year (inventory AFOLU data) -------------------------------------------
dt=np[variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU") & Category %in% c("2 °C","1.5 °C")] 
dt$scenario<-NULL
dt$Baseline<-NULL
dt$Scope<-NULL

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(period)), by=c("model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=model %in% check$model)

# Read data for harmonisation
har=fread("data/landuseinventory.csv",header=TRUE)
har1=har[variable=="Emissions|CO2|AFOLU"]

# Merge and calculate model deviation in 2010
dt1=dt[period==2010 & variable=="Emissions|CO2|AFOLU"]
harmo=merge(dt1,har1, by=c("region","variable"))
setnames(harmo,"value.x","modelvalue")
setnames(harmo,"value.y","hist")
harmo=harmo %>% mutate(factor=modelvalue-hist)
harmo$modelvalue<-NULL
harmo$hist<-NULL 
harmo$period<-NULL

# Harmonise - only land use
dth=merge(dt[variable=="Emissions|CO2|AFOLU"],harmo,by=c("region","variable","Category","model","unit"))
dth=dth %>% mutate(harmonised=value-factor)
dth$factor<-NULL
dth$value<-NULL
setnames(dth,"harmonised","value")
dth$variable<-"Emissions|CO2|AFOLU|Harmo"
dth=data.table(dth)
dth=setcolorder(dth,c("Category","model","region","variable","unit","period","value"))

# Add harmonised land use to GHG excl. land use
dtm=rbind(dt,dth)
dtm=spread(dtm[,!c('unit'),with=FALSE],variable, value)
dtm=na.omit(dtm)
dtm=dtm %>% mutate(`Emissions|Kyoto Gases|Harmo`=`Emissions|Kyoto Gases` - `Emissions|CO2|AFOLU` + `Emissions|CO2|AFOLU|Harmo`)
dtm=gather(dtm,variable,value,c(`Emissions|Kyoto Gases|Harmo`,`Emissions|Kyoto Gases`, `Emissions|CO2|AFOLU`,`Emissions|CO2|AFOLU|Harmo`))
dtm=data.table(dtm)
dth=dtm[variable=="Emissions|Kyoto Gases|Harmo"]
dth$variable<-"Emissions|Kyoto Gases"

# Calculate phase-out year
poy=dth[!duplicated(dth[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE]
poy=merge(poy,dth[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1=2105

# Relative to global
world=poy[region=="World"]
poy=merge(poy,world, by=c("model","Category","variable"))
setnames(poy,"V1.x","poy")
setnames(poy,"V1.y","world")
poy$region.y<-NULL
setnames(poy,"region.x","region")
poy$diff=ifelse(poy$poy<poy$world,"earlier",ifelse(poy$poy>poy$world,"later","same"))
poy$years=poy$poy-poy$world

# Add number of models per region
models=poy[,list(number=length(unique(model))),by=c('region','variable')]
poy=merge(poy, models, by=c('region','variable'))
poy$region <- paste(poy$region,' [',poy$number,' models]',sep="")
poy=poy[!number<2]

poyrange=data.table(poy[,list(median=median(years),min=min(years),max=max(years)),by=c("Category","region","variable")])

poyrange2=poyrange
poyrange2$variable<-"Inventory AFOLU data"

  # Plotting ----------------------------------------------------------------
poy=rbind(poyrange1,poyrange2) 

# Plot
S1 = ggplot()
S1 = S1 + geom_errorbar(data=poy[Category%in%c("2 °C","1.5 °C")&!region=="World [7 models]"], aes(ymin=min,ymax=max, x=region, colour=variable)) #variable as fill?
S1 = S1 + geom_point(data=poy[Category%in%c("2 °C","1.5 °C")&!region=="World [7 models]"], aes(y=median,x=region,colour=variable))
S1 = S1 + coord_flip()
S1 = S1 + facet_grid(.~Category, scales="free_y")
S1 = S1 + geom_hline(yintercept=0)
S1 = S1 + ylab("Phase out year relative to world (years)")
S1 = S1 + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
S1 = S1 + theme_bw()
S1 = S1 + theme(axis.text.y=element_text(angle=45, size=16))
S1 = S1 + theme(strip.text.x=element_text(size=14))
S1 = S1 + theme(axis.title=element_text(size=18))
S1 = S1 + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S1 = S1 + theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_diffworld_inventory.png",sep=""),S1,width=11, height=8, dpi=120)


# Effect of allocation of negative emissions ------------------------------

#	Allocation negative emissions: check CO2 BECCS, add to region it belongs to, then allocate based on biomass production  check effect on neutrality   
# Check effect of different allocations (either to biomass producer or electricity/CCS) on phase-out years. E.g. energy crop production, CCSbiomass carbon sequestration and trade-primary energy-biomass-volume?

# Mitigation strategies / why are some regions earlier or later ---------------------------------------------------

# EU late vs. US early: space for afforestation (e.g. population density).  Check database for other reasons , e.g. non-CO2 share in 2015? Mogelijk bevolkingsdichtheid of misschien productieve grond per persoon tegen jaar CO2 neutraliteit? Evt. ook iets van CCS capaciteit.
#	Graph: X indicator with effect on neutrality, e.g. afforestation  capacity; Y phase-out year 
# Graph: Emissions in phase-out year (like Joeri’s) 
# En dus bijvoorbeeld ook de strategie waarlangs een regio neutraliteit krijgt (meer uit reductie emissies, over meer uit negatieve emissies).
# TODO: continue cleaning up / selecting / adjusting from here

### MILES ###
sdt=dat[Variable %in% c("Emissions|CH4","Emissions|F-Gases","Emissions|Kyoto Gases","Emissions|N2O",
                        "Emissions|CO2",
                        "Emissions|CO2|Fossil Fuels and Industry",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Supply",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Demand",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Agriculture",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Residential and Commercial",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Industry",
                        "Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Transportation",
                        "Emissions|CO2|Carbon Capture and Storage",
                        "Emissions|CO2|Carbon Capture and Storage|Biomass",
                        "Emissions|CO2|Land Use") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450",,"Realistic 450"
dt=sdt[Variable %in% c("Emissions|Kyoto Gases")]
write.csv(dt,paste(outt,"/emissionsbreakdown_delayed_fullset.csv",sep=""))
write.xlsx(dt,paste(outt,"/emissionsbreakdown_delayed_fullset.xlsx",sep=""))

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)
write.csv(dt,paste(outt,"/emissionsbreakdown_delayed_2100.csv",sep=""))
write.xlsx(dt,paste(outt,"/emissionsbreakdown_delayed_2100.xlsx",sep=""))

#Phase-out year
poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$Unit<-NULL
poy=na.omit(poy)
setnames(poy,"V1","Year")

#selection for merge
library(tidyr)
emis=spread(sdt[,!c('Unit'),with=FALSE],Variable,value)
poyemis=merge(poy,emis, by=c('Model','Scenario_original','Scenario','Region','Year'))
poyemis$Variable<-NULL

#Calculations for final plotting
setnames(poyemis,"Emissions|CH4","CH4")
setnames(poyemis,"Emissions|N2O","N2O")
poyemis$CH4=poyemis$CH4*25
poyemis$N2O=poyemis$N2O*298/1000

#Selection of categories to avoid double counting
setnames(poyemis,"Emissions|CO2","CO2")
setnames(poyemis,"Emissions|CO2|Carbon Capture and Storage","CCS")
setnames(poyemis,"Emissions|CO2|Carbon Capture and Storage|Biomass","CCSbio")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry","CO2ffi")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Demand","CO2demand")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Agriculture","CO2agriculture")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Industry","CO2industry")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Residential and Commercial","CO2buildings")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Demand|Transportation","CO2transport")
setnames(poyemis,"Emissions|CO2|Fossil Fuels and Industry|Energy Supply","CO2supply")
setnames(poyemis,"Emissions|CO2|Land Use","CO2land")
setnames(poyemis,"Emissions|F-Gases","Fgases")
setnames(poyemis,"Emissions|Kyoto Gases","Kyoto")
poyemis$CO2<-NULL
poyemis$CO2ffi<-NULL
poyemis$CO2demand<-NULL
poyemis$Kyoto<-NULL

#CCS: calculating CCS without bio from total to avoid double counting, and converting to negative emissions (stored amounts reported as positive numbers)
poyemis$CCS=poyemis$CCS-poyemis$CCSbio
poyemis$CCS=poyemis$CCS*-1
poyemis$CCSbio=poyemis$CCSbio*-1

#Gather for plotting (exclude CCS because of double counting - plot separately?)
poyemis=gather(poyemis,Variable,value,c(CH4,CO2agriculture,CO2industry,CO2buildings,CO2transport,CO2supply,CO2land,Fgases,N2O))
poyemis$CCS<-NULL
poyemis$CCSbio<-NULL
poyemis=data.table(poyemis)
#poyemis[Region=="World"]$Model <- paste(poyemis[Region=="World"]$Model,' [',poyemis[Region=="World"]$Year,']',sep="")
poyemis$Model <- paste(poyemis$Model,' [',poyemis$Year,']',sep="")

# Legend order
poyemis$Variable=factor(poyemis$Variable, levels=c("CH4","Fgases","N2O","CO2buildings","CO2industry","CO2transport","CO2supply","CO2land"))

#check if each scenario category has only one scenario
check=poyemis[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

#Mean per scenario category per model
poyemis=poyemis[,mean(value,na.rm=TRUE),by=c('Scenario','Region','Variable','Model','Year')]
setnames(poyemis,"V1","value")

#Stacked bar chart remaining emissions in poy - add phase-out year somewhere for regional graphs?
library(ggplot2)
library(gridExtra)
# common theme
ttheme = theme_bw() + 
  theme(axis.text.x=element_text(angle=30, size=18, hjust=0.75)) + theme(axis.text.y=element_text(size=18)) + theme(axis.title=element_text(size=18)) + theme(axis.title.y=element_text(size=14)) + theme(legend.text=element_text(size=18)) + theme(legend.title=element_text(size=18)) + theme(strip.text=element_text(size=18))

Canada = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Canada") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Canada") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(out,"/Canada",".png",sep=""),Canada,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Canada_exPOLES",".png",sep=""),Canada,height=12, width=12,dpi=500)

Brazil = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Brazil") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Brazil") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Brazil",".png",sep=""),Brazil,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Brazil_exPOLES",".png",sep=""),Brazil,height=12, width=12,dpi=500)

Mexico = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Mexico") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Mexico") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Mexico",".png",sep=""),Mexico,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Mexico_exPOLES",".png",sep=""),Mexico,height=12, width=12,dpi=500)

SouthKorea = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("South Korea") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("South Korea") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/South Korea",".png",sep=""),SouthKorea,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/South Korea_exPOLES",".png",sep=""),SouthKorea,height=12, width=12,dpi=500)

Turkey = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Turkey") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Turkey") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Turkey",".png",sep=""),Turkey,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Turkey_exPOLES",".png",sep=""),Turkey,height=12, width=12,dpi=500)

EU = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("EU") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("EU") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/EU",".png",sep=""),EU,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/EU_exPOLES",".png",sep=""),EU,height=12, width=12,dpi=500)

Indonesia = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Indonesia") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Indonesia") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Indonesia",".png",sep=""),Indonesia,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Indonesia_exPOLES",".png",sep=""),Indonesia,height=12, width=12,dpi=500)

Japan = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Japan") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Japan") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Japan",".png",sep=""),Japan,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Japan_exPOLES",".png",sep=""),Japan,height=12, width=12,dpi=500)

Russia = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("Russia") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("Russia") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/Russia",".png",sep=""),Russia,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/Russia_exPOLES",".png",sep=""),Russia,height=12, width=12,dpi=500)

USA = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("USA") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("USA") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/USA",".png",sep=""),USA,height=14, width=12,dpi=500)
#ggsave(file=paste(out,"/USA_exPOLES",".png",sep=""),USA,height=14, width=12,dpi=500)

China = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("China") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("China") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme + 
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/China",".png",sep=""),China,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/China_exPOLES",".png",sep=""),China,height=12, width=12,dpi=500)

India = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("India") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("India") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme +
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/India",".png",sep=""),India,height=12, width=12,dpi=500)
#ggsave(file=paste(out,"/India_exPOLES",".png",sep=""),India,height=12, width=12,dpi=500)

World = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("World") & Scenario %in% c("Delayed 450") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("World") & Scenario %in% c("Delayed 450") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme +
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00","CO2transport"="#4d4dff"))
ggsave(file=paste(out,"/World",".png",sep=""),World,height=13, width=12,dpi=500)
#ggsave(file=paste(out,"/World_exPOLES",".png",sep=""),World,height=13, width=12,dpi=500)

ASIA = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("ASIA") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("ASIA") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(out,"/ASIA",".png",sep=""),ASIA,height=12, width=12,dpi=500)

LAM = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("LAM") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("LAM") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))+
  ylim(-5000,3000)
ggsave(file=paste(out,"/LAM",".png",sep=""),LAM,height=12, width=12,dpi=500)

OECD = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("OECD90+EU") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("OECD90+EU") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))+
  ylim(-5000,3000)
ggsave(file=paste(out,"/OECD",".png",sep=""),OECD,height=12, width=12,dpi=500)

MAF = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("MAF") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("MAF") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(out,"/MAF",".png",sep=""),MAF,height=12, width=12,dpi=500)

REF = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("REF") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("REF") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(out,"/REF",".png",sep=""),REF,height=12, width=12,dpi=500)

ROWO = ggplot() +
  geom_bar(data=subset(poyemis,Region %in% c("ROWO") & value>0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  geom_bar(data=subset(poyemis,Region %in% c("ROWO") & value<0),
           aes(x=Model,y=value,fill=Variable),stat="Identity") +
  facet_grid(Scenario~Region) +
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2land"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(out,"/ROWO",".png",sep=""),ROWO,height=12, width=12,dpi=500)

