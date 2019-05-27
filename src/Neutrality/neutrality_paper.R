# Load data ---------------------------------------------------------------
setwd("~/disks/local/factsheet/src")
config <-"config_xCut"
scencateg <- "scen_categ_V4"
variables <- "variables_neutrality"
adjust <- "adjust_reporting_neutrality" # TODO to decide: remove MESSAGE for China due to region definition? and COFFEE & DNE for EU and DNE for China and India?
addvars <- F
datafile <-"cdlinks_compare_20190416-124844"
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
poymodel=poy
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
S = S + geom_errorbar(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill?
S = S + geom_point(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66))
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
poyinventory=poy

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
S1 = S1 + geom_errorbar(data=poy[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill?
S1 = S1 + geom_point(data=poy[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66))
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

# Other plot: difference between model and inventory data (not relative to world)
poy1=merge(poymodel[variable=="Emissions|Kyoto Gases"],poyinventory,by=c("model","Category","region","variable"))

# calculate difference with and without inventory data for easier plot
poy1=poy1%>%mutate(diff=V1.y-V1.x)
poy1=data.table(gather(poy1,variable,value,c("V1.y","V1.x","diff")))

models=poy1[,list(number=length(unique(model))),by=c('region','variable')]
poy1=merge(poy1, models, by=c('region','variable'))
poy1$region <- paste(poy1$region,' [',poy1$number,' models]',sep="")
poy1=poy1[!number<2]

poyrange1=data.table(poy1[,list(median=median(value),min=min(value),max=max(value)),by=c("Category","region","variable")]) #,"unit"

S2 = ggplot()
S2 = S2 + geom_errorbar(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"&variable=="diff"], aes(ymin=min,ymax=max, x=region)) #, colour=variable
S2 = S2 + geom_point(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [8 models]"&variable=="diff"], aes(y=median,x=region)) #,colour=variable
#S2 = S2 + geom_point(data=poy1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"], aes(y=poy,x=region,colour=model,shape=variable),size=2)
S2 = S2 + coord_flip()
S2 = S2 + facet_grid(.~Category, scales="free_y")
S2 = S2 + geom_hline(yintercept=0)
S2 = S2 + ylab("Phase-out year difference due to inventory vs. model LULUCF (<0: earlier if based on inventory)")
#S2 = S2 + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
S2 = S2 + theme_bw()+ theme(axis.text.y=element_text(angle=45, size=16))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_LULUCF_diff.png",sep=""),S2,width=12, height=8, dpi=120)


# Effect of allocation of negative emissions ------------------------------

#	Allocation negative emissions: check CO2 BECCS, add to region it belongs to, then allocate based on biomass production --> check effect on neutrality   
# Check effect of different allocations (either to biomass producer or electricity/CCS) on phase-out years. 
# E.g. energy crop production, CCSbiomass carbon sequestration and trade-primary energy-biomass-volume?
alloc=np[variable%in%c("Agricultural Production|Energy","Carbon Sequestration|CCS|Biomass","Emissions|CO2") #|Energy and Industrial Processes, "Trade|Primary Energy|Biomass|Volume",
         & Category%in%c("2 °C","2 °C (2030)","1.5 °C")]
allocw1=alloc[region=="World"&variable=="Agricultural Production|Energy"]
allocw2=alloc[region=="World"&variable=="Carbon Sequestration|CCS|Biomass"]
alloc$unit<-NULL
alloc=spread(alloc,variable,value)
alloc=alloc%>%mutate(EmisCO2BECCS=`Emissions|CO2`+`Carbon Sequestration|CCS|Biomass`)
alloc=merge(alloc,allocw1,by=c("scenario","Category","Baseline","model","period","Scope"))
alloc=merge(alloc,allocw2,by=c("scenario","Category","Baseline","model","period","Scope"))
alloc=alloc%>%mutate(CCSbioshare=(`Agricultural Production|Energy`/value.x)*value.y,`Emissions|CO2|Allocation`=EmisCO2BECCS-CCSbioshare)
alloc=data.table(alloc)
allocation=alloc[ ,`:=`("Agricultural Production|Energy" = NULL, "Carbon Sequestration|CCS|Biomass" = NULL, "EmisCO2BECCS"=NULL,"region.y"=NULL,"variable.x"=NULL,"unit.x"=NULL,
                        "value.x"=NULL,"region"=NULL,"variable.y"=NULL,"unit.y"=NULL,"value.y"=NULL,"CCSbioshare"=NULL)]
setnames(allocation,"region.x","region")
allocation=data.table(gather(allocation,variable,value,c("Emissions|CO2","Emissions|CO2|Allocation")))

# check effect on phase-out year
check=allocation[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
allocation=allocation[model%in%check$model]

poy=allocation[!duplicated(allocation[,list(model,Category,region,variable),with=TRUE]),!c('value','period',"Scope","Baseline","scenario"),with=FALSE]
poy=merge(poy,allocation[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1=2105
poy1=poy
world=poy[region=="World"]
poy=merge(poy,world, by=c("model","Category","variable")) #,"unit"
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

poyrange=data.table(poy[,list(median=median(years),min=min(years),max=max(years)),by=c("Category","region","variable")]) #,"unit"

a = ggplot()
a = a + geom_errorbar(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill?
a = a + geom_point(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66))
#a = a + geom_point(data=poy[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"], aes(y=poy,x=region,colour=model,shape=variable),size=2)
a = a + coord_flip()
a = a + facet_grid(.~Category, scales="free_y")
a = a + geom_hline(yintercept=0)
a = a + ylab("Phase out year relative to world (years)")
#a = a + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
a = a + theme_bw()+ theme(axis.text.y=element_text(angle=45, size=16))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ 
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_allocation_BECCS.png",sep=""),a,width=11, height=8, dpi=120)

# calculate difference with and without allocation for easier plot
poy1=spread(poy1,variable,V1)
poy1=poy1%>%mutate(diff=`Emissions|CO2|Allocation`-`Emissions|CO2`)
poy1=data.table(gather(poy1,variable,value,c("Emissions|CO2|Allocation","Emissions|CO2","diff")))

models=poy1[,list(number=length(unique(model))),by=c('region','variable')]
poy1=merge(poy1, models, by=c('region','variable'))
poy1$region <- paste(poy1$region,' [',poy1$number,' models]',sep="")
poy1=poy1[!number<2]

poyrange1=data.table(poy1[,list(median=median(value),min=min(value),max=max(value)),by=c("Category","region","variable")]) #,"unit"

a1 = ggplot()
a1 = a1 + geom_errorbar(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"&variable=="diff"], aes(ymin=min,ymax=max, x=region)) #, colour=variable
a1 = a1 + geom_point(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"&variable=="diff"], aes(y=median,x=region)) #,colour=variable
#a1 = a1 + geom_point(data=poy1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"], aes(y=poy,x=region,colour=model,shape=variable),size=2)
a1 = a1 + coord_flip()
a1 = a1 + facet_grid(.~Category, scales="free_y")
a1 = a1 + geom_hline(yintercept=0)
a1 = a1 + ylab("Phase-out year difference due to BECCS allocation (<0: earlier if based on biomass production)")
#a1 = a1 + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
a1 = a1 + theme_bw()+ theme(axis.text.y=element_text(angle=45, size=16))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_allocation_BECCS_diff.png",sep=""),a1,width=12, height=8, dpi=120)


# Mitigation strategies / why are some regions earlier or later ---------------------------------------------------
# EU late vs. US early: space for afforestation (e.g. population density).  Check database for other reasons , e.g. non-CO2 share in 2015? 
# Mogelijk bevolkingsdichtheid of misschien productieve grond per persoon tegen jaar CO2 neutraliteit? Evt. ook iets van CCS capaciteit.
#	TODO Graph: X indicator with effect on neutrality, e.g. afforestation  capacity; Y phase-out year 


# Emissions in phase-out year ---------------------------------------------
# Graph: Emissions in phase-out year (like Joeri’s)
# En dus bijvoorbeeld ook de strategie waarlangs een regio neutraliteit krijgt (meer uit reductie emissies, over meer uit negatieve emissies).

# select data
bd=np[variable %in% c("Emissions|CH4","Emissions|F-Gases","Emissions|Kyoto Gases","Emissions|N2O","Emissions|CO2","Emissions|CO2|AFOLU",
                        "Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand",
                        "Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Industry", #"Emissions|CO2|Energy|Demand|AFOFI",
                        "Emissions|CO2|Energy|Demand|Transportation","Carbon Sequestration|CCS","Carbon Sequestration|CCS|Biomass") 
      & Category%in%c("2 °C","2 °C (2030)","1.5 °C")]
ghg=bd[variable %in% c("Emissions|Kyoto Gases")]

# To make sure we only use the models with data until 2100, important for this indicator
check=ghg[,list(unique(period)), by=c("model")]
check=subset(check, subset=V1=="2100")
ghg=subset(ghg, subset=model %in% check$model)

# Calculate phase-out year
poy=ghg[!duplicated(ghg[,list(model,Category,region,variable),with=TRUE]),!c('value','period',"Scope","Baseline","scenario"),with=FALSE]
poy=merge(poy,ghg[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy$unit<-NULL
poy=na.omit(poy)
setnames(poy,"V1","period")

#selection for merge
emis=spread(bd[,!c('unit'),with=FALSE],variable,value)
poyemis=merge(poy,emis, by=c('model','Category','region','period'))
poyemis$variable<-NULL

#Calculations for final plotting
setnames(poyemis,"Emissions|CH4","CH4")
setnames(poyemis,"Emissions|N2O","N2O")
poyemis$CH4=poyemis$CH4*25
poyemis$N2O=poyemis$N2O*298/1000

#Selection of categories to avoid double counting
setnames(poyemis,"Emissions|CO2","CO2")
setnames(poyemis,"Carbon Sequestration|CCS","CCS")
setnames(poyemis,"Carbon Sequestration|CCS|Biomass","CCSbio")
setnames(poyemis,"Emissions|CO2|Energy and Industrial Processes","CO2ffi")
setnames(poyemis,"Emissions|CO2|Energy|Demand","CO2demand")
#setnames(poyemis,"Emissions|CO2|Energy|Demand|AFOFI","CO2agriculture") 
setnames(poyemis,"Emissions|CO2|Energy|Demand|Industry","CO2industry")
setnames(poyemis,"Emissions|CO2|Energy|Demand|Residential and Commercial","CO2buildings")
setnames(poyemis,"Emissions|CO2|Energy|Demand|Transportation","CO2transport")
setnames(poyemis,"Emissions|CO2|Energy|Supply","CO2supply")
setnames(poyemis,"Emissions|CO2|AFOLU","CO2afolu")
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
poyemisccs=poyemis

#Gather for plotting (exclude CCS because of double counting - plot separately)
poyemis=gather(poyemis,variable,value,c(CH4,CO2industry,CO2buildings,CO2transport,CO2supply,CO2afolu,Fgases,N2O))
poyemis$CCS<-NULL
poyemis$CCSbio<-NULL
poyemis$CO2agriculture<-NULL
poyemis=data.table(poyemis)
#poyemis$model <- paste(poyemis$model,' [',poyemis$period,']',sep="")

poyemisccs=data.table(gather(poyemisccs,variable,value,c(CCS,CCSbio)))
poyemisccs=poyemisccs[ ,`:=`("CH4" = NULL, "CO2industry" = NULL, "CO2buildings"=NULL,"CO2transport"=NULL,"CO2supply"=NULL,"CO2afolu"=NULL,
                        "Fgases"=NULL,"N2O"=NULL)]

# Legend order
poyemis$variable=factor(poyemis$variable, levels=c("CH4","Fgases","N2O","CO2buildings","CO2industry","CO2transport","CO2supply","CO2afolu"))

#check if each scenario category has only one scenario
check=poyemis[,list(number=length(unique(scenario))),by=c('model','Category','region')]

#Mean per scenario category per model
poyemis=poyemis[,mean(value,na.rm=TRUE),by=c('Category','region','variable','model','period')]
setnames(poyemis,"V1","value")

poyemis=poyemis[!model=="MESSAGEix-GLOBIOM_1.0"]
poyemisccs=poyemisccs[!model=="MESSAGEix-GLOBIOM_1.0"]

#Stacked bar chart remaining emissions in poy - add phase-out year somewhere for regional graphs?
library(gridExtra)
# common theme
ttheme = theme_bw() + 
  theme(axis.text.x=element_text(angle=30, size=18, hjust=0.75)) + theme(axis.text.y=element_text(size=18)) + theme(axis.title=element_text(size=18)) + theme(axis.title.y=element_text(size=14)) + theme(legend.text=element_text(size=18)) + theme(legend.title=element_text(size=18)) + theme(strip.text=element_text(size=18))

b1 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("BRA","EU","RUS")&variable=="CH4"],stat="identity",aes(x=model,y=980,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(outdir,"/emissions_breakdown_poy_BRA-EU-RUS",".png",sep=""),b1,height=12, width=16,dpi=500)

b2 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("CAN","JPN","TUR")&variable=="CH4"],stat="identity",aes(x=model,y=250,label=period),size=6) +
  facet_grid(region~Category) + #scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(outdir,"/emissions_breakdown_poy_CAN-JPN-TUR",".png",sep=""),b2,height=12, width=16,dpi=500)

b3 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("CHN","IND","USA")&variable=="CH4"],stat="identity",aes(x=model,y=1600,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"))
ggsave(file=paste(outdir,"/emissions_breakdown_poy_CHN-IND-USA",".png",sep=""),b3,height=12, width=16,dpi=500)

c1 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("BRA","EU","RUS")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_BRA-EU-RUS",".png",sep=""),c1,height=12, width=16,dpi=500)

c2 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("CAN","JPN","TUR")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_CAN-JPN-TUR",".png",sep=""),c2,height=12, width=16,dpi=500)

c3 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("CHN","IND","USA")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_CHN-IND-USA",".png",sep=""),c3,height=12, width=16,dpi=500)

