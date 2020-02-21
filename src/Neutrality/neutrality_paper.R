# Load data ---------------------------------------------------------------
setwd("~/disks/local/factsheet/src")
config <-"config_neutrality"
scencateg <- "scen_categ_V4"
variables <- "variables_neutrality"
adjust <- "adjust_reporting_neutrality" # TODO to check: remove MESSAGE for China due to region definition? and COFFEE & DNE for EU and DNE for China and India? and COFFEE for Japan?
addvars <- F
datafile <-"cdlinks_compare_20191118-083136"
source("load_data.R")

outdir <- "Neutrality/graphs"
if(!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}

library(Hmisc)

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
np=np[Scope=="global"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5REF","R5OECD90+EU")&!model=="COPPE-COFFEE 1.0"]

# write output: overview of original scenarios per category, model and region
u=np[,list(unique(scenario)),by=c("model","Category","region")]
write.csv(u,paste("Neutrality","/Scenario overview.csv",sep=""))


# Regional phase-out years ------------------------------------------------
# See functions - pbl_colors.r?
### Absolute ###
ghg=np[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Emissions|CO2|Energy and Industrial Processes")]
check=ghg[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
ghg=ghg[model%in%check$model]

## Extrapolate beyond 2100 to estimate phase-out year if not this century
ghge=ghg[period%in%c(2050:2100)&Category%in%c("1.5 °C","2 °C")] 
ghgextra=ghge[,list(approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$y,approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$x),by=c("Category","model","region","variable")]
setnames(ghgextra,"V1","value")
setnames(ghgextra,"V2","period")
ghg = select(ghg,-scenario,-Baseline,-Scope,-unit)
setcolorder(ghgextra,colnames(ghg))
ghgextra=rbind(ghgextra[period%in%c(2101:2200)],ghg[Category%in%c("1.5 °C","2 °C")])

poy=ghgextra[!duplicated(ghgextra[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE] #"Scope","Baseline","scenario"
poy=merge(poy,ghgextra[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1="No phase-out"

models=poy[,list(number=length(unique(model))),by=c('region','variable')]
poy=merge(poy, models, by=c('region','variable'))
poy$region <- paste(poy$region,' [',poy$number,' models]',sep="")
poy=poy[!number<3]

poyrange=data.table(poy[,list(median=median(V1, na.rm=T),min=min(V1, na.rm=T),max=max(V1, na.rm=T)),by=c("Category","region","variable")]) #"unit"

# change order for plotting
#poyrange = poyrange[order(Category,variable,median)]
poyrange$region <- factor(poyrange$region, levels=c("BRA [3 models]","CAN [3 models]","TUR [3 models]","USA [6 models]","EU [6 models]",
                                                    "RUS [3 models]","JPN [4 models]","IND [5 models]","CHN [6 models]","IDN [3 models]","World [6 models]")) #unique(poyrange$region)
#poy=poy[order(Category,variable,V1)]
poy$region<-factor(poy$region,levels=c("BRA [3 models]","CAN [3 models]","TUR [3 models]","USA [6 models]","EU [6 models]",
                                       "RUS [3 models]","JPN [4 models]","IND [5 models]","CHN [6 models]","IDN [3 models]","World [6 models]")) #levels=unique(poy$region)
poyrange$Category <- factor(poyrange$Category,levels=c("2 °C","1.5 °C"))
poy$Category <- factor(poy$Category,levels=c("2 °C","1.5 °C"))

# For plotting, including the ones with phase-out after 2100 or no phase-out at all
poy$label <-""
poy[V1>2100]$label <-">2100"
poy[is.na(V1),]$label <- "NA"
poy$showyear <- poy$V1
poy[V1>2100]$showyear <- 2105
poy[is.na(V1),]$showyear <- 2110

poyrange$label <-""
poyrange[max>2100]$label <- ">2100"
poyrange$showyear <- poyrange$max
poyrange[max>2100]$showyear <- 2105

S = ggplot()
#S = S + geom_errorbar(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region%in%c("SAF [2 models]","MEX [2 models]")], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill? #,size=0.2
S = S + geom_pointrange(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region%in%c("SAF [2 models]","MEX [2 models]")], aes(ymin=min,ymax=showyear,y=median, x=region, colour=variable),alpha=0.5,size=5,fatten=1,show.legend = F) #,position=position_dodge(width=0.66)
S = S + geom_point(data=poy[Category%in%c("2 °C","1.5 °C")&!region%in%c("SAF [2 models]","MEX [2 models]")],aes(x=region,y=showyear,shape=model,colour=variable),size=3) #,position=position_dodge(width=0.66)
S = S + geom_text(data=poy[Category%in%c("2 °C","1.5 °C")],stat="identity",aes(x=region,y=showyear,label=label),size=4)
S = S + geom_text(data=poyrange[Category%in%c("2 °C","1.5 °C")],stat="identity",aes(x=region,y=showyear,label=label),size=4)
S = S + geom_hline(data=poyrange[region=="World [6 models]"&Category%in%c("2 °C","1.5 °C")], aes(yintercept=median),linetype="dotted") 
#S = S + geom_boxplot(data=poy[Category%in%c("2 °C","1.5 °C")&!region%in%c("SAF [2 models]","MEX [2 models]")], aes(y=V1, x=region, colour=variable,fill=variable),position=position_dodge(width=0.66),width=0.66)
#S = S + geom_point(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region%in%c("SAF [2 models]","MEX [2 models]")], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66)) #,size=0.2
S = S + coord_flip()
S = S + facet_grid(Category~variable, scales="free_y", 
                   labeller=labeller(variable=c("Emissions|CO2"="CO2 (all)","Emissions|CO2|Energy and Industrial Processes"="CO2 (fossil&cement)","Emissions|Kyoto Gases"="Kyoto gases")))
S = S + ylab("Phase out year")+xlab("")
S = S + scale_y_continuous(limits=c(2030,2110),breaks=c(2030,2040,2050,2060,2070,2080,2090,2100,2110))
#S = S + scale_y_continuous(breaks=c(2030,2040,2050,2060,2070,2080,2090,2100,2110,2120,2130,2140,2150,2160,2170,2180,2190,2200))
S = S + theme_bw() + theme(axis.text.y=element_text(size=16)) + theme(strip.text=element_text(size=14)) + theme(axis.title=element_text(size=18)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size=14)) + theme(plot.title=element_text(size=18)) + theme(legend.position = "bottom") +
        theme(legend.text=element_text(size=11),legend.title=element_text(size=12))
ggsave(file=paste(outdir,"/Phase_out_year.png",sep=""),S,width=16, height=10, dpi=120)

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

# prep something needed for PCA
poyclass = poy[variable=="Emissions|Kyoto Gases" & Category%in%c("2 °C","1.5 °C")&!region=="World"]
poyclass = select(poyclass,-variable,-unit,-poy,-world,-years)

# continue with plotting relative to global
models=poy[,list(number=length(unique(model))),by=c('region','variable')]
poy=merge(poy, models, by=c('region','variable'))
poy$region <- paste(poy$region,' [',poy$number,' models]',sep="")
poy=poy[!number<3]

poyrange=data.table(poy[,list(median=median(years),min=min(years),max=max(years)),by=c("Category","region","unit","variable")])

S = ggplot()
S = S + geom_errorbar(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]","SAF [2 models]","MEX [2 models]")], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill?
S = S + geom_point(data=poyrange[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]","SAF [2 models]","MEX [2 models]")], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66))
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
har=fread("data/landuseinventory2019.csv",header=TRUE)
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
## Extrapolate beyond 2100 to estimate phase-out year if not this century
dthe=dth[period%in%c(2050:2100)&Category%in%c("1.5 °C","2 °C")] 
dthextra=dthe[,list(approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$y,approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$x),by=c("Category","model","region","variable")]
setnames(dthextra,"V1","value")
setnames(dthextra,"V2","period")
dth = select(dth,-scenario,-Baseline,-Scope)
setcolorder(dthextra,colnames(dth))
dthextra=rbind(dthextra[period%in%c(2101:2200)],dth)

poy=dthextra[!duplicated(dthextra[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE]
poy=merge(poy,dthextra[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1<-"No phase-out"
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
poy=poy[!number<3]

poyrange=data.table(poy[,list(median=median(years,na.rm=T),min=min(years,na.rm=T),max=max(years,na.rm=T)),by=c("Category","region","variable")])

poyrange2=poyrange
poyrange2$variable<-"Inventory AFOLU data"

  # Plotting ----------------------------------------------------------------
poy=rbind(poyrange1,poyrange2) 

# Plot
S1 = ggplot()
S1 = S1 + geom_errorbar(data=poy[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]")], aes(ymin=min,ymax=max, x=region, colour=variable),position=position_dodge(width=0.66),width=0.66) #variable as fill? #,"SAF [2 models]","MEX [2 models]"
S1 = S1 + geom_point(data=poy[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]")], aes(y=median,x=region,colour=variable),position=position_dodge(width=0.66))
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
poy1=poy1[!number<3]

poyrange1=data.table(poy1[,list(median=median(value,na.rm=T),min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","region","variable")]) #,"unit"
#change plot order
poyrange1$Category <- factor(poyrange1$Category,levels=c("2 °C","1.5 °C"))
poy1$Category <- factor(poy1$Category,levels=c("2 °C","1.5 °C"))

S2 = ggplot()
S2 = S2 + geom_errorbar(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]")&variable=="diff"], aes(ymin=min,ymax=max, x=region)) #, colour=variable #,"SAF [2 models]","MEX [2 models]"
S2 = S2 + geom_point(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region%in%c("World [6 models]")&variable=="diff"], aes(y=median,x=region,size=0.2),show.legend = F) #,colour=variable
S2 = S2 + geom_point(data=poy1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"&variable=="diff"], aes(y=value,x=region,colour=model,shape=model),size=3)
S2 = S2 + coord_flip()
S2 = S2 + facet_grid(.~Category, scales="free_y")
S2 = S2 + geom_hline(yintercept=0)
S2 = S2 + ylab("Phase-out year difference due to inventory vs. model LULUCF (<0: earlier if based on inventory)")+xlab("")
#S2 = S2 + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
S2 = S2 + theme_bw()+ theme(axis.text.y=element_text(size=16))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ #angle=45, 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_LULUCF_diff.png",sep=""),S2,width=12, height=8, dpi=120)

# plot trajectories to understand differences
np = np[Scope=="global"]
dt = np[variable=="Emissions|Kyoto Gases"]%>% select(-scenario,-Baseline,-unit,-Scope)
dt$landuse <- "Model data"
dth$landuse <- "Inventory data"
setcolorder(dt,colnames(dth))
dthcomp = rbind(dt,dth)

S3 = ggplot()
S3 = S3 + geom_path(data=dthcomp[Category%in%c("2 °C","1.5 °C")&!region%in%c("World","ARG","AUS","MEX",'ROK',"SAF","SAU")],aes(x=period,y=value,colour=Category,linetype=landuse))
S3 = S3 + facet_grid(region~model, scale="free_y")
S3 = S3 + xlab("") + ylab("Emissions|Kyoto Gases (MtCO2eq/year)")
S3 = S3 + theme_bw() + theme(axis.text.y=element_text(size=14))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_LULUCF_trajectory.png",sep=""),S3,width=12, height=8, dpi=120)


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

## Extrapolate beyond 2100 to estimate phase-out year if not this century
allocatione=allocation[period%in%c(2050:2100)&Category%in%c("1.5 °C","2 °C")] 
allocationextra=allocatione[,list(approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$y,approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$x),by=c("Category","model","region","variable")]
setnames(allocationextra,"V1","value")
setnames(allocationextra,"V2","period")
allocation = select(allocation,-scenario,-Baseline,-Scope)
setcolorder(allocationextra,colnames(allocation))
allocationextra=rbind(allocationextra[period%in%c(2101:2200)],allocation)

poy=allocationextra[!duplicated(allocationextra[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE] #,"Scope","Baseline","scenario"
poy=merge(poy,allocationextra[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1="No phase-out"
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
poy=poy[!number<3]

poyrange=data.table(poy[,list(median=median(years,na.rm=T),min=min(years,na.rm=T),max=max(years,na.rm=T)),by=c("Category","region","variable")]) #,"unit"

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

poyrange1=data.table(poy1[,list(median=median(value,na.rm=T),min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("Category","region","variable","number")]) #,"unit"
poyrange1=poyrange1[!number<2]
#For display
poyrange1[region=="RUS [2 models]"&Category=="2 °C"&variable=="diff"]$median<-100
poyrange1[region=="RUS [2 models]"&Category=="2 °C"&variable=="diff"]$min<-100
poyrange1[region=="RUS [2 models]"&Category=="2 °C"&variable=="diff"]$max<-100

#change plot order
poyrange1$Category <- factor(poyrange1$Category,levels=c("2 °C","1.5 °C"))
poy1$Category <- factor(poy1$Category,levels=c("2 °C","1.5 °C"))

a1 = ggplot()
a1 = a1 + geom_errorbar(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [5 models]"&variable=="diff"], aes(ymin=min,ymax=max, x=region)) #, colour=variable
a1 = a1 + geom_point(data=poyrange1[Category%in%c("2 °C","1.5 °C")&!region=="World [5 models]"&variable=="diff"], aes(y=median,x=region,size=0.2),show.legend = F) #,colour=variable
a1 = a1 + geom_point(data=poy1[Category%in%c("2 °C","1.5 °C")&!region=="World [6 models]"&variable=="diff"], aes(y=value,x=region,colour=model,shape=model),size=3)
a1 = a1 + coord_flip()
a1 = a1 + facet_grid(.~Category, scales="free_y")
a1 = a1 + geom_hline(yintercept=0)
a1 = a1 + scale_y_continuous(limits=c(-80,90),breaks=c(-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90))
a1 = a1 + geom_text(data=poyrange1[region=="RUS [2 models]"&Category=="2 °C"&variable=="diff"],stat="identity",aes(x=region,y=50,label="2077 > No phase-out"),size=6)
a1 = a1 + ylab("Phase-out year difference due to BECCS allocation (<0: earlier if based on biomass production)")+xlab("")
#a1 = a1 + scale_y_continuous(breaks=c(-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60))
a1 = a1 + theme_bw()+ theme(axis.text.y=element_text(size=16))+ theme(strip.text.x=element_text(size=14))+ theme(axis.title=element_text(size=18))+ #angle=45,
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))+ theme(plot.title=element_text(size=18))
ggsave(file=paste(outdir,"/Phase_out_year_allocation_BECCS_diff.png",sep=""),a1,width=12, height=8, dpi=120)


# Mitigation strategies / why are some regions earlier or later aka PCA prep---------------------------------------------------
# EU late vs. US early: space for afforestation (e.g. population density).  Check database for other reasons , e.g. non-CO2 share in 2015? 
# Mogelijk bevolkingsdichtheid of misschien productieve grond per persoon tegen jaar CO2 neutraliteit? Evt. ook iets van CCS capaciteit.
# TODO: add CO2 in transport/industry/buildings per capita instead of/next to share of total? Trade|Primary Energy|Biomass|Volume - just test all variables

### select data
rd=np[variable%in%c("Emissions|Kyoto Gases","Emissions|CH4","Emissions|N2O","Emissions|F-Gases","Emissions|CO2","Population","GDP|MER",
                    "Land Cover","Land Cover|Cropland","Land Cover|Cropland|Energy Crops","Land Cover|Forest|Afforestation and Reforestation","Land Cover|Forest",
                    "Agricultural Production|Energy","Yield|Oilcrops",
                    "Carbon Sequestration|CCS","Carbon Sequestration|CCS|Biomass","Carbon Sequestration|Land Use|Afforestation",
                    "Investment|Energy Efficiency","Investment|Energy Supply|Electricity|Electricity Storage",
                    "Emissions|CO2|Energy|Supply|Electricity","Secondary Energy|Electricity",
                    "Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Industry")
      ] #"2 °C (2030)", & Category%in%c("2 °C","1.5 °C")
ghg=rd[variable %in% c("Emissions|Kyoto Gases")]

### First calculate phase-out year (only for models with data until 2100)
check=ghg[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
rd=rd[model%in%check$model]
ghg=ghg[model%in%check$model]

## Extrapolate beyond 2100 to estimate phase-out year if not this century
ghge=ghg[period%in%c(2050:2100)&Category%in%c("1.5 °C","2 °C")] 
ghgextra=ghge[,list(approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$y,approxExtrap(x=period,y=value,xout=seq(2050,2200),method="linear")$x),by=c("Category","model","region")]
setnames(ghgextra,"V1","value")
setnames(ghgextra,"V2","period")
ghg = select(ghg,-scenario,-Baseline,-Scope,-unit,-variable)
setcolorder(ghgextra,colnames(ghg))
ghgextra=rbind(ghgextra[period%in%c(2101:2200)],ghg)

#check
p=ggplot() #[region=="CHN"&model=="AIM V2.1"&Category=="1.5 °C"]
p=p+facet_grid(region~model)
p=p+geom_line(data=ghg[Category%in%c("1.5 °C","2 °C")&region%in%c("BRA","CAN","CHN","EU","IDN","IND","JPN","RUS","TUR","USA")], aes(x = period, y=value,linetype=Category),color="black")
p=p+geom_line(data=ghgextra[Category%in%c("1.5 °C","2 °C")&region%in%c("BRA","CAN","CHN","EU","IDN","IND","JPN","RUS","TUR","USA")], aes(x = period, y=value,linetype=Category), color="red") #[region=="CHN"&model=="AIM V2.1"&Category=="1.5 °C"]
p=p+theme_bw()
print(p)

#calculate phase-out year
poy=ghgextra[!duplicated(ghgextra[,list(model,Category,region),with=TRUE]),!c('value','period'),with=FALSE] #variable "Scope","Baseline","scenario"
poy=merge(poy,ghgextra[value<=0,min(period),by=c('model','Category','region')],by=c('model','Category','region'),all=TRUE) #,'variable'
#poy$unit<-NULL
poy[is.na(V1),]$V1="No phase-out" #TODO also with extrapolation some NA - check what it does in PCA?
setnames(poy,"V1","period")

#add classification earlier/later/same as global average (for PCA plotting)
world=poy[region=="World"]
poy=merge(poy,world, by=c("model","Category"))
setnames(poy,"period.x","poy")
setnames(poy,"period.y","world")
poy$region.y<-NULL
setnames(poy,"region.x","region")
poy$diff=ifelse(is.na(poy$poy),"No phase-out",ifelse(poy$poy<poy$world,"earlier",ifelse(poy$poy>poy$world,"later","same")))
poyclass = poy[Category%in%c("2 °C","1.5 °C")&!region=="World"]
poyclass = select(poyclass,-poy,-world)

### Calculate indicators to plot on X-axis
## First interpolate to get 2015 data for MESSAGE, needed for some indicators TODO move to adjust reporting?
mes=spread(rd[period%in%c(2010,2020)&model=="MESSAGEix-GLOBIOM_1.1"],period,value)
mes = mes%>%mutate(`2015`=(`2010`+`2020`)/2)
mes = data.table(gather(mes,period,value,c(`2010`,`2015`,`2020`)))
mes = mes[period==2015]
rd = rbind(rd,mes)

mesg=spread(ghg[period%in%c(2010,2020)&model=="MESSAGEix-GLOBIOM_1.1"],period,value)
mesg = mesg%>%mutate(`2015`=(`2010`+`2020`)/2)
mesg = data.table(gather(mesg,period,value,c(`2010`,`2015`,`2020`)))
mesg = mesg[period==2015]
ghg = rbind(ghg,mesg)

## and replace 0 land cover for Japan in REMIND with POLES' value TODO move to adjust reporting? --> not used as REMIND is removed anyway, due to missing afforestation data
# land = rd[variable%in%c("Land Cover","Land Cover|Cropland")&region=="JPN"&model=="POLES CDL"]
# land$model <-"REMIND-MAgPIE 1.7-3.0"
# rd=rd[!c(model=="REMIND-MAgPIE 1.7-3.0"&region=="JPN"&variable%in%c("Land Cover","Land Cover|Cropland"))]
# rd=rbind(rd,land)

## Population density 
popd=rd[variable%in%c("Population","Land Cover")] #&!c(model=="REMIND-MAgPIE 1.7-3.0"&region=="JPN")
popd=spread(popd[,!c('unit'),with=FALSE],variable,value)
popd=popd%>%mutate(density=Population/`Land Cover`)
popd=data.table(gather(popd,variable,value,c("Population","Land Cover","density")))
popd=popd[variable=="density"]
popd$unit<-"persons/ha"

# average over models
popdm=popd[,list(mean(value,na.rm=TRUE)),by=c("Category","region","variable","unit","period")]
setnames(popdm,"V1","value")

poym=poy[,list(mean(period,na.rm=TRUE)),by=c("Category","region")] #,"variable"
setnames(poym,"V1","period")

popden=merge(popd,poy,by=c("model","Category","region"))
popdenm=merge(popdm,poym,by=c("Category","region"))

# Calculate Pearson correlation
popdencor = popden[,list(cor(value,period.y,method="pearson")),by=c("Category")]
popdenmcor = popdenm[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## Non-CO2 share in 2015
nonco2=rd[variable%in%c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases","Emissions|Kyoto Gases")]
nonco2=spread(nonco2[,!c('unit'),with=FALSE],variable,value)
nonco2=nonco2%>%mutate(nonCO2=`Emissions|CH4`*25+`Emissions|N2O`*0.298+`Emissions|F-Gases`,nonCO2share=nonCO2/`Emissions|Kyoto Gases`*100)
nonco2=data.table(gather(nonco2,variable,value,c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases","nonCO2","nonCO2share","Emissions|Kyoto Gases")))
nonco2=nonco2[variable=="nonCO2share"&period==2015]
nonco2$unit <- "%"

# average over models
nonco2m=nonco2[,list(mean(value,na.rm=TRUE)),by=c("Category","region","variable","unit","period")]
setnames(nonco2m,"V1","value")

nonco2emis=merge(nonco2,poy,by=c("model","Category","region"))
nonco2emism=merge(nonco2m,poym,by=c("Category","region"))

# Calculate Pearson correlation
nonco2cor = nonco2emis[,list(cor(value,period.y,method="pearson")),by=c("Category")]
nonco2mcor = nonco2emism[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## productive area per capita
prod=rd[variable%in%c("Land Cover|Cropland","Population")]
prod=spread(prod[,!c('unit'),with=FALSE],variable,value)
prod=prod%>%mutate(prodcap=`Land Cover|Cropland`/Population)
prod=data.table(gather(prod,variable,value,c("Land Cover|Cropland","Population","prodcap")))
prod=prod[variable=="prodcap"]
prod$unit <- "ha/person"

# average over models
prodm=prod[,list(mean(value,na.rm=TRUE)),by=c("Category","region","variable","unit","period")]
setnames(prodm,"V1","value")

prodcap=merge(prod,poy,by=c("model","Category","region"))
prodcapm=merge(prodm,poym,by=c("Category","region"))

# Calculate Pearson correlation
prodcapcor = prodcap[,list(cor(value,period.y,method="pearson")),by=c("Category")]
prodcapmcor = prodcapm[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## Afforestation
forest=rd[variable=="Land Cover|Forest|Afforestation and Reforestation"]
#add 0 for AIM and REMIND (not reported) to make PCA work --> removing these models as afforestation is not 0 in many countries
# aimforest = rd[variable=="Population" & model=="AIM V2.1"]
# aimforest$value<-0
# aimforest$variable<-"Land Cover|Forest|Afforestation and Reforestation"
# remindforest = rd[variable=="Population" & model=="REMIND-MAgPIE 1.7-3.0"]
# remindforest$value<-0
# remindforest$variable<-"Land Cover|Forest|Afforestation and Reforestation"
# forest=rbind(forest,aimforest,remindforest)
afforest = merge(forest,poy,by=c("model","Category","region"))

# Calculate Pearson correlation
afforestcor = afforest[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## CCS
ccs = rd[variable%in%c("Carbon Sequestration|CCS","Emissions|Kyoto Gases")]
ccs = spread(ccs[,!c('unit'),with=FALSE],variable,value)
ccs = ccs%>%mutate(CCSshare=abs(`Carbon Sequestration|CCS`/(`Emissions|Kyoto Gases`+`Carbon Sequestration|CCS`)*100))
ccs = data.table(gather(ccs,variable,value,c("Carbon Sequestration|CCS","Emissions|Kyoto Gases","CCSshare")))
ccs = ccs[variable=="CCSshare"]
ccs$unit <-"%"
ccscap = merge(ccs,poy,by=c("model","Category","region"))

# Calculate Pearson correlation
ccscor = ccscap[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## GDP per capita
gdpcap=rd[variable%in%c("GDP|MER","Population")]
gdpcap=spread(gdpcap[,!c('unit'),with=FALSE],variable,value)
gdpcap=gdpcap%>%mutate(gdpcap=`GDP|MER`/Population*1000)
gdpcap=data.table(gather(gdpcap,variable,value,c("GDP|MER","Population","gdpcap")))
gdpcap=gdpcap[variable=="gdpcap"]
gdpcap$unit <- "USD/person"

# average over models
gdpcapm=gdpcap[,list(mean(value,na.rm=TRUE)),by=c("Category","region","variable","unit","period")]
setnames(gdpcapm,"V1","value")

gdpcappoy=merge(gdpcap,poy,by=c("model","Category","region"))
gdpcappoym=merge(gdpcapm,poym,by=c("Category","region"))

## Land cover cropland share of total
cropshare=rd[variable%in%c("Land Cover","Land Cover|Cropland")]
cropshare=spread(cropshare[,!c('unit'),with=FALSE],variable,value)
cropshare=cropshare%>%mutate(cropshare=`Land Cover|Cropland`/`Land Cover`*100)
cropshare=data.table(gather(cropshare,variable,value,c("Land Cover|Cropland","Land Cover","cropshare")))
cropshare=cropshare[variable=="cropshare"]
cropshare$unit <- "%"

# average over models
cropsharem=cropshare[,list(mean(value,na.rm=TRUE)),by=c("Category","region","variable","unit","period")]
setnames(cropsharem,"V1","value")

cropsharepoy=merge(cropshare,poy,by=c("model","Category","region"))
cropsharepoym=merge(cropsharem,poym,by=c("Category","region"))

## Land cover forest share of total
forestshare=rd[variable%in%c("Land Cover","Land Cover|Forest")]
#temporary fix: put POLES forest cover for SAU at 0 (rather than negative values, incorrect), as instructed by Jacques
forestpoles=forestshare[model=="POLES CDL" & variable=="Land Cover|Forest" & region=="SAU"]
#forestpoles$value=forestpoles[period==2010&Category=="2 °C"]$value
forestpoles$value <- 0
forestshare=rbind(forestshare[!c(model=="POLES CDL"&region=="SAU"&variable=="Land Cover|Forest")],forestpoles)
forestshare=spread(forestshare[,!c('unit'),with=FALSE],variable,value)
forestshare=forestshare%>%mutate(forestshare=`Land Cover|Forest`/`Land Cover`*100)
forestshare=data.table(gather(forestshare,variable,value,c("Land Cover|Forest","Land Cover","forestshare")))
forestshare=forestshare[variable=="forestshare"]
forestshare$unit <- "%"

## Baseline growth
blg = ghg[Category=="No policy"&period%in%c(2015,2050,2100)]
blg = spread(blg,period,value)
blg = blg%>%mutate(blgrowth50=(`2050`-`2015`)/`2015`*100,blgrowth100=(`2100`-`2015`)/`2015`*100)
blg = data.table(gather(blg,period,value,c(`2015`,`2050`,`2100`,"blgrowth50","blgrowth100")))
blg = blg[period%in%c("blgrowth50","blgrowth100")]
blg$unit <-"%"
blg50=blg[period=="blgrowth50"]
blg50$period<-2050
blg50$variable <- "BaselineGHG2050"
blg50copy = blg50
blg50$Category <- "1.5 °C" 
blg50copy$Category <- "2 °C"
blg100=blg[period=="blgrowth100"]
blg100$period<-2100
blg100$variable <- "BaselineGHG2100"
blg100copy = blg100
blg100$Category <- "1.5 °C"
blg100copy$Category <- "2 °C"
blg50 = rbind(blg50,blg50copy)
blg100 = rbind(blg100,blg100copy)

## Emissions intensity electricity sector
emisint=rd[variable%in%c("Emissions|CO2|Energy|Supply|Electricity","Secondary Energy|Electricity")]
emisint=spread(emisint[,!c('unit'),with=FALSE],variable,value)
emisint=emisint%>%mutate(emisint=`Emissions|CO2|Energy|Supply|Electricity`/`Secondary Energy|Electricity`)
emisint=data.table(gather(emisint,variable,value,c("Emissions|CO2|Energy|Supply|Electricity","Secondary Energy|Electricity","emisint")))
emisint=emisint[variable=="emisint"]
emisint$unit <- "Mt CO2 / EJ"

## Emissions per capita
emiscap=rd[variable%in%c("Emissions|Kyoto Gases","Population")]
emiscap=spread(emiscap[,!c('unit'),with=FALSE],variable,value)
emiscap=emiscap%>%mutate(emiscap=`Emissions|Kyoto Gases`/Population)
emiscap=data.table(gather(emiscap,variable,value,c("Emissions|Kyoto Gases","Population","emiscap")))
emiscap=emiscap[variable=="emiscap"]
emiscap$unit <- "tCO2e/person"

## Emissions share demand sectors
transportshare=rd[variable%in%c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2")]
transportshare=spread(transportshare[,!c('unit'),with=FALSE],variable,value)
transportshare=transportshare%>%mutate(transportshare=`Emissions|CO2|Energy|Demand|Transportation`/`Emissions|CO2`*100)
transportshare=data.table(gather(transportshare,variable,value,c("Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2","transportshare")))
transportshare=transportshare[variable=="transportshare"]
transportshare$unit <- "%"

buildingshare=rd[variable%in%c("Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2")]
buildingshare=spread(buildingshare[,!c('unit'),with=FALSE],variable,value)
buildingshare=buildingshare%>%mutate(buildingshare=`Emissions|CO2|Energy|Demand|Residential and Commercial`/`Emissions|CO2`*100)
buildingshare=data.table(gather(buildingshare,variable,value,c("Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2","buildingshare")))
buildingshare=buildingshare[variable=="buildingshare"]
buildingshare$unit <- "%"

industryshare=rd[variable%in%c("Emissions|CO2|Energy|Demand|Industry","Emissions|CO2")]
industryshare=spread(industryshare[,!c('unit'),with=FALSE],variable,value)
industryshare=industryshare%>%mutate(industryshare=`Emissions|CO2|Energy|Demand|Industry`/`Emissions|CO2`*100)
industryshare=data.table(gather(industryshare,variable,value,c("Emissions|CO2|Energy|Demand|Industry","Emissions|CO2","industryshare")))
industryshare=industryshare[variable=="industryshare"]
industryshare$unit <- "%"

# Old: plot indicators vs. phase-out year ---------------------------------
### plot indicators vs. phase-out year
## Population density
# per model
# pd = ggplot(popden[period.x==2015])
# pd = pd + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# pd = pd + facet_grid(model~Category)
# pd = pd + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100),limits=c(2020,2110))
# pd = pd + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                                strip.text=element_text(size=18))
# pd = pd + labs(x="Population density in 2015 (persons/ha)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_population-density_models",".png",sep=""),pd,height=12, width=16,dpi=500)
# 
# # average over models
# pdm = ggplot(popdenm[period.x==2015])
# pdm = pdm + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# pdm = pdm + facet_grid(~Category,scales="fixed")
# pdm = pdm + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100),limits=c(2040,2110))
# pdm = pdm + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                                strip.text=element_text(size=18))
# pdm = pdm + labs(x="Population density in 2015 (persons/ha)",y="Phase-out year of GHG emissions")
# #pdm = pdm + geom_text(data=popdenm[period.y==2105],stat="identity",aes(x=value,y=2105,label="no phase-out"),size=6) #TODO how to calculate average without the no pase-out but show the no phase-out (marked as 2105)?
# ggsave(file=paste(outdir,"/poy_vs_population-density",".png",sep=""),pdm,height=12, width=16,dpi=500)
# 
# ## Non-CO2
# # per model
# nc = ggplot(nonco2emis)
# nc = nc + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# nc = nc + facet_grid(model~Category)
# nc = nc + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                              strip.text=element_text(size=18))
# nc = nc + labs(x="Non-CO2 GHG emissions share in 2015 (%)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_non-CO2-share_models",".png",sep=""),nc,height=12, width=16,dpi=500)
# 
# # average over models
# ncm = ggplot(nonco2emism)
# ncm = ncm + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# ncm = ncm + facet_grid(~Category)
# ncm = ncm + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                              strip.text=element_text(size=18))
# ncm = ncm + labs(x="Non-CO2 GHG emissions share in 2015 (%)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_non-CO2-share",".png",sep=""),ncm,height=12, width=16,dpi=500)
# 
# ## Productive area per capita
# # per model
# pc = ggplot(prodcap[period.x==2015])
# pc = pc + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# pc = pc + facet_grid(model~Category)
# pc = pc + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                              strip.text=element_text(size=18))
# pc = pc + labs(x="Cropland area per capita in 2015 (ha/person)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_productive-area-capita_models",".png",sep=""),pc,height=12, width=16,dpi=500)
# 
# # average over models
# pcm = ggplot(prodcapm[period.x==2015])
# pcm = pcm + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# pcm = pcm + facet_grid(~Category)
# pcm = pcm + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                                strip.text=element_text(size=18))
# pcm = pcm + labs(x="Cropland area per capita in 2015 (ha/person)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_productive-area-capita",".png",sep=""),pcm,height=12, width=16,dpi=500)
# 
# ## Afforestation
# # per model
# af = ggplot(afforest[period.x==2030&!region=="World"])
# af = af + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# af = af + facet_grid(model~Category)
# af = af + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                              strip.text=element_text(size=18))
# af = af + labs(x="Afforestation and reforestation in 2030 (million ha)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_afforestation_models",".png",sep=""),af,height=12, width=16,dpi=500)
# 
# ## CCS capacity
# # per model
# cc = ggplot(ccscap[period.x==2100&!region=="World"])
# cc = cc + geom_point(aes(x=value,y=period.y,colour=region),size=3)
# cc = cc + facet_grid(model~Category)
# cc = cc + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
#                              strip.text=element_text(size=18))
# cc = cc + labs(x="CCS in 2100 (MtCO2/year)",y="Phase-out year of GHG emissions")
# ggsave(file=paste(outdir,"/poy_vs_ccs_models",".png",sep=""),cc,height=12, width=16,dpi=500)


# Principal Component Analysis --------------------------------------------
#select data and put in right format TODO add more explanatory variables for bigger dataset? also add ccs in 2015, 2020, etc.
popdx = select(popd[period==2015],-scenario,-Baseline,-Scope)
nonco2x = select(nonco2[period==2015],-scenario,-Baseline,-Scope)
prodx = select(prod[period==2015],-scenario,-Baseline,-Scope)
forestx = select(forest[period==2050],-scenario,-Baseline,-Scope)
ccsx = select(ccs[period==2050],-scenario,-Baseline,-Scope)
gdpcapx = select(gdpcap[period==2015],-scenario,-Baseline,-Scope)
cropsharex = select(cropshare[period==2015],-scenario,-Baseline,-Scope)
forestsharex = select(forestshare[period==2015],-scenario,-Baseline,-Scope)
emisintx = select(emisint[period==2015],-scenario,-Baseline,-Scope)
emiscapx = select(emiscap[period==2015],-scenario,-Baseline,-Scope)
industrysharex = select(industryshare[period==2015],-scenario,-Baseline,-Scope)
buildingsharex = select(buildingshare[period==2015],-scenario,-Baseline,-Scope)
transportsharex = select(transportshare[period==2015],-scenario,-Baseline,-Scope)
blg50x = blg50
blg100x = blg100
setcolorder(blg50x,colnames(popdx))
setcolorder(blg100x,colnames(popdx))
# blg50x = select(blg50,-scenario,-Baseline,-Scope)
# blg100x = select(blg100,-scenario,-Baseline,-Scope)
setnames(poy,"poy","value")
poy$period<-"x"
poy$unit<-"Year"
poy$variable<-"Emissions|Kyoto Gases"
poy$world<-NULL
poy$diff<-NULL #add this later because columns need to be the same
setcolorder(poy,colnames(ccsx))
pca=rbind(popdx,nonco2x,prodx,forestx,ccsx,gdpcapx,cropsharex,forestsharex,emisintx,emiscapx,transportsharex,buildingsharex,industrysharex,blg50x,blg100x,poy)
#same but without phase-out years as rows for scatter plot
scat=rbind(popdx,nonco2x,prodx,forestx,ccsx,gdpcapx,cropsharex,forestsharex,emisintx,emiscapx,transportsharex,buildingsharex,industrysharex,blg50x,blg100x)
#continue with pca dataset - separately for model median and all models on one big pile
pcamedian=data.table(pca[!model%in%c("AIM V2.1","REMIND-MAgPIE 1.7-3.0"),list(value=median(value,na.rm=T)),by=c("Category","region","period","variable","unit")])
scatmedian=data.table(scat[!model%in%c("AIM V2.1","REMIND-MAgPIE 1.7-3.0"),list(value=median(value,na.rm=T)),by=c("Category","region","period","variable","unit")])
#put in right format: all models
pca=spread(pca[,!c('unit','period'),with=FALSE],variable,value)
pca=data.table(gather(pca,variable,value,c(`Emissions|Kyoto Gases`)))
pca$variable<-NULL
pca=pca[!model%in%c("AIM V2.1","REMIND-MAgPIE 1.7-3.0")] #add when they do add afforestation reporting?
#put in right format: median
pcamedian=spread(pcamedian[,!c('unit','period'),with=FALSE],variable,value)
pcamedian=data.table(gather(pcamedian,variable,value,c(`Emissions|Kyoto Gases`)))
pcamedian$variable<-NULL

# First: all scatterplots for visual inspection
poyscat=poy
poyscat$variable<- NULL
poyscat$unit<- NULL
poyscat$period<- NULL
setnames(poyscat,"value","poy")
scat=merge(scat, poyscat, by=c("Category","model","region"))
poyscatmedian=poyscat[,list(poy=median(poy,na.rm=T)),by=c("Category","region")]
scatmedian=merge(scatmedian,poyscatmedian,by=c("Category","region"))

library(RColorBrewer)
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

s = ggplot(scat[Category%in%c("2 °C","1.5 °C")&!region=="World"])
s = s + geom_point(aes(x=value,y=poy,colour=region,shape=Category),size=3)
s = s + scale_color_manual(values=mycolors)
#s = s + geom_text(aes(x=value,y=poy,label=region))
s = s + facet_wrap(~variable,scales="free_x",nrow=3,ncol=5)
s = s + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100,2120,2140),limits=c(2020,2150))
s = s + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
                               strip.text=element_text(size=18))
s = s + labs(x="",y="Phase-out year of GHG emissions")
ggsave(file=paste(outdir,"/poy_scatterplot_models",".png",sep=""),s,height=14, width=18,dpi=500)

#separately for 1.5 and 2 C
s1 = ggplot(scat[Category%in%c("1.5 °C")&!region=="World"])
s1 = s1 + geom_point(aes(x=value,y=poy,colour=region,shape=Category),size=3)
s1 = s1 + scale_color_manual(values=mycolors)
#s = s + geom_text(aes(x=value,y=poy,label=region))
s1 = s1 + facet_wrap(~variable,scales="free_x",nrow=3,ncol=5)
s1 = s1 + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100,2120,2140),limits=c(2020,2150))
s1 = s1 + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
                           strip.text=element_text(size=18))
s1 = s1 + labs(x="",y="Phase-out year of GHG emissions")
ggsave(file=paste(outdir,"/poy_scatterplot_models_1p5",".png",sep=""),s1,height=14, width=18,dpi=500)

s2 = ggplot(scat[Category%in%c("2 °C")&!region=="World"])
s2 = s2 + geom_point(aes(x=value,y=poy,colour=region,shape=Category),size=3)
s2 = s2 + scale_color_manual(values=mycolors)
#s = s + geom_text(aes(x=value,y=poy,label=region))
s2 = s2 + facet_wrap(~variable,scales="free_x",nrow=3,ncol=5)
s2 = s2 + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100,2120,2140),limits=c(2020,2150))
s2 = s2 + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
                             strip.text=element_text(size=18))
s2 = s2 + labs(x="",y="Phase-out year of GHG emissions")
ggsave(file=paste(outdir,"/poy_scatterplot_models_2",".png",sep=""),s2,height=14, width=18,dpi=500)

# and for median
s3 = ggplot(scatmedian[Category%in%c("2 °C","1.5 °C")&!region=="World"])
s3 = s3 + geom_point(aes(x=value,y=poy,colour=region,shape=Category),size=3)
s3 = s3 + scale_color_manual(values=mycolors)
#s3 = s3 + geom_text(aes(x=value,y=poy,label=region))
s3 = s3 + facet_wrap(~variable,scales="free_x",nrow=3,ncol=5)
s3 = s3 + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100,2120,2140),limits=c(2020,2150))
s3 = s3 + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
                           strip.text=element_text(size=18))
s3 = s3 + labs(x="",y="Phase-out year of GHG emissions")
ggsave(file=paste(outdir,"/poy_scatterplot_median",".png",sep=""),s3,height=14, width=18,dpi=500)

# Continue with PCA: For all models together and per individual model / median
#pca=data.table(pca)
pca=pca[Category%in%c("2 °C","1.5 °C")&!region%in%c("World")]
pca$ID <-with(pca,paste0(region,"-",value))
pca=merge(pca,poyclass,by=c("Category","model","region"))

#split out different groups
pcaP = pca[model=="POLES CDL"]
pcaP$ID <-with(pcaP,paste0(region,"-",value))
pca=pca[region%in%c("BRA","CAN","TUR","USA","EU","RUS","JPN","IND","CHN","IDN")]
pcaPI = pca[model%in%c("IMAGE 3.0","POLES CDL")]
pcanoNA = na.omit(pca)
pcamedian=pcamedian[Category%in%c("2 °C","1.5 °C")&region%in%c("BRA","CAN","TUR","USA","EU","RUS","JPN","IND","CHN","IDN")]
pcamedian$ID <-with(pcamedian,paste0(region,"-",value))
pcamedian1=pcamedian[Category%in%c("1.5 °C")]
pcamedian2=pcamedian[Category%in%c("2 °C")]
#TODO add early late grouping for median

#old: per model
# pcaI = pca[model=="IMAGE 3.0"]
# pcaI$ID <-with(pcaI,paste0(region,"-",value))
# pcaA = pca[model=="AIM V2.1"]
# pcaA$ID <-with(pcaA,paste0(region,"-",value))
# pcaM = pca[model=="MESSAGEix-GLOBIOM_1.1"]
# pcaM$ID <-with(pcaM,paste0(region,"-",value))
# pcaR = pca[model=="REMIND-MAgPIE 1.7-3.0"]
# pcaR$ID <-with(pcaR,paste0(region,"-",value))
# pcaW = pca[model=="WITCH2016"]
# pcaW$ID <-with(pcaW,paste0(region,"-",value))

#old: per model --- calculate principal components - TODO fix what it does with remaining NA phase-out year
# pcaI.pca <- prcomp(pcaI[,c(4:12)], center = TRUE,scale. = TRUE)
# summary(pcaI.pca)
# str(pcaI.pca)
# pcaA.pca <- prcomp(pcaA[,c(4:7,9:12)], center = TRUE,scale. = TRUE)
# summary(pcaA.pca)
# # pcaM.pca <- prcomp(pcaM[,c(4:8)], center = TRUE,scale. = TRUE)
# # summary(pcaM.pca)
# # pcaR.pca <- prcomp(pcaR[,c(4:7)], center = TRUE,scale. = TRUE)
# # summary(pcaR.pca)
# pcaW.pca <- prcomp(pcaW[,c(4:12)], center = TRUE,scale. = TRUE)
# summary(pcaW.pca)
# pca.pca <- prcomp(pca[,c(4:8)], center = TRUE,scale. = TRUE)
# summary(pca.pca)

#only POLES, all regions
pcaP.pca <- prcomp(pcaP[,c(4:18)], center = TRUE,scale. = TRUE)
POLESsum=data.frame(summary(pcaP.pca)$importance)
POLESrot=pcaP.pca$rotation
POLESpca=rbind(POLESrot,POLESsum)
write.csv(POLESpca,paste("Neutrality","/POLESpca.csv",sep=""))
# all models, 10 regions
pca.pca <- prcomp(pca[,c(4:18)], center = TRUE,scale. = TRUE)
#summary(pca.pca)
#str(pca.pca)
allsum=data.frame(summary(pca.pca)$importance)
allrot=pca.pca$rotation
allpca=rbind(allrot,allsum)
write.csv(allpca,paste("Neutrality","/allpca.csv",sep=""))
#all models, 10 regions, omit NA phase-out
pcanoNA.pca <- prcomp(pcanoNA[,c(4:18)], center = TRUE,scale. = TRUE)
noNAsum=data.frame(summary(pcanoNA.pca)$importance)
noNArot=pcanoNA.pca$rotation
noNApca=rbind(noNArot,noNAsum)
write.csv(noNApca,paste("Neutrality","/noNApca.csv",sep=""))
#model median, 10 regions
pcamedian.pca<-prcomp(pcamedian[,c(3:17)], center = TRUE,scale. = TRUE)
mediansum=data.frame(summary(pcamedian.pca)$importance)
medianrot=pcamedian.pca$rotation
medianpca=rbind(medianrot,mediansum)
write.csv(medianpca,paste("Neutrality","/medianpca.csv",sep=""))
#separately for 1.5 and 2C
pcamedian1.pca<-prcomp(pcamedian1[,c(3:17)], center = TRUE,scale. = TRUE)
median1sum=data.frame(summary(pcamedian1.pca)$importance)
median1rot=pcamedian1.pca$rotation
median1pca=rbind(median1rot,median1sum)
write.csv(median1pca,paste("Neutrality","/median1pca.csv",sep=""))
pcamedian2.pca<-prcomp(pcamedian2[,c(3:17)], center = TRUE,scale. = TRUE)
median2sum=data.frame(summary(pcamedian2.pca)$importance)
median2rot=pcamedian2.pca$rotation
median2pca=rbind(median2rot,median2sum)
write.csv(median2pca,paste("Neutrality","/median2pca.csv",sep=""))
#only POLES and IMAGE, 10 regions
pcaPI.pca <- prcomp(pcaPI[,c(4:18)], center = TRUE,scale. = TRUE)
POLESIMAGEsum=data.frame(summary(pcaPI.pca)$importance)
POLESIMAGErot=pcaPI.pca$rotation
POLESIMAGEpca=rbind(POLESIMAGErot,POLESIMAGEsum)
write.csv(POLESIMAGEpca,paste("Neutrality","/POLESIMAGEpca.csv",sep=""))

#plot TODO for other models individually?
library(ggbiplot)
# pI = ggbiplot(pcaI.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcaI$ID, groups=pcaI$Category)  +
#   #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
#   ggtitle("PCA of regional phase-out years in IMAGE")+
#   theme_bw()+
#   theme(legend.position = "bottom")
# ggsave(file=paste(outdir,"/PCA_IMAGE",".png",sep=""),pI,height=12, width=16,dpi=500)

pall = ggbiplot(pca.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pca$ID, groups=pca$diff)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_all_early-late-grouping",".png",sep=""),pall,height=12, width=16,dpi=500)

pPI = ggbiplot(pcaPI.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcaPI$ID, groups=pcaPI$diff)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_POLES-IMAGE_early-late-grouping",".png",sep=""),pPI,height=12, width=16,dpi=500)

pP = ggbiplot(pcaP.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcaP$ID, groups=pcaP$diff)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_POLES_early-late-grouping",".png",sep=""),pP,height=12, width=16,dpi=500)

pnoNA = ggbiplot(pcanoNA.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcanoNA$ID, groups=pcanoNA$diff)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_noNA_early-late-grouping",".png",sep=""),pnoNA,height=12, width=16,dpi=500)

#to add early / late grouping TODO
pmedian = ggbiplot(pcamedian.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcamedian$ID, groups=pcamedian$Category)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_median_scenario-grouping",".png",sep=""),pmedian,height=12, width=16,dpi=500)

pmedian1 = ggbiplot(pcamedian1.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcamedian1$ID)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_median_1p5",".png",sep=""),pmedian1,height=12, width=16,dpi=500)

pmedian2 = ggbiplot(pcamedian2.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcamedian2$ID)  +  #,choices=c(3,4) #groups=pca$model (try Category, diff, model, region, value?)
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_median_2",".png",sep=""),pmedian2,height=12, width=16,dpi=500)

# With a different package
#install.packages(c("FactoMineR","factoextra"))
library("FactoMineR")
library("factoextra")
library("corrplot")
pca.active <- pca[,4:18]
res.pca <- PCA(pca.active)
print(res.pca)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca,addlabels = T,ylim=c(0,50))
var <- get_pca_var(res.pca)
var
head(var$coord)
head(var$cos2)
head(var$contrib)
fviz_pca_var(res.pca,col.var="black")
corrplot(var$cos2,is.corr=F)
fviz_cos2(res.pca,choice="var",axes=1:2)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
corrplot(var$contrib, is.corr=FALSE)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# TODO continute with this http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# from color by groups


# Regression with top 5 variables based on PCA, scatterplots, Lasso --------
model <- lm(value ~ scale(BaselineGHG2100) + scale(forestshare) + scale(cropshare) + scale(prodcap) + scale(transportshare), data = pcaPI)
summary(model)

#check difference with other top 5: only the one from Lasso
model <- lm(value ~ scale(BaselineGHG2100) + scale(forestshare) + scale(cropshare) + scale(`Land Cover|Forest|Afforestation and Reforestation`) + scale(transportshare), data = pcaPI)
summary(model)

#check difference with other top 5: only the one from PCA
model <- lm(value ~ scale(prodcap) + scale(gdpcap) + scale(buildingshare) + scale(transportshare) + scale(emisint), data = pcaPI)
summary(model)

#check difference with other top 5: only the one from scatterplots
model <- lm(value ~ scale(BaselineGHG2100) + scale(forestshare) + scale(nonCO2share) + scale(CCSshare) + scale(transportshare), data = pcaPI)
summary(model)

# Hans method: 3 models, with x1-x5, x6-x10, x11-x15, then lm with the most important ones
model1<- lm(value ~ scale(density) + scale(nonCO2share) + scale(prodcap) + scale(`Land Cover|Forest|Afforestation and Reforestation`) + scale(CCSshare), data = pcaPI)
summary(model1)

model2<- lm(value ~ scale(gdpcap) + scale(cropshare) + scale(forestshare) + scale(emisint) + scale(emiscap), data = pcaPI)
summary(model2)

model3<- lm(value ~ scale(transportshare) + scale(buildingshare) + scale(industryshare) + scale(BaselineGHG2050) + scale(BaselineGHG2100), data = pcaPI)
summary(model3)

#combined model
model4<- lm(value ~ scale(CCSshare)+scale(transportshare)+scale(`Land Cover|Forest|Afforestation and Reforestation`)+scale(forestshare),data=pcaPI)
summary(model4)

# New scatterplots only for the top 4, including straight line fit
s5 = ggplot(scat[Category%in%c("2 °C","1.5 °C")&region%in%unique(pcaPI$region)&model%in%c("IMAGE 3.0","POLES CDL")
                 &variable%in%c("forestshare","Land Cover|Forest|Afforestation and Reforestation","transportshare","CCSshare")])
s5 = s5 + geom_point(aes(x=value,y=poy,colour=region,shape=Category),size=3)
#s5 = s5 + abline(lm(scat$poy~scat$value))
s5 = s5 + geom_quantile(method="rq",aes(x=value,y=poy),formula=y~x, stat="quantile", quantiles = 0.5, lty = "solid")
s5 = s5 + geom_quantile(method="rq",aes(x=value,y=poy),formula=y~x, stat="quantile", quantiles = c(0.1,0.9), lty = "dashed")
s5 = s5 + scale_color_manual(values=mycolors)
#s = s + geom_text(aes(x=value,y=poy,label=region))
s5 = s5 + facet_wrap(~variable,scales="free_x",nrow=2,ncol=2)
s5 = s5 + scale_y_continuous(breaks=c(2020,2040,2060,2080,2100,2120,2140),limits=c(2020,2150))
s5 = s5 + theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=18),legend.text=element_text(size=18),legend.title=element_text(size=18),
                           strip.text=element_text(size=18))
s5 = s5 + labs(x="",y="Phase-out year of GHG emissions")
ggsave(file=paste(outdir,"/poy_scatterplot_PI_top4",".png",sep=""),s5,height=14, width=18,dpi=500)


# Emissions in phase-out year ---------------------------------------------
# Graph: Emissions in phase-out year (like Joeri’s)
# En dus bijvoorbeeld ook de strategie waarlangs een regio neutraliteit krijgt (meer uit reductie emissies, over meer uit negatieve emissies).

# select data
bd=np[variable %in% c("Emissions|CH4","Emissions|F-Gases","Emissions|Kyoto Gases","Emissions|N2O","Emissions|CO2","Emissions|CO2|AFOLU",
                        "Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand",
                        "Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Industry", #"Emissions|CO2|Energy|Demand|AFOFI",
                        "Emissions|CO2|Energy|Demand|Transportation","Carbon Sequestration|CCS","Carbon Sequestration|CCS|Biomass","Carbon Sequestration|Land Use") 
      & Category%in%c("2 °C","1.5 °C")] # ,"2 °C (2030)"
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
setnames(poyemis,"Carbon Sequestration|Land Use","CSland")
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
poyemis$nonCO2 <- poyemis$CH4+poyemis$N2O+poyemis$Fgases
poyemis$CO2demand <- poyemis$CO2transport+poyemis$CO2buildings+poyemis$CO2industry

#CCS: calculating CCS without bio from total to avoid double counting, and converting to negative emissions (stored amounts reported as positive numbers)
poyemis$CCS=poyemis$CCS-poyemis$CCSbio
poyemis$CCS=poyemis$CCS*-1
poyemis$CCSbio=poyemis$CCSbio*-1
poyemis$CSland=poyemis$CSland*-1
poyemisccs=poyemis

#Gather for plotting (exclude CCS because of double counting - plot separately)
poyemis=gather(poyemis,variable,value,c(CO2demand,CO2supply,CO2afolu,nonCO2)) #CH4,CO2industry,CO2buildings,CO2transport,CO2supply,CO2afolu,Fgases,N2O
poyemis$CCS<-NULL
poyemis$CCSbio<-NULL
poyemis$CO2agriculture<-NULL
poyemis=data.table(poyemis)
#poyemis$model <- paste(poyemis$model,' [',poyemis$period,']',sep="")

poyemisccs=data.table(gather(poyemisccs,variable,value,c(CCS,CCSbio,CSland)))
poyemisccs=poyemisccs[ ,`:=`("CH4" = NULL, "CO2industry" = NULL, "CO2buildings"=NULL,"CO2transport"=NULL,"CO2supply"=NULL,"CO2afolu"=NULL,
                        "Fgases"=NULL,"N2O"=NULL)]

# Legend order
poyemis$variable=factor(poyemis$variable, levels=c("nonCO2","CO2demand","CO2supply","CO2afolu")) #CH4 Fgases N2O CO2buildings CO2industry CO2transport CO2supply CO2afolu

#check if each scenario category has only one scenario
check=poyemis[,list(number=length(unique(scenario))),by=c('model','Category','region')]

#Mean per scenario category per model
poyemis=poyemis[,mean(value,na.rm=TRUE),by=c('Category','region','variable','model','period')]
setnames(poyemis,"V1","value")

# poyemis=poyemis[!model=="MESSAGEix-GLOBIOM_1.0"]
# poyemisccs=poyemisccs[!model=="MESSAGEix-GLOBIOM_1.0"]

#change plot order
poyemis$Category <- factor(poyemis$Category,levels=c("2 °C","1.5 °C"))

#Stacked bar chart remaining emissions in poy - add phase-out year somewhere for regional graphs?
library(gridExtra)
# common theme
ttheme = theme_bw() + 
  theme(axis.text.x=element_text(angle=30, size=18, hjust=0.75)) + theme(axis.text.y=element_text(size=18)) + theme(axis.title=element_text(size=18)) + theme(axis.title.y=element_text(size=14)) + theme(legend.text=element_text(size=18)) + theme(legend.title=element_text(size=18)) + theme(strip.text=element_text(size=18))

b1 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("BRA","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("BRA","EU","RUS")&variable=="nonCO2"],stat="identity",aes(x=model,y=980,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("nonCO2"="#E69F00","CO2demand"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec"))
  # "CCS"="#999999","CCSbio"="#9aff9a","CH4"="#E69F00","CO2buildings"="#72bcd4","CO2industry"="#0080ff","CO2transport"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec","Fgases"="#b36200","N2O"="#D55E00"
ggsave(file=paste(outdir,"/emissions_breakdown_poy_BRA-EU-RUS",".png",sep=""),b1,height=12, width=16,dpi=500)

b2 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("CAN","JPN","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("CAN","JPN","TUR")&variable=="nonCO2"],stat="identity",aes(x=model,y=250,label=period),size=6) +
  facet_grid(region~Category) + #scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("nonCO2"="#E69F00","CO2demand"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec"))
ggsave(file=paste(outdir,"/emissions_breakdown_poy_CAN-JPN-TUR",".png",sep=""),b2,height=12, width=16,dpi=500)

b3 = ggplot() +
  geom_bar(data=poyemis[value>0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemis[value<0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemis[region%in%c("CHN","IND","USA")&variable=="nonCO2"],stat="identity",aes(x=model,y=1600,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Emissions in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("nonCO2"="#E69F00","CO2demand"="#4d4dff","CO2afolu"="#009E73","CO2supply"="#c1e1ec"))
ggsave(file=paste(outdir,"/emissions_breakdown_poy_CHN-IND-USA",".png",sep=""),b3,height=12, width=16,dpi=500)

c1 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("JPN","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("JPN","EU","RUS")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("JPN","EU","RUS")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CSland"="#009E73"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_JPN-EU-RUS",".png",sep=""),c1,height=12, width=16,dpi=500)

c2 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("CAN","BRA","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("CAN","BRA","TUR")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("CAN","BRA","TUR")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CSland"="#009E73"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_CAN-BRA-TUR",".png",sep=""),c2,height=12, width=16,dpi=500)

c3 = ggplot() +
  geom_bar(data=poyemisccs[value>0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_bar(data=poyemisccs[value<0&region%in%c("CHN","IND","USA")],aes(x=model,y=value,fill=variable),stat="Identity") +
  geom_text(data=poyemisccs[region%in%c("CHN","IND","USA")&variable=="CCS"],stat="identity",aes(x=model,y=0,label=period),size=6) +
  facet_grid(region~Category) + #,scales="free_y"
  labs(y=bquote("Carbon sequestration in phase-out year (Mt"~CO[2]~"eq/year)"),x="") +
  ttheme+
  scale_fill_manual(values=c("CCS"="#999999","CCSbio"="#9aff9a","CSland"="#009E73"))
ggsave(file=paste(outdir,"/CCS_breakdown_poy_CHN-IND-USA",".png",sep=""),c3,height=12, width=16,dpi=500)



# Pathways (mitigation strategies) ----------------------------------------
setcolorder(dthextra,colnames(ghgextra))
setcolorder(allocationextra,colnames(ghgextra))
dthextra$variable = paste(dthextra$variable,"|Inventory",sep="")
paths=rbind(ghgextra,dthextra,allocationextra[variable=="Emissions|CO2|Allocation"])

# calculate mean/range
pathsrange=data.table(paths[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),min=min(value,na.rm=T),max=max(value,na.rm=T)),
                            by=c("Category","region","variable","period")])
pathsrange=pathsrange[!Category=="2 °C (2030)"&!variable=="Emissions|CO2|Energy and Industrial Processes"&period%in%c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)]

#To change plot order: 2, 1.5, USA, IND, GHG, GHG inventory, CO2, CO2 allocation
pathsrange$Category = factor(pathsrange$Category,levels=c("2 °C","1.5 °C"))
pathsrange$region = factor(pathsrange$region,levels=c("USA","IND"))
pathsrange$variable = factor(pathsrange$variable,levels=c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Inventory","Emissions|CO2","Emissions|CO2|Allocation"))

p = ggplot(data=pathsrange[region%in%c("USA","IND")])
p = p + geom_line(aes(x=period,y=mean,colour=Category))
p = p + geom_ribbon(aes(x=period,ymin=min,ymax=max,fill=Category),alpha=0.3)
p = p + facet_grid(variable~region, scale="free_y",labeller=labeller(variable=c("Emissions|CO2"="CO2","Emissions|CO2|Allocation"="CO2 (allocation)",
                                                                                "Emissions|Kyoto Gases"="GHG","Emissions|Kyoto Gases|Inventory"="GHG (inventory)")))
p = p + geom_hline(yintercept=0)
p = p + xlab("") + ylab("Emissions (MtCO2eq/year)")
p = p + theme_bw() + theme(axis.text.y=element_text(size=14))+ theme(strip.text=element_text(size=16))+ theme(axis.title=element_text(size=18))+ 
  theme(axis.text.x = element_text(size=14))+ theme(plot.title=element_text(size=18))+theme(legend.text=element_text(size=18))+theme(legend.title=element_text(size=18))
ggsave(file=paste(outdir,"/Pathways_IND_USA.png",sep=""),p,width=12, height=8, dpi=120)

