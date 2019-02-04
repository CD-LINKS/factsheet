# Load data ---------------------------------------------------------------
setwd("~/disks/local/factsheet/src")
config <-"config_xCut"
scencateg <- "scen_categ_V4"
variables <- "variables_neutrality"
source("load_data.R")

mods=unique(all[period=="2010"& variable=="Emissions|CO2"& region=="EU" & value>1000,model,with=TRUE])
all2=subset(all, model %in% mods & region=="EU")
all=rbind(subset(all, !region=="EU"),all2)

# Removing MESSAGE model for India, as MESSAGE has South Asia, not India separately
India = all[region=="India"]
India = India[!model=="MESSAGE V.4"]
all=rbind(subset(all, !region=="India"),India)

# Removing MESSAGE model for EU, as MESSAGE has EU including Turkey
EU = all[region=="EU"]
EU = EU[!model=="MESSAGE V.4"]
all=rbind(subset(all, !region=="EU"),EU)

# write output: overview of original scenarios per category, model and region
u=all[,list(unique(scenario)),by=c("model","Category","region")]
write.csv(u,paste("Neutrality","/Scenario overview.csv",sep=""))


# Regional phase-out years ------------------------------------------------

# TODO: continue cleaning up / selecting / adjusting from here
# GHG vs. CO2, fossil/energy vs. including land
# Graph like Joeri’s
# Only individual regions, not R5
# See functions - pbl_colors.r?

### Policy brief ###
#2c: phase-out years for NPi1000 for world and major / R5 regions
poy=all[Category=="2 °C"&variable%in%c("Emissions|CO2")&Scope=="global"&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #!region=="Bunkers"
check=poy[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
poy=poy[model%in%check$model]
poy2=poy[!duplicated(poy[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE]
poy=merge(poy2,poy[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poy[is.na(V1),]$V1=2100
setnames(poy,"V1","value")
poy$value=as.numeric(poy$value)
poyrange=data.table(poy[,list(median=median(value),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable")])
poyrange$unit<-"Mt CO2-equiv/yr"

F2c=ggplot(poyrange)
#F2c=F2c+facet_grid(~variable,scale="fixed")
F2c=F2c+geom_point(aes(x=region,y=median,colour=Category),size=5) #,colour="#a50000"
F2c=F2c+geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=unit))
F2c=F2c+coord_flip()
F2c=F2c+scale_colour_manual(name="Statistics",values=c("2 °C"="#a50000","Mt CO2-equiv/yr"="black"),labels=c("2 °C"="median","Mt CO2-equiv/yr"="10-90 percentile range"))
F2c=F2c+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),plot.title = element_text(size=22),legend.text=element_text(size=16),legend.title=element_text(size=18)) #,axis.text.x=element_text(angle=45)
F2c=F2c+theme(legend.position = c(0.2,0.1))
F2c=F2c+ggtitle(bquote(paste("c) Phase-out year of ",CO[2], " emissions")))
F2c=F2c+ylab("")  + xlab("")
ggsave(file=paste0(cfg$outdir,"/","F2c",".png"),F2c, width=11, height=8, dpi=300)

### CLIMA/MILES ###
dt=dat[Variable %in% c("Emissions|Kyoto Gases") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450", ,"Realistic 450"
write.csv(dt,paste(outt,"/GHG_delayed_fullset.csv",sep=""))
write.xlsx(dt,paste(outt,"/GHG_delayed_fullset.xlsx",sep=""))

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)
write.csv(dt,paste(outt,"/GHG_delayed_2100.csv",sep=""))
write.xlsx(dt,paste(outt,"/GHG_delayed_2100.xlsx",sep=""))

poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
write.csv(poy,paste(outt,"/POY_GHG_delayed_2100.csv",sep=""))
write.xlsx(poy,paste(outt,"/POY_GHG_delayed_2100.xlsx",sep=""))

#poy=na.omit(poy)
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

#check if each scenario category has only one scenario
check=poy[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

# !! Mean per scenario category per model - fix (doesn't work with factor) !!
#poy=poy[,mean(V1,na.rm=TRUE),by=c('Scenario','Region','Variable','Model')]

# poy=poy[Region%in%c("USA [4 models]","Turkey [1 models]","South Korea [1 models]","South Africa [1 models]",
#          "Russia [2 models]","Mexico [1 models]","Japan [2 models]","Indonesia [1 models]","India [3 models]",
#          "EU [4 models]","China [4 models]","Canada [1 models]","Brazil [1 models]")]

S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year Kyoto gas emissions"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_reg.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_reg_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)

### repeat for first calculating mean per model

dt=dt[,mean(value,na.rm=TRUE),by=c('Scenario','Model','Region','Variable','Unit','Year')]
setnames(dt,"V1","value")

poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario','Region','Variable')],by=c('Model','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
write.csv(poy,paste(outt,"/POY_GHG_delayed_2100_1scenpermodel.csv",sep=""))
write.xlsx(poy,paste(outt,"/POY_GHG_delayed_2100_1scenpermodel.xlsx",sep=""))

#poy=na.omit(poy)
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")
poy=poy[!number<2]

# poy=poy[Region%in%c("USA [4 models]","Turkey [1 models]","South Korea [1 models]","South Africa [1 models]",
#          "Russia [2 models]","Mexico [1 models]","Japan [2 models]","Indonesia [1 models]","India [3 models]",
#          "EU [4 models]","China [4 models]","Canada [1 models]","Brazil [1 models]")]

S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year Kyoto gas emissions"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_reg_1scenpermodel.png",sep=""),S,width=11, height=8, dpi=120)

### relative to global ###
poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario','Region','Variable')],by=c('Model','Scenario','Region','Variable'),all=TRUE)
poy[is.na(V1),]$V1=2105
world=poy[Region=="World"]
poy=merge(poy,world, by=c("Model","Scenario","Variable","Unit"))
setnames(poy,"V1.x","poy")
setnames(poy,"V1.y","world")
poy$Region.y<-NULL
setnames(poy,"Region.x","Region")
poy$diff=ifelse(poy$poy<poy$world,"earlier",ifelse(poy$poy>poy$world,"later","same"))
poy$years=poy$poy-poy$world

models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")
poy=poy[!number<2]

S = ggplot()
S = S + geom_point(data=poy[Scenario=="Delayed 450"], aes(y=Region, x=years, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + geom_vline(xintercept=0)
#S = S + geom_rect()
S = S + xlab("Phase out year relative to world (years)")
S = S + scale_x_continuous(breaks=c(-50,-40,-30,-20,-10,0,10,20))
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_reg_1scenpermodel_diffworld.png",sep=""),S,width=11, height=8, dpi=120)

### CO2 ###

dt=dat[Variable %in% c("Emissions|CO2") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450", ,"Realistic 450"
write.csv(dt,paste(outt,"/CO2_delayed_fullset.csv",sep=""))
write.xlsx(dt,paste(outt,"/CO2_delayed_fullset.xlsx",sep=""))

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)
write.csv(dt,paste(outt,"/CO2_delayed_2100.csv",sep=""))
write.xlsx(dt,paste(outt,"/CO2_delayed_2100.xlsx",sep=""))

poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
#poy=na.omit(poy)
write.csv(poy,paste(outt,"/POY_CO2_delayed_2100.csv",sep=""))
write.xlsx(poy,paste(outt,"/POY_CO2_delayed_2100.xlsx",sep=""))

models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

#check if each scenario category has only one scenario
check=poy[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

# !! Mean per scenario category per model - fix (doesn't work with factor) !!
#poy=poy[,mean(V1,na.rm=TRUE),by=c('Scenario','Region','Variable','Model')]

S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year"~CO[2]~"emissions"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_CO2_450.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_CO2_450_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)

### repeat for first calculating mean per model

dt=dt[,mean(value,na.rm=TRUE),by=c('Scenario','Model','Region','Variable','Unit','Year')]
setnames(dt,"V1","value")

poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario','Region','Variable')],by=c('Model','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
#poy=na.omit(poy)
write.csv(poy,paste(outt,"/POY_CO2_delayed_2100_1scenpermodel.csv",sep=""))
write.xlsx(poy,paste(outt,"/POY_CO2_delayed_2100_1scenpermodel.xlsx",sep=""))

models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")
poy=poy[!number<2]

S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year"~CO[2]~"emissions"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_CO2_450_1scenpermodel.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_CO2_450_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)



# Effect of LULUCF definitions --------------------------------------------

#	Land CO2 in models vs. in inventories: effect on neutrality of different definitions 
# Graph: ‘harmonisation’ effect on phase-out year (don't call it harmonisation)

### MILES ###
dt=dat[Variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|Land Use") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450",,"Realistic 450"
write.csv(dt,paste(outt,"/Harmo_delayed_fullset.csv",sep=""))
write.xlsx(dt,paste(outt,"/Harmo_delayed_fullset.xlsx",sep=""))

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)
write.csv(dt,paste(outt,"/Harmo_delayed_2100_input.csv",sep=""))
write.xlsx(dt,paste(outt,"/Harmo_delayed_2100_input.xlsx",sep=""))

# Read data for harmonisation
har=fread("harmonisation.csv",header=TRUE)
har1=har[Variable=="Emissions|CO2|Land Use"]

# Merge and calculate model deviation in 2010
dt1=dt[Year==2010 & Variable=="Emissions|CO2|Land Use"]
harmo=merge(dt1,har1, by=c("Region","Variable"))
setnames(harmo,"value.x","model")
setnames(harmo,"value.y","hist")
harmo=harmo %>% mutate(factor=model-hist)
harmo$model<-NULL
harmo$hist<-NULL 
harmo$Year<-NULL
write.csv(harmo,paste(outt,"/Harmo_delayed_2100_offset.csv",sep=""))
write.xlsx(harmo,paste(outt,"/Harmo_delayed_2100_offset.xlsx",sep=""))

# Harmonise - only land use
dth=merge(dt[Variable=="Emissions|CO2|Land Use"],harmo,by=c("Region","Variable","Scenario_original","Scenario","Model","Unit"))
dth=dth %>% mutate(harmonised=value-factor)

# Intermezzo: plot harmonised/unharmonised pathways
dthplot=data.table(gather(dth,Harmo,value,c('value','factor','harmonised')))
setnames(dthplot,"Harmo","Harmonisation")
library(stringr)
dthplot$Harmonisation<-str_replace_all(dthplot$Harmonisation,"harmonised","Harmonised")
dthplot$Harmonisation<-str_replace_all(dthplot$Harmonisation,"value","Unharmonised")
dthplot$Harmonisation<-str_replace_all(dthplot$Harmonisation,"factor","Offset")
h = ggplot (dthplot[Region%in%c("China","EU","India","Japan","Russia","USA")&Scenario=="Delayed 450"])
h = h + geom_line(aes(x=Year, y=value,colour=Harmonisation)) #,linetype=Model
h = h + facet_grid(Region~Model,scales='free_y')
h = h + theme_bw() + theme(legend.text=element_text(size=14), legend.title=element_text(size=16),axis.text=element_text(size=14),
                           axis.title=element_text(size=16),strip.text=element_text(size=14),axis.text.x=element_text(angle=90))
h = h + ylab("Land use CO2 emissions (Mt CO2/yr) - Delayed 450")
ggsave(file=paste(out,"/landuseCO2_pathways_harmo.png",sep=""),h,width=13, height=10, dpi=120)

#continue calculations  
dth$factor<-NULL
dth$value<-NULL
setnames(dth,"harmonised","value")
dth$Variable<-"Emissions|CO2|Land Use|Harmo"
dth=data.table(dth)
dth=setcolorder(dth,c("Scenario_original","Scenario","Model","Region","Variable","Unit","Year","value"))
write.csv(dth,paste(outt,"/Harmo_delayed_2100_harmonised.csv",sep=""))
write.xlsx(dth,paste(outt,"/Harmo_delayed_2100_harmonised.xlsx",sep=""))

# Add harmonised land use tot GHG excl. land use
dtm=rbind(dt,dth)
dtm=spread(dtm[,!c('Unit'),with=FALSE],Variable, value)
dtm=na.omit(dtm)
dtm=dtm %>% mutate(`Emissions|Kyoto Gases|Harmo`=`Emissions|Kyoto Gases` - `Emissions|CO2|Land Use` + `Emissions|CO2|Land Use|Harmo`)
dtm=gather(dtm,Variable,value,c(`Emissions|Kyoto Gases|Harmo`,`Emissions|Kyoto Gases`, `Emissions|CO2|Land Use`,`Emissions|CO2|Land Use|Harmo`))
dtm=data.table(dtm)

# Intermezzo: plot harmonised/unharmonised pathways
dtmplot=dtm[Variable%in%c("Emissions|Kyoto Gases|Harmo","Emissions|Kyoto Gases")]
# dtmplot$Variable<-str_replace_all(dtmplot$Variable,"Emissions|Kyoto Gases|Harmo","Harmonised")
# dtmplot$Variable<-str_replace_all(dtmplot$Variable,"Emissions|Kyoto Gases","Unharmonised")
setnames(dtmplot,"Variable","Harmonisation")
#dtmplot=dtmplot[!Year%in%c(2005,2015,2025,2035,2045,2055,2065,2075,2085,2095)]
h = ggplot (dtmplot[Region%in%c("China","EU","India","Japan","Russia","USA")&Scenario=="Delayed 450"])
h = h + geom_line(aes(x=Year, y=value,colour=Harmonisation)) #,linetype=Model
h = h + facet_grid(Region~Model,scales='free_y')
h = h + theme_bw() + theme(legend.text=element_text(size=14), legend.title=element_text(size=16),axis.text=element_text(size=14),
                           axis.title=element_text(size=16),strip.text=element_text(size=14),axis.text.x=element_text(angle=90))
h = h + ylab("GHG emissions (Mt CO2eq/yr) - Delayed 450")
ggsave(file=paste(out,"/GHG_pathways_harmo.png",sep=""),h,width=13, height=10, dpi=120)

#continue calculations
dth=dtm[Variable=="Emissions|Kyoto Gases|Harmo"]
dth$Variable<-"Emissions|Kyoto Gases"
write.csv(dth,paste(outt,"/Harmo_delayed_2100_harmonised_GHG.csv",sep=""))
write.xlsx(dth,paste(outt,"/Harmo_delayed_2100_harmonised_GHG.xlsx",sep=""))

# Calculate phase-out year
poy=dth[!duplicated(dth[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dth[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
#poy=na.omit(poy)
write.csv(poy,paste(outt,"/Harmo_delayed_2100_harmonised_poy.csv",sep=""))
write.xlsx(poy,paste(outt,"/Harmo_delayed_2100_harmonised_poy.xlsx",sep=""))

# Add number of models per region
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

#check if each scenario category has only one scenario
check=poy[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

# !! Mean per scenario category per model - fix (doesn't work with factor) !!
#poy=poy[,mean(V1,na.rm=TRUE),by=c('Scenario','Region','Variable','Model')]

# Plot
S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year GHG emissions - harmonised to 2010 (land use)"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_harmo_only.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_harmo_only_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)

### Repeat for mean scenario per model and category

dth=dth[,mean(value,na.rm=TRUE),by=c('Scenario','Model','Region','Variable','Year')]
setnames(dth,"V1","value")

# Calculate phase-out year
poy=dth[!duplicated(dth[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dth[value<=0,min(Year),by=c('Model', 'Scenario','Region','Variable')],by=c('Model','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
#poy=na.omit(poy)

# Add number of models per region
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

# Plot
S = ggplot()
S = S + geom_point(data=poy, aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(.~Scenario, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year GHG emissions - harmonised to 2010 (land use)"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_harmo_only_1scenpermodel.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_450_harmo_only_exPOLES.png",sep=""),S,width=11, height=8, dpi=120)

  ### Combined figure ####

  # Phase-out year (no harmonisation) ---------------------------------------
dt=dat[Variable %in% c("Emissions|Kyoto Gases") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450", ,"Realistic 450"

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)

poy=dt[!duplicated(dt[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dt[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"

poy=poy[Region%in%c("Brazil","Canada","China","EU","India","Indonesia","Japan","Mexico","Russia","South Africa","South Korea","Turkey","USA","World")]
#poy=na.omit(poy)
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

#check if each scenario category has only one scenario
check=poy[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

# poy=poy[Region%in%c("USA [4 models]","Turkey [1 models]","South Korea [1 models]","South Africa [1 models]",
#                     "Russia [2 models]","Mexico [1 models]","Japan [2 models]","Indonesia [1 models]","India [3 models]",
#                     "EU [4 models]","China [4 models]","Canada [1 models]","Brazil [1 models]")]
poy0=poy
poy0$Variable<-paste(poy0$Variable,'|No harmo',sep="")
poy0$Unit<-NULL

  # Harmonisation - only land use -------------------------------------------
dt=dat[Variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|Land Use") & Scenario %in% c("Delayed 450","Delayed 450_2030")] #"Optimal 450",,"Realistic 450"

# To make sure we only use the models with data until 2100, important for this indicator
check=dt[,list(unique(Year)), by=c("Model")]
check=subset(check, subset=V1=="2100")
dt=subset(dt, subset=Model %in% check$Model)

# Read data for harmonisation
har=fread("harmonisation.csv",header=TRUE)
har1=har[Variable=="Emissions|CO2|Land Use"]

# Merge and calculate model deviation in 2010
dt1=dt[Year==2010 & Variable=="Emissions|CO2|Land Use"]
harmo=merge(dt1,har1, by=c("Region","Variable"))
setnames(harmo,"value.x","model")
setnames(harmo,"value.y","hist")
harmo=harmo %>% mutate(factor=model-hist)
harmo$model<-NULL
harmo$hist<-NULL 
harmo$Year<-NULL

# Harmonise - only land use
dth=merge(dt[Variable=="Emissions|CO2|Land Use"],harmo,by=c("Region","Variable","Scenario_original","Scenario","Model","Unit"))
dth=dth %>% mutate(harmonised=value-factor)
dth$factor<-NULL
dth$value<-NULL
setnames(dth,"harmonised","value")
dth$Variable<-"Emissions|CO2|Land Use|Harmo"
dth=data.table(dth)
dth=setcolorder(dth,c("Scenario_original","Scenario","Model","Region","Variable","Unit","Year","value"))

# Add harmonised land use tot GHG excl. land use
dtm=rbind(dt,dth)
dtm=spread(dtm[,!c('Unit'),with=FALSE],Variable, value)
dtm=na.omit(dtm)
dtm=dtm %>% mutate(`Emissions|Kyoto Gases|Harmo`=`Emissions|Kyoto Gases` - `Emissions|CO2|Land Use` + `Emissions|CO2|Land Use|Harmo`)
dtm=gather(dtm,Variable,value,c(`Emissions|Kyoto Gases|Harmo`,`Emissions|Kyoto Gases`, `Emissions|CO2|Land Use`,`Emissions|CO2|Land Use|Harmo`))
dtm=data.table(dtm)
dth=dtm[Variable=="Emissions|Kyoto Gases|Harmo"]
dth$Variable<-"Emissions|Kyoto Gases"

# Calculate phase-out year
poy=dth[!duplicated(dth[,list(Model,Scenario,Region,Variable),with=TRUE]),!c('value','Year'),with=FALSE]
poy=merge(poy,dth[value<=0,min(Year),by=c('Model','Scenario_original', 'Scenario','Region','Variable')],by=c('Model','Scenario_original','Scenario','Region','Variable'),all=TRUE)
poy$V1=as.factor(poy$V1)
poy[is.na(V1),]$V1="No phase out"
#poy=na.omit(poy)

# Add number of models per region
models=poy[,list(number=length(unique(Model))),by=c('Region','Variable')]
poy=merge(poy, models, by=c('Region','Variable'))
poy$Region <- paste(poy$Region,' [',poy$number,' models]',sep="")

#check if each scenario category has only one scenario
check=poy[,list(number=length(unique(Scenario_original))),by=c('Model','Scenario','Region')]

poy2=poy
poy2$Variable<-paste(poy2$Variable,'|Harmo|Land',sep="")

  # Plotting ----------------------------------------------------------------
poy=rbind(poy0,poy2) #poy1,poy3
poy=spread(poy, Variable, V1)
poy=setnames(poy,"Emissions|Kyoto Gases|No harmo","No harmonisation")
#poy=setnames(poy,"Emissions|Kyoto Gases|Harmo|Total","Total")
#poy=setnames(poy,"Emissions|Kyoto Gases|Harmo|Excl. land","Excl. land")
poy=setnames(poy,"Emissions|Kyoto Gases|Harmo|Land","Land")
poy=gather(poy,Variable,V1,c(`No harmonisation`,Land)) #Total,`Excl. land`,
poy=data.table(poy)
poy$Variable=factor(poy$Variable, levels=c("No harmonisation","Land")) #c("No harmonisation","Total","Land","Excl. land")
poy=poy[!number<2]


# Plot
S = ggplot()
S = S + geom_point(data=poy[Scenario=="Delayed 450"], aes(y=Region, x=V1, colour=Model, shape=Model), size=4)
S = S + scale_shape_manual(values=c("POLES 2014" = 1, "REMIND 1.5" = 2, "MESSAGE V.4" = 3,
                                    "DNE21+ V.12A" = 4, "WITCH2013" = 5, "IMAGE 2.4" = 6, 
                                    "GEM-E3_V1" = 7, "GEM-E3_IPTS_World" = 8, "DNE21+ V.12E" = 9,
                                    "GCAM4"=10, "GCAM_LAMP" =11, "POLES AMPERE" = 12, "POLES EMF27" = 13
                                    #,"DNE21+ V.MILES" = 14, "MESSAGE-Brazil v.1.3" = 15, "PRIMES_V1" = 16, "REMIND 1.6" = 17
))
S = S + facet_grid(Scenario~Variable, scales="free_y")
S = S + scale_x_discrete(limits=c("2005", "2010","2015", "2020","2025", "2030","2035" , "2040", "2045","2050","2055", "2060","2065","2070","2075","2080","2085","2090","2095" ,"2100","No phase out"),
                         breaks=c("2010", "2020", "2030", "2040", "2050","2060","2070","2080","2090","2100","No phase out"))
S = S + ggtitle(bquote("Phase-out year GHG emissions - harmonised to 2010 - Delayed 450"))
S = S + xlab("Phase out year")
S = S + theme_bw()
S = S + theme(axis.text.y=element_text(angle=45, size=16))
S = S + theme(strip.text.x=element_text(size=14))
S = S + theme(axis.title=element_text(size=18))
S = S + theme(axis.text.x = element_text(angle = 60, hjust = 1, size=14))
S = S + theme(plot.title=element_text(size=18))
ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_delay450_harmo.png",sep=""),S,width=11, height=8, dpi=120)
#ggsave(file=paste(out,"/Phase_out_year_all_Kyoto_delay450_harmo_exPOLES.png",sep=""),S,width=13, height=8, dpi=120)


# Effect of allocation of negative emissions ------------------------------

#	Allocation negative emissions: check CO2 BECCS, add to region it belongs to, then allocate based on biomass production  check effect on neutrality   
# Check effect of different allocations (either to biomass producer or electricity/CCS) on phase-out years. E.g. energy crop production, CCSbiomass carbon sequestration and trade-primary energy-biomass-volume?

# Mitigation strategies / why are some regions earlier or later ---------------------------------------------------

# EU late vs. US early: space for afforestation (e.g. population density).  Check database for other reasons , e.g. non-CO2 share in 2015? Mogelijk bevolkingsdichtheid of misschien productieve grond per persoon tegen jaar CO2 neutraliteit? Evt. ook iets van CCS capaciteit.
#	Graph: X indicator with effect on neutrality, e.g. afforestation  capacity; Y phase-out year 
# Graph: Emissions in phase-out year (like Joeri’s) 
# En dus bijvoorbeeld ook de strategie waarlangs een regio neutraliteit krijgt (meer uit reductie emissies, over meer uit negatieve emissies).

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

