# Load data ---------------------------------------------------------------
setwd("~/disks/local/factsheet/src")
config <-"config_neutrality"
scencateg <- "scen_categ_V4"
variables <- "variables_neutrality"
adjust <- "adjust_reporting_neutrality" # TODO to check: remove MESSAGE for China due to region definition? and COFFEE & DNE for EU and DNE for China and India? and COFFEE for Japan?
addvars <- F
datafile <-"cdlinks_compare_20191118-083136" # backup because file went missing: 20191119-134837
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

# Data preparation --------------------------------------------------------
np=data.table(all)
np=np[Scope=="global"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5REF","R5OECD90+EU")&!model=="COPPE-COFFEE 1.0"]

# write output: overview of original scenarios per category, model and region
u=np[,list(unique(scenario)),by=c("model","Category","region")]
write.csv(u,paste("Neutrality","/Scenario overview.csv",sep=""))

# save source data for paper
npsave = spread(np,period,value)
npsave=npsave[ ,`:=`("Baseline" = NULL, "Scope" = NULL)]
write.csv(npsave,paste("Neutrality","/Source data.csv",sep=""))

# SI table peak year etc --------------------------------------------------
# Negative emissions in 2100, also 2030/2050 reduction targets, peak year
SItable=np[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Carbon Sequestration|CCS","Carbon Sequestration|Land Use")]  #,"Carbon Sequestration|Direct Air Capture","Carbon Sequestration|Enhanced Weathering","Carbon Sequestration|Other"

#calculate negative emissions
SItable=spread(SItable[,!c('unit'),with=FALSE],variable,value)
SItable[is.na(SItable)] <- 0
SItable=SItable%>%mutate(Negative_emissions=`Carbon Sequestration|CCS`+`Carbon Sequestration|Land Use`)
SItable=data.table(gather(SItable,variable,value,c('Negative_emissions','Carbon Sequestration|CCS','Carbon Sequestration|Land Use','Emissions|Kyoto Gases','Emissions|CO2')))
NegEmis2100 = SItable[period==2100&variable=="Negative_emissions"&region%in%c("BRA","CAN","CHN","EU","IND","JPN","TUR","USA","World","IDN","RUS")&Category%in%c("1.5 °C","2 °C"),list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","region","variable","period","Scope")]
write.csv(NegEmis2100,paste("Neutrality","/SItableNegEmis.csv",sep=""))

#calculate peak year
peak = SItable[variable%in%c("Emissions|CO2"," Emissions|Kyoto Gases"),list(value=as.numeric(period[which.max(value)])),by=c('scenario','Category','Baseline','model','region','Scope','variable')]
peakrange=peak[region%in%c("BRA","CAN","CHN","EU","IND","JPN","TUR","USA","World","IDN","RUS")&Category%in%c("1.5 °C","2 °C"),list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","region","variable","Scope")]
write.csv(peakrange,paste("Neutrality","/SItablePeak.csv",sep=""))

#calculate reduction targets
mesg=spread(SItable[period%in%c(2010,2020)&model=="MESSAGEix-GLOBIOM_1.1"],period,value)
mesg = mesg%>%mutate(`2015`=(`2010`+`2020`)/2)
mesg = data.table(gather(mesg,period,value,c(`2010`,`2015`,`2020`)))
mesg = mesg[period==2015]
SItable = rbind(SItable,mesg)

emisrel = SItable[period%in%c(2010,2015,2030,2050)&variable=="Emissions|CO2"]
emisrel = spread(emisrel,period,value)
emisrel = emisrel%>%mutate(rel205015=(`2050`-`2015`)/`2015`*100,rel203015=(`2030`-`2015`)/`2015`*100, rel205010=(`2050`-`2010`)/`2010`*100,rel203010=(`2030`-`2010`)/`2010`*100)
emisrel = data.table(gather(emisrel,period,value,c(`2010`, `2015`,`2050`,`2030`,"rel205015","rel203015","rel205010","rel203010")))
emisrel = emisrel[period%in%c("rel205010","rel203010","rel205015","rel203015")]
emisrel$unit <-"%"
emisrel[period%in%c("rel205010","rel203010")]$variable <-"CO2 emissions relative to 2010"
emisrel[period%in%c("rel205015","rel203015")]$variable <-"CO2 emissions relative to 2015"
emisrel[period%in%c("rel205015","rel205010")]$period<-2050
emisrel[period%in%c("rel203015","rel203010")]$period<-2030
emisrelrange=emisrel[region%in%c("BRA","CAN","CHN","EU","IND","JPN","TUR","USA","World","IDN","RUS")&Category%in%c("1.5 °C","2 °C"),list(min=min(value,na.rm=T),max=max(value,na.rm=T),med=median(value,na.rm=T)),by=c("Category","region","variable","Scope","period")]
write.csv(emisrelrange,paste("Neutrality","/SItableRelEmisCO2.csv",sep=""))
