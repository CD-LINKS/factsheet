
# Data processing ---------------------------------------------------------
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")
setwd("~/disks/local/factsheet/src")

#source configuration file for region-specific data
source("settings/config_policybrief.R")
cfg$infile <- "cdlinks_compare_20171127-154822"
#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for overwriting a dataframe for a subset of variables
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if _proc.rdata file is available
# set to true if you always want data re-processed
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#input reference budgets for national scenarios:
bud <- read.csv2("data/ref_budgets.csv")
bud$high <- as.numeric(as.character(bud$high))
bud$low <- as.numeric(as.character(bud$low))
ref_budgets <- data.frame(region =c(rep("IND",2),rep("BRA",2), rep("JPN",2),rep("RUS",2) ,rep("CHN",2), rep("EU",2),rep("USA",2),rep("World",2)),scen=rep(c("high","low"),8),
                          value=c(bud[bud$country=="IND",]$high,bud[bud$country=="IND",]$low,
                                  bud[bud$country=="BRA",]$high,bud[bud$country=="BRA",]$low,
                                  bud[bud$country=="JPN",]$high,bud[bud$country=="JPN",]$low,
                                  bud[bud$country=="RUS",]$high,bud[bud$country=="RUS",]$low,
                                  bud[bud$country=="CHN",]$high,bud[bud$country=="CHN",]$low,
                                  bud[bud$country=="EUR",]$high,bud[bud$country=="EUR",]$low,
                                  bud[bud$country=="USA",]$high,bud[bud$country=="USA",]$low,
                                  bud[bud$country=="World",]$high,bud[bud$country=="World",]$low))

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,"_proc.Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,"_proc.Rdata"))
  Sys.sleep(2)#give everybody the chance to read the above message
} else {
  
  if (file.exists(paste0("data/",cfg$infile,".Rdata"))) {
    cat("Loading file", paste0("data/",cfg$infile,".Rdata"),"\n")
    load(paste0("data/",cfg$infile,".Rdata"))
  } else {
    cat("Reading data from file",paste0("data/",cfg$infile,".csv"),"\n")
    all <- invisible(fread(paste0("data/",cfg$infile,".csv"),header=TRUE))
    save("all",file = paste0("data/",cfg$infile,".Rdata"))
  }
  
  #reduce size of the data frame
  vars <- fread("settings/variables_xCut.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$regions]
  
  cat("Processing stocktaking data\n")
  
  # Add information for new column "Category"
  scens <- fread("settings/scen_categ_cdlinks_indc_bycountry_V3.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]
  
  #all <- all[!(MODEL=="GEM-E3_V1"&SCENARIO=="INDC")]
  
  # Change scenario names for some models to V4 to not mix up old global model results with new ones, while using latest version of other models (V3)
  #Special case for RU_TIMES: no V3/V4 at all
  all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO,'_V3',sep="")
  all=all[!c(MODEL=="GEM-E3"&SCENARIO%in%c("NPi_V3"))]
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi_V4")]$SCENARIO,"NPi_V4","NPi_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO,"INDCi_recGenTaxation_V4","INDCi_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO,"INDC2030i_1000_recGenTaxation_V4","INDC2030i_1000_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO,"NPi2020_1000_recGenTaxation_V4","NPi2020_1000_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO,"NPi2020_400_recGenTaxation_V4","NPi2020_400_V3")
  
  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  # model specific adjustments
  source("adjust_reporting_indc_Mark.R")
  
  #### add variables
  all <- add_variables(all,scens)
  
  #correct scope for added variables
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"
  
  #### manual changes after addition of variables
  
  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  #get rid of Historical duplicates
  #all <- all[Category!="Historical"]
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))
  
}# end if-else: load and process stocktaking data


# Change scenario names for paper -----------------------------------------
all$Category=str_replace_all(all$Category,"NoPOL","No policy")
all$Category=str_replace_all(all$Category,"INDC","NDC")
all$Category=str_replace_all(all$Category,"NPip","National policies planned")
all$Category=str_replace_all(all$Category,"NPi","National policies")
all$Category=str_replace_all(all$Category,"2020_low","2°C")
all$Category=str_replace_all(all$Category,"2020_verylow","1.5°C")
all$Category=str_replace_all(all$Category,"2030_low","2°C (2030)")

# And region names
#all$region=str_replace_all(all$region,"R5OECD90+EU","OECD90+EU")
oecd=all[region=="R5OECD90+EU"]
oecd$region<-"OECD90+EU"
all=all[!region=="R5OECD90+EU"]
all=rbind(all,oecd)
all$region=str_replace_all(all$region,"R5REF","Reforming")
all$region=str_replace_all(all$region,"R5MAF","ME+Africa")
all$region=str_replace_all(all$region,"R5LAM","Latin America")
all$region=str_replace_all(all$region,"R5ASIA","Asia")

# Figure 1 - time series -------------------------------------------------
source("functions/plot_functions.R")
# Figure 1. See poster figure 1a-gap.R
cats <- c("No policy","National policies","NDC","2°C","1.5°C")
reg="World"
dt=all
vars=c("Emissions|Kyoto Gases")
title=""
file_pre="1_GHG_funnel_all"
glob_lines=F
xlim=c(2010,2032)
ylim=c(20000,75000)
range=T
median=T
linetypemanual=F

dt <- dt[region==reg & Category%in% cats & variable%in% vars]
unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
unitsy <- paste(rev(unitsy),sep='',collapse='')
models=dt[,list(number=length(unique(model))),by=c('region','variable','Category')]
dt=merge(dt, models, by=c('region','variable','Category'))
minmax=dt[Scope=="global" ,list(ymax=max(value,na.rm=TRUE),ymin=min(value,na.rm=TRUE),med=median(value,na.rm=TRUE)),by=c('region','period','Category','variable')]
minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
minmax<-minmax[order(region, Category, period),]
minmax$period=as.numeric(minmax$period)
dt$period=as.numeric(dt$period)

minmax$ymax=minmax$ymax/1000
minmax$ymin=minmax$ymin/1000
minmax$med=minmax$med/1000
ylim=c(20,80)

p = ggplot()
p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                          color=Category),size=1.3)}
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario")
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario")
if (range & length(unique(dt$Category))==5){
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2030, xend=2030, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2030.5, xend=2030.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2031, xend=2031, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2031.5, xend=2031.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2032, xend=2032, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab("GtCO2-equiv/year") + xlab("")
p = p + ggplot2::theme_bw() 
p = p + theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18),
              strip.text=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              plot.title=element_text(size=18))

ggsave(file=paste0(cfg$outdir,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)


# Figure 2 - operational targets ------------------------------------------

#2a: 2030/2050 emission reductions in NPi1000 for world and major / R5 regions
emisred=all[Category=="2°C"&variable=="Emissions|Kyoto Gases"&Scope=="global"&period%in%c(2010,2030,2050)&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #&region%in%c("World","BRA","CAN","CHN","EU","IND","JPN","RUS","TUR","USA")
emisred=spread(emisred,period,value)
emisred=emisred%>%mutate(red2050=(`2050`-`2010`)/`2010`*100,red2030=(`2030`-`2010`)/`2010`*100)
emisred=data.table(gather(emisred,period,value,c(`2010`,`2030`,`2050`,red2030,red2050)))
emisred=emisred[period%in%c("red2030","red2050")]
emisred$unit<-"%"
emisred$period=str_replace_all(emisred$period,"red2030","2030")
emisred$period=str_replace_all(emisred$period,"red2050","2050")
emisredrange=data.table(emisred[,list(median=median(value),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])

F2=ggplot(emisredrange)
F2=F2+facet_grid(variable~period,scale="fixed")
F2=F2+geom_bar(aes(x=region,y=median),stat = "identity",fill="#a50000")
F2=F2+geom_errorbar(aes(x=region,ymin=min,ymax=max))
F2=F2+coord_flip()
F2=F2+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22))
F2=F2+ylab("")  + xlab("")
F2=F2+ggtitle("a) GHG emission reductions by 2030 and 2050")
ggsave(file=paste0(cfg$outdir,"/","F2a",".png"),F2, width=10, height=8, dpi=300)

#2b: peak years for NPi1000 for world and major / R5 regions
peak=all[Category=="2°C"&variable=="Emissions|Kyoto Gases"&Scope=="global"&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")]
check=peak[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
peak=peak[model%in%check$model]
peak=peak[,list(value=period[which.max(value)]),by=c("Category","model","region","unit","variable")]
peak=peak[!value%in%c("2005")]
peak$value=as.numeric(peak$value)
peakrange=data.table(peak[,list(median=median(value),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable")])

F2b=ggplot(peakrange)
F2b=F2b+geom_point(aes(x=region,y=median,colour=Category),size=5)
F2b=F2b+geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=unit))
F2b=F2b+coord_flip()
F2b=F2b+scale_colour_manual(name="Statistics",values=c("2°C"="#a50000","Mt CO2-equiv/yr"="black"),labels=c("2°C"="median","Mt CO2-equiv/yr"="10-90 percentile range"))
F2b=F2b+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=20))
F2b=F2b+theme(legend.position = c(0.2,0.1))
F2b=F2b+ylab("")  + xlab("")
F2b=F2b+ggtitle("b) GHG emission peak years")
ggsave(file=paste0(cfg$outdir,"/","F2b",".png"),F2b, width=10, height=8, dpi=300)

#2c: phase-out years for NPi1000 for world and major / R5 regions
poy=all[Category=="2°C"&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2")&Scope=="global"&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #!region=="Bunkers"
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
F2c=F2c+facet_grid(~variable,scale="fixed")
F2c=F2c+geom_point(aes(x=region,y=median,colour=Category),size=5) #,colour="#a50000"
F2c=F2c+geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=unit))
F2c=F2c+coord_flip()
F2c=F2c+scale_colour_manual(name="Statistics",values=c("2°C"="#a50000","Mt CO2-equiv/yr"="black"),labels=c("2°C"="median","Mt CO2-equiv/yr"="10-90 percentile range"))
F2c=F2c+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),legend.text=element_text(size=16),legend.title=element_text(size=18))
F2c=F2c+theme(legend.position = c(0.7,0.1))
F2c=F2c+ylab("")  + xlab("")
F2c=F2c+ggtitle("c) Phase-out year of CO2 and GHG emissions")
ggsave(file=paste0(cfg$outdir,"/","F2c",".png"),F2c, width=11, height=8, dpi=300)

#2d negative emissions/ remaining emissions
dt=all[Category%in%c("2°C","NDC")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Emissions|F-Gases","Emissions|N2O","Emissions|CH4")&Scope=="global"&region%in%c("World")]
sectors=dt[Category%in%c("2°C")&variable%in%c("Emissions|CO2","Emissions|F-Gases","Emissions|N2O","Emissions|CH4")]
sectors[variable=="Emissions|N2O"]$value<-sectors[variable=="Emissions|N2O"]$value*298/1000
sectors[variable=="Emissions|CH4"]$value<-sectors[variable=="Emissions|CH4"]$value*25
sectors$unit<-"Mt CO2-equiv/yr"
total=dt[Category%in%c("2°C","NDC")&variable%in%c("Emissions|Kyoto Gases")]
dt=rbind(sectors,total)
dt$value=dt$value/1000
dtrange=data.table(dt[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])
dtrange=dtrange[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]

setnames(dtrange,"variable","Emissions")
setnames(dtrange,"Category","Scenario")
dtrange$Emissions=str_replace_all(dtrange$Emissions,"Emissions|","")
# dtrange$Emissions=str_replace_all(dtrange$Emissions,"Emissions|N2O","N2O")
# dtrange$Emissions=str_replace_all(dtrange$Emissions,"Emissions|F-Gases","F-Gases")
# dtrange$Emissions=str_replace_all(dtrange$Emissions,"Emissions|CO2","CO2")
# dtrange$Emissions=str_replace_all(dtrange$Emissions,"Emissions|Kyoto Gases","Kyoto GHG")
         
F2d=ggplot(dtrange)
F2d=F2d+geom_line(aes(x=period,y=median,colour=Emissions,linetype=Scenario),size=2)
F2d=F2d+theme_bw()+theme(axis.text=element_text(size=20),plot.title = element_text(size=22),legend.text=element_text(size=16),legend.title=element_text(size=18),axis.title=element_text(size=20))
F2d=F2d+ggtitle("d) Individual GHG emissions") + ylab("GtCO2eq/year") + xlab("")
ggsave(file=paste0(cfg$outdir,"/","F2d",".png"),F2d, width=11, height=8, dpi=300)


# Figure 4 ----------------------------------------------------------------
#4a
cats <- c("No policy","National policies","NDC","2°C","1.5°C")
reg="World"
dt=all
vars=c("Emissions|Kyoto Gases")
title=""
file_pre="4a_GHG_funnel_all"
glob_lines=F
xlim=c(2010,2032)
ylim=c(20000,75000)
range=T
median=T
linetypemanual=F

dt <- dt[region==reg & Category%in% cats & variable%in% vars]
unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
unitsy <- paste(rev(unitsy),sep='',collapse='')
models=dt[,list(number=length(unique(model))),by=c('region','variable','Category')]
dt=merge(dt, models, by=c('region','variable','Category'))
minmax=dt[Scope=="global" ,list(ymax=max(value,na.rm=TRUE),ymin=min(value,na.rm=TRUE),med=median(value,na.rm=TRUE)),by=c('region','period','Category','variable')]
minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
minmax<-minmax[order(region, Category, period),]
minmax$period=as.numeric(minmax$period)
dt$period=as.numeric(dt$period)

minmax$ymax=minmax$ymax/1000
minmax$ymin=minmax$ymin/1000
minmax$med=minmax$med/1000
ylim=c(20,80)

p = ggplot()
p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                          color=Category),size=1.3)}
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario")
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario")
if (range & length(unique(dt$Category))==5){
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2030, xend=2030, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2030.5, xend=2030.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2031, xend=2031, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2031.5, xend=2031.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2032, xend=2032, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab("GtCO2-equiv/year") + xlab("") + ggtitle("a) Global greenhouse gas emissions until 2030")
p = p + ggplot2::theme_bw() 
p = p + theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18),
              strip.text=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              plot.title=element_text(size=18))

ggsave(file=paste0(cfg$outdir,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)

#4b
source("functions/plot_functions_xcut.R")
dt_all <- all
dt_all[model == "COPPE-COFFEE 1.0" & region == "EU", "value"] <- NA
dt_all[model == "DNE21+ V.14" & (region == "CHN" | region == "IND"), "value"] <- NA
dt_all[model == "MESSAGE" & region == "EU", "value"] <- NA

UseErrorBars = F
ylim_top = c(-1000,17500)
ylim_bottom=c(-250,2750)
breaks_top = c(5000, 10000, 15000)
breaks_bottom = c(500, 1000, 1500, 2000, 2500)
regs <- c("BRA")
cats <- c("Historical","No policy","National policies","NDC", "2°C", "1.5°C")
catsnat <- c("Historical","No policy","National policies","NDC")
g2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial CO2","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2g_BRA_2030_alt",lab = "Brazil GHG emissions (Mt CO2eq/yr)", ylim=ylim_bottom, ybreaks=breaks_bottom, hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                                   "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                                   "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F, error_bar=UseErrorBars, catsnat=catsnat,total=T)

regs <- c("CHN")
b2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="bc_CHN_2030_alt",lab = "China GHG emissions (Mt CO2eq/yr)", ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                           "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                           "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("IND")
e2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2e_IND_2030_alt",lab = "India GHG emissions (Mt CO2eq/yr)", ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                           "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                           "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("JPN")
h2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2h_JPN_2030_alt",lab = "Japan GHG emissions (Mt CO2eq/yr)", ylim=ylim_bottom, ybreaks=breaks_bottom,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                                 "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                                 "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("USA")
c2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2c_USA_2030_alt",lab = "USA GHG emissions (Mt CO2eq/yr)", ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                         "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                         "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("EU")
d2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2d_EU_2030_alt",lab = "EU GHG emissions (Mt CO2eq/yr)", ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings  CO2",
                                                                                                                                                                       "Emissions|CO2|AFOLU" ="AFOLU  CO2",
                                                                                                                                                                       "Emissions|Non-CO2" = "Non-CO2 emissions"),

                          TotalEmis_var = "Emissions|Kyoto Gases2", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)


regs <- c("RUS")
f2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2f_RUS_2030_alt",lab = "Russia GHG emissions (Mt CO2eq/yr)", ylim=ylim_bottom, ybreaks=breaks_bottom,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply  CO2",
                                                                                                                                                                                  "Emissions|CO2|Energy|Demand|Transportation"="Transport  CO2",
                                                                                                                                                                                  "Emissions|CO2|Energy|Demand|Industry"="Industry  CO2",
                                                                                                                                                                                  "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings  CO2",
                                                                                                                                                                                  "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                                  "Emissions|Non-CO2" = "Non-CO2 emissions"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

tmp<-ggplot_gtable(ggplot_build(b2_alt))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
b2_alt=b2_alt+theme(legend.position = "none")
c2_alt=c2_alt+theme(legend.position = "none")
d2_alt=d2_alt+theme(legend.position = "none")
e2_alt=e2_alt+theme(legend.position = "none")
f2_alt=f2_alt+theme(legend.position = "none")
g2_alt=g2_alt+theme(legend.position = "none")
h2_alt=h2_alt+theme(legend.position = "none")
lay<-rbind(c(1,2,3,4),c(5,6,7,8))
h=grid.arrange(b2_alt,c2_alt,d2_alt,e2_alt,f2_alt,g2_alt,h2_alt,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/Fig4b_arrange_alt.png",sep=""),h,width=20,height=16,dpi=200)

#4d
regs <- c("BRA","CHN", "IND", "RUS", "EU","JPN","USA",  "World")
catsglob <- c("NDC","2°C")

plot_stackbar_diff(regs=regs,dt=all,vars=c("Renewables Share|Excl. Nuclear"),cats = catsglob, ylim=c(0,100),ybreaks=c(0,10,20,30,40,50,60,70,80,90,100),
                  lab="%",per=c(2030,2050),file_pre="2030_2050_ElecREN_excl_nuc",labels=T,scen.labels = c("2°C","NDC"),b.multiyear = T,title=T,
                  Title="c) Increase in renewable energy share",out=cfg$outdir)


# Figure 5 ----------------------------------------------------------------
# Figure 5a ---------------------------------------------------------------
#Figure 5a
cats <- c("2°C","2°C (2030)")
reg="World"
dt=all
vars=c("Emissions|Kyoto Gases")
title=""
file_pre="1_GHG_funnel"
glob_lines=F
xlim=c(2010,2102)
ylim=c(20000,75000)
range=T
median=T
linetypemanual=F

dt <- dt[region==reg & Category%in% cats & variable%in% vars]
unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
unitsy <- paste(rev(unitsy),sep='',collapse='')
models=dt[,list(number=length(unique(model))),by=c('region','variable','Category')]
dt=merge(dt, models, by=c('region','variable','Category'))
minmax=dt[Scope=="global" ,list(ymax=max(value,na.rm=TRUE),ymin=min(value,na.rm=TRUE),med=median(value,na.rm=TRUE)),by=c('region','period','Category','variable')]
minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
minmax<-minmax[order(region, Category, period),]
minmax$period=as.numeric(minmax$period)
dt$period=as.numeric(dt$period)

minmax$ymax=minmax$ymax/1000
minmax$ymin=minmax$ymin/1000
minmax$med=minmax$med/1000
ylim=c(-20,80)

p = ggplot()
p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                          color=Category),size=1.3)}
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario (2°C)",labels=c("2°C"="Start 2020","2°C (2030)"="Start 2030"))
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario (2°C)",labels=c("2°C"="Start 2020","2°C (2030)"="Start 2030"))
if (range & length(unique(dt$Category))==5){
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2100, xend=2100, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2100.5, xend=2100.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2101, xend=2101, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2101.5, xend=2101.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2102, xend=2102, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab("GtCO2-equiv/year") + xlab("") + ggtitle("a) Global greenhouse gas emissions until 2030")
p = p + ggplot2::theme_bw() 
p = p + theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18),
              strip.text=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              plot.title=element_text(size=18))

ggsave(file=paste0(cfg$outdir,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)



# Figure 5b ---------------------------------------------------------------
#Rates of change
rate=all[Category%in%c("2°C","2°C (2030)","1.5°C")&variable%in%c("Rate of Change| GHG Intensity of GDP|MER","Rate of Change| GHG Intensity of FE","Rate of Change| Energy Intensity of GDP|MER","Rate of Change| Emissions|CO2|FFI")&Scope=="global"&period%in%c("2030-2050")&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #&region%in%c("World","BRA","CAN","CHN","EU","IND","JPN","RUS","TUR","USA") #Renewables Share|Excl. Nuclear
raterange=data.table(rate[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])
raterange$variable=str_replace_all(raterange$variable,"Rate of Change","")

F5=ggplot(raterange[region=="World"])
F5=F5+facet_wrap(~variable,scale="fixed")
F5=F5+geom_bar(aes(x=Category,y=median,fill=Category),stat = "identity",show.legend = F)
F5=F5+geom_errorbar(aes(x=Category,ymin=min,ymax=max))
F5=F5+theme_bw()+theme(strip.text=element_text(size=18),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),
                       legend.text=element_text(size=20),legend.title=element_text(size=20))
F5=F5+ylab("")  + xlab("")
F5=F5+ggtitle("b) Global rate of change in 2030-2050 period (%/year)")
ggsave(file=paste0(cfg$outdir,"/","F5b",".png"),F5, width=10, height=8, dpi=300)

#renewables deployment
Ren=all[Category%in%c("2°C","2°C (2030)","1.5°C")&variable%in%c("Renewables Share|TPES|Excl. Nuclear")&Scope=="global"&period%in%c(2030,2050)&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] 
Renrange=data.table(Ren[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])

F5c=ggplot(Renrange)
F5c=F5c+facet_wrap(~region,scale="fixed")
F5c=F5c+geom_bar(aes(x=Category,y=median,fill=period),stat = "identity",position=position_dodge(width=0.66),width=0.66)
F5c=F5c+geom_errorbar(aes(x=Category,ymin=min,ymax=max,colour=period),position=position_dodge(width=0.66),width=0.66)
F5c=F5c+theme_bw()+theme(strip.text=element_text(size=18),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),
                       legend.text=element_text(size=20),legend.title=element_text(size=20))
F5c=F5c+ylab("")  + xlab("")
F5c=F5c+ggtitle("c) Renewable energy share in total primary energy use (%)")
ggsave(file=paste0(cfg$outdir,"/","F5c",".png"),F5c, width=10, height=8, dpi=300)


# Figure 10 ---------------------------------------------------------------
sector=all[Category%in%c("2°C","1.5°C")&variable%in%c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Industrial Processes","Emissions|CO2|AFOLU")&Scope=="global"&region%in%c("World")]
sectorrange=data.table(sector[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])
sectorrange$period=as.numeric(sectorrange$period)

F10=ggplot(sectorrange)
F10=F10+facet_wrap(~Category,scale="fixed")
F10=F10+geom_line(aes(x=period,y=median,colour=variable))
F10=F10+geom_ribbon(aes(x=period,ymin=min,ymax=max,fill=variable))
F10=F10+theme_bw()+theme(strip.text=element_text(size=18),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),
                       legend.text=element_text(size=20),legend.title=element_text(size=20))
F10=F10+ylab("")  + xlab("")
F10=F10+ggtitle("Sectoral CO2 emissions (MtCO2/year)")
ggsave(file=paste0(cfg$outdir,"/","F10",".png"),F10, width=10, height=8, dpi=300)


