
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
b.procdata = F

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
all$Category=str_replace_all(all$Category,"2020_low","2 °C")
all$Category=str_replace_all(all$Category,"2020_verylow","1.5 °C")
all$Category=str_replace_all(all$Category,"2030_low","2 °C (2030)")

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
source("functions/plotstyle.R") 
# Figure 1. See poster figure 1a-gap.R
cats <- c("National policies","NDC","2 °C","1.5 °C")
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

#minmax$Category=str_replace_all(minmax$Category,"National policies","Current (national) policies")
minmax$Category =  factor(minmax$Category, levels = c("National policies","NDC","2 °C","1.5 °C"), ordered = T)

p = ggplot()
p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                          color=Category),size=1.3)}
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario")
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario")
if (range & length(unique(dt$Category))==4){
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2030, xend=2030, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2030.5, xend=2030.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2031, xend=2031, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2031.5, xend=2031.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  #p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2032, xend=2032, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab(bquote(paste("Gt",CO[2],"e/yr"))) + xlab("")
p = p + ggplot2::theme_bw() 
p = p + theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18),
              strip.text=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              plot.title=element_text(size=18))
#p=p+guides(colour = guide_legend(reverse=T),fill=guide_legend(reverse=T))
ggsave(file=paste0(cfg$outdir,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)


# Figure 2 - operational targets ------------------------------------------

#2a: 2030/2050 emission reductions in NPi1000 for world and major / R5 regions
emisred=all[Category=="2 °C"&variable=="Emissions|Kyoto Gases"&Scope=="global"&period%in%c(2010,2030,2050)&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #&region%in%c("World","BRA","CAN","CHN","EU","IND","JPN","RUS","TUR","USA")
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
peak=all[Category=="2 °C"&variable=="Emissions|Kyoto Gases"&Scope=="global"&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")]
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
F2b=F2b+scale_colour_manual(name="Statistics",values=c("2 °C"="#a50000","Mt CO2-equiv/yr"="black"),labels=c("2 °C"="median","Mt CO2-equiv/yr"="10-90 percentile range"))
F2b=F2b+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),legend.text=element_text(size=18),legend.title=element_text(size=20))
F2b=F2b+theme(legend.position = c(0.2,0.1))
F2b=F2b+ylab("")  + xlab("")
F2b=F2b+ggtitle("b) GHG emission peak years")
ggsave(file=paste0(cfg$outdir,"/","F2b",".png"),F2b, width=10, height=8, dpi=300)

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

#2d negative emissions/ remaining emissions
dt=all[Category%in%c("2 °C")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Emissions|F-Gases","Emissions|N2O","Emissions|CH4")&Scope=="global"&region%in%c("World")]
sectors=dt[Category%in%c("2 °C")&variable%in%c("Emissions|CO2","Emissions|F-Gases","Emissions|N2O","Emissions|CH4")]
sectors[variable=="Emissions|N2O"]$value<-sectors[variable=="Emissions|N2O"]$value*298/1000
sectors[variable=="Emissions|CH4"]$value<-sectors[variable=="Emissions|CH4"]$value*25
sectors$unit<-"Mt CO2-equiv/yr"
total=dt[Category%in%c("2 °C")&variable%in%c("Emissions|Kyoto Gases")]
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
F2d=F2d+geom_line(aes(x=period,y=median,colour=Emissions,group=Emissions),size=2) #,linetype=Scenario
F2d=F2d+theme_bw()+theme(axis.text=element_text(size=20),plot.title = element_text(size=22),legend.text=element_text(size=16),legend.title=element_text(size=18),axis.title=element_text(size=20))
F2d=F2d+ggtitle("d) Individual GHG emissions") + ylab(bquote(paste("Gt",CO[2],"e/yr"))) + xlab("")
ggsave(file=paste0(cfg$outdir,"/","F2d",".png"),F2d, width=11, height=8, dpi=300)

F2all = arrangeGrob(F2,F2b,F2c,F2d,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig2_arrange.png",sep=""),F2all,width=22,height=18,dpi=200)
  
# Figure 4 ----------------------------------------------------------------
#4a
cats <- c("No policy","National policies","NDC","2 °C","1.5 °C")
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
p = p + ylab(bquote(paste("Gt",CO[2],"e/yr"))) + xlab("") + ggtitle("a) Global greenhouse gas emissions until 2030")
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
cats <- c("Historical","No policy","National policies","NDC", "2 °C", "1.5 °C")
catsnat <- c("Historical","No policy","National policies","NDC")
g2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial CO2","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2g_BRA_2030_alt",lab = bquote(paste("Brazil GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_bottom, ybreaks=breaks_bottom, hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                                   "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                                   "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                                   "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F, error_bar=UseErrorBars, catsnat=catsnat,total=T)

regs <- c("CHN")
b2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="bc_CHN_2030_alt",lab = bquote(paste("China GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                           "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                           "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("IND")
e2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2e_IND_2030_alt", lab = bquote(paste("India GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                           "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                           "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                           "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("JPN")
h2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2h_JPN_2030_alt", lab = bquote(paste("Japan GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_bottom, ybreaks=breaks_bottom,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                                 "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                                 "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                                 "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("USA")
c2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2c_USA_2030_alt",lab = bquote(paste("USA GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                         "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings CO2",
                                                                                                                                                                         "Emissions|CO2|AFOLU" ="AFOLU CO2",
                                                                                                                                                                         "Emissions|Non-CO2" = "Non-CO2"),
                          TotalEmis_var = "Emissions|Kyoto Gases", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)

regs <- c("EU")
d2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2d_EU_2030_alt", lab = bquote(paste("EU GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_top, ybreaks=breaks_top,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Transportation"="Transport CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Industry"="Industry CO2",
                                                                                                                                                                       "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings  CO2",
                                                                                                                                                                       "Emissions|CO2|AFOLU" ="AFOLU  CO2",
                                                                                                                                                                       "Emissions|Non-CO2" = "Non-CO2 emissions"),

                          TotalEmis_var = "Emissions|Kyoto Gases2", natpoints=F,error_bar=UseErrorBars,catsnat=catsnat,total=T)


regs <- c("RUS")
f2_alt<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|AFOLU", "Emissions|Non-CO2"),cats = cats,
                          per=c(2030),file_pre="2f_RUS_2030_alt",lab = bquote(paste("Russia GHG emissions (Mt",CO[2],"eq/yr)")), ylim=ylim_bottom, ybreaks=breaks_bottom,hist=T,labels=T,var.labels=c("Emissions|CO2|Energy|Supply"="Energy Supply  CO2",
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
catsglob <- c("NDC","2 °C")

F4b<-plot_stackbar_diff(regs=regs,dt=all,vars=c("Renewables Share|Excl. Nuclear"),cats = catsglob, ylim=c(0,100),ybreaks=c(0,10,20,30,40,50,60,70,80,90,100),
                  lab="%",per=c(2030,2050),file_pre="2030_2050_ElecREN_excl_nuc",labels=T,scen.labels = c("2 °C","NDC"),b.multiyear = T,title=T,
                  Title="b) Increase in renewable energy share",out=cfg$outdir)

F4all = arrangeGrob(h,F4b,ncol=1)
ggsave(file=paste(cfg$outdir,"/Fig4_arrange.png",sep=""),F4all,width=18,height=24,dpi=200)

# Figure 5 ----------------------------------------------------------------
# Figure 5a ---------------------------------------------------------------
#Figure 5a
cats <- c("2 °C","2 °C (2030)")
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
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario (2 °C)",labels=c("2 °C"="Start 2020","2 °C (2030)"="Start 2030"))
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario (2 °C)",labels=c("2 °C"="Start 2020","2 °C (2030)"="Start 2030"))
if (range & length(unique(dt$Category))==5){
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2100, xend=2100, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2100.5, xend=2100.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2101, xend=2101, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2101.5, xend=2101.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2100) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2102, xend=2102, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab(bquote(paste("Gt",CO[2],"e/yr"))) + xlab("") + ggtitle("a) Global greenhouse gas emissions until 2030")
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
rate=all[Category%in%c("2 °C","2 °C (2030)","1.5 °C")&variable%in%c("Rate of Change| GHG Intensity of GDP|MER","Rate of Change| GHG Intensity of FE","Rate of Change| Energy Intensity of GDP|MER","Rate of Change| Emissions|CO2|FFI")&Scope=="global"&period%in%c("2030-2050")&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")] #&region%in%c("World","BRA","CAN","CHN","EU","IND","JPN","RUS","TUR","USA") #Renewables Share|Excl. Nuclear
raterange=data.table(rate[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])
raterange$variable=str_replace_all(raterange$variable,"Rate of Change","")

F5=ggplot(raterange[region=="World"])
F5=F5+facet_wrap(~variable,scale="fixed")
F5=F5+geom_bar(aes(x=Category,y=median,fill=Category),stat = "identity",show.legend = F)
F5 = F5 + scale_fill_manual(values=plotstyle(c("2 °C","2 °C (2030)","1.5 °C")))
F5=F5+geom_errorbar(aes(x=Category,ymin=min,ymax=max))
F5=F5+theme_bw()+theme(strip.text=element_text(size=18),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),
                       legend.text=element_text(size=20),legend.title=element_text(size=20))
F5=F5+ylab("")  + xlab("")
F5=F5+ggtitle("b) Global rate of change in 2030-2050 period (%/year)")
ggsave(file=paste0(cfg$outdir,"/","F5b",".png"),F5, width=10, height=8, dpi=300)

#renewables deployment
Ren=all[Category%in%c("2 °C","2 °C (2030)","1.5 °C")&variable%in%c("Renewables Share|TPES|Excl. Nuclear")&Scope=="global"&period%in%c(2030,2050)&region%in%c("World","Reforming","OECD90+EU","ME+Africa","Latin America","Asia")&!model=="DNE21+ V.14"] 
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

# F5all = arrangeGrob(p,F5,F5c,ncol=2)
# ggsave(file=paste(cfg$outdir,"/Fig5_arrange.png",sep=""),F5all,width=22,height=16,dpi=200)

# Figure 10 ---------------------------------------------------------------
sector=all[Category%in%c("2 °C","1.5 °C")&variable%in%c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Industrial Processes","Emissions|CO2|AFOLU")&Scope=="global"&region%in%c("World")]
sectorrange=data.table(sector[,list(median=median(value,na.rm=T),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])
sectorrange$period=as.numeric(sectorrange$period)
sectorrange=sectorrange[period%in%c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100)]
sectorrange1=sectorrange[variable=="Emissions|CO2|AFOLU"]
sectorrange1$variable<-"AFOLU"
sectorrange2=sectorrange[variable=="Emissions|CO2|Energy|Demand|Industry"]
sectorrange2$variable<-"Industry"
sectorrange3=sectorrange[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
sectorrange3$variable<-"Buildings"
sectorrange4=sectorrange[variable=="Emissions|CO2|Energy|Demand|Transportation"]
sectorrange4$variable<-"Transportation"
sectorrange5=sectorrange[variable=="Emissions|CO2|Energy|Supply"]
sectorrange5$variable<-"Energy supply"
sectorrange6=sectorrange[variable=="Emissions|CO2|Industrial Processes"]
sectorrange6$variable<-"Industrial processes"
sectorrange=rbind(sectorrange1,sectorrange2,sectorrange3,sectorrange4,sectorrange5,sectorrange6)
setnames(sectorrange,"variable","sector")

F10=ggplot(sectorrange)
F10=F10+facet_wrap(~Category,scale="fixed")
F10=F10+geom_line(aes(x=period,y=median,colour=sector))
F10=F10+geom_ribbon(aes(x=period,ymin=min,ymax=max,fill=sector),alpha=0.1)
F10=F10+theme_bw()+theme(strip.text=element_text(size=18),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),
                       legend.text=element_text(size=18),legend.title=element_text(size=18))+theme(legend.position = c(0.65,0.2))
F10=F10+ylab("")  + xlab("")
F10=F10+ggtitle(bquote(paste("b) Sectoral ", CO[2], " emissions (Mt",CO[2],"/year)")))
ggsave(file=paste0(cfg$outdir,"/","F10",".png"),F10, width=12, height=8, dpi=300)

# phase-out year per sector
poys=sector
check=poys[,list(unique(period)),by=c("model")]
check=check[V1=="2100"]
poys=poys[model%in%check$model]
poys2=poys[!duplicated(poys[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE]
poys=merge(poys2,poys[value<=0,min(period),by=c('model','Category','region','variable')],by=c('model','Category','region','variable'),all=TRUE)
poys[is.na(V1),]$V1=2100
setnames(poys,"V1","value")
poys$value=as.numeric(poys$value)
poysrange=data.table(poys[,list(median=median(value),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable")])
poysrange$unit<-"Mt CO2-equiv/yr"

poysrange1=poysrange[variable=="Emissions|CO2|AFOLU"]
poysrange1$variable<-"AFOLU"
poysrange2=poysrange[variable=="Emissions|CO2|Energy|Demand|Industry"]
poysrange2$variable<-"Industry"
poysrange3=poysrange[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
poysrange3$variable<-"Buildings"
poysrange4=poysrange[variable=="Emissions|CO2|Energy|Demand|Transportation"]
poysrange4$variable<-"Transportation"
poysrange5=poysrange[variable=="Emissions|CO2|Energy|Supply"]
poysrange5$variable<-"Energy supply"
poysrange6=poysrange[variable=="Emissions|CO2|Industrial Processes"]
poysrange6$variable<-"Industrial processes"
poysrange=rbind(poysrange1,poysrange2,poysrange3,poysrange4,poysrange5,poysrange6)
setnames(poysrange,"variable","sector")

F10b=ggplot(poysrange)
F10b=F10b+facet_grid(~Category,scale="fixed")
F10b=F10b+geom_point(aes(x=sector,y=median,colour=region),size=5) #,colour="#a50000"
F10b=F10b+geom_errorbar(aes(x=sector,ymin=min,ymax=max,colour=unit))
F10b=F10b+coord_flip()
F10b=F10b+scale_colour_manual(name="Statistics",values=c("World"="#a50000","Mt CO2-equiv/yr"="black"),labels=c("World"="median","Mt CO2-equiv/yr"="10-90 percentile range"))
F10b=F10b+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),axis.text.x=element_text(angle=45),plot.title = element_text(size=22),legend.text=element_text(size=16),legend.title=element_text(size=18))
F10b=F10b+theme(legend.position = c(0.7,0.4))
F10b=F10b+ylab("")  + xlab("")
F10b=F10b+ggtitle(bquote(paste("c) Phase-out year of sectoral ",CO[2], " emissions")))
ggsave(file=paste0(cfg$outdir,"/","F10b",".png"),F10b, width=11, height=8, dpi=300)

# 2050 reductions per sector
emisreds=sector
emisreds=spread(emisreds,period,value)
emisreds=emisreds%>%mutate(red2050=(`2050`-`2010`)/`2010`*100,red2030=(`2030`-`2010`)/`2010`*100)
emisreds=data.table(gather(emisreds,period,value,c(`2010`,`2030`,`2050`,red2030,red2050)))
emisreds=emisreds[period%in%c("red2030","red2050")]
emisreds$unit<-"%"
emisreds$period=str_replace_all(emisreds$period,"red2030","2030")
emisreds$period=str_replace_all(emisreds$period,"red2050","2050")
emisredsrange=data.table(emisreds[,list(median=median(value),min=quantile(value,prob=0.1,na.rm = T),max=quantile(value,prob=0.9,na.rm = T)),by=c("Category","region","unit","variable","period")])

emisredsrange1=emisredsrange[variable=="Emissions|CO2|AFOLU"]
emisredsrange1$variable<-"AFOLU"
emisredsrange2=emisredsrange[variable=="Emissions|CO2|Energy|Demand|Industry"]
emisredsrange2$variable<-"Industry"
emisredsrange3=emisredsrange[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
emisredsrange3$variable<-"Buildings"
emisredsrange4=emisredsrange[variable=="Emissions|CO2|Energy|Demand|Transportation"]
emisredsrange4$variable<-"Transportation"
emisredsrange5=emisredsrange[variable=="Emissions|CO2|Energy|Supply"]
emisredsrange5$variable<-"Energy supply"
emisredsrange6=emisredsrange[variable=="Emissions|CO2|Industrial Processes"]
emisredsrange6$variable<-"Industrial processes"
emisredsrange=rbind(emisredsrange1,emisredsrange2,emisredsrange3,emisredsrange4,emisredsrange5,emisredsrange6)
setnames(emisredsrange,"variable","sector")
emisredsrange[sector=="AFOLU"&period==2050]$min=-199

F10c=ggplot(emisredsrange)
F10c=F10c+facet_grid(Category~period,scale="fixed")
F10c=F10c+geom_bar(aes(x=sector,y=median),stat = "identity",fill="#a50000")
F10c=F10c+geom_errorbar(aes(x=sector,ymin=min,ymax=max))
F10c=F10c+coord_flip()
F10c=F10c+theme_bw()+theme(strip.text=element_text(size=20),axis.text=element_text(size=20),plot.title = element_text(size=20)) #axis.text.x=element_text(angle=45),
F10c=F10c+ylab("")  + xlab("")
F10c=F10c+ylim(-200,50)
F10c=F10c+ggtitle(bquote(paste("c) Sectoral " ,CO[2], " emissions by 2030 and 2050, relative to 2010 (%)")))
F10c=F10c+geom_hline(yintercept=-100,linetype="dashed")
ggsave(file=paste0(cfg$outdir,"/","F10c",".png"),F10c, width=12, height=8, dpi=300)

F10all = arrangeGrob(F10,F10c,ncol=1)
ggsave(file=paste(cfg$outdir,"/Fig10_arrange.png",sep=""),F10all,width=18,height=18,dpi=200)

# Figure investment changes ------------------------------------------------------
invest <- read.csv2("data/Data_investment_to_2050.csv",sep=",")

#change scenario names
invest$Scenario=str_replace_all(invest$Scenario,"CPol","National policies")
invest$Scenario=str_replace_all(invest$Scenario,"2c-NPi","2 °C")
invest$Scenario=str_replace_all(invest$Scenario,"1.5c-NPi","1.5 °C")
invest$Scenario=str_replace_all(invest$Scenario,"1.5c-2c","1.5 °C - 2 °C")
#invest$Scenario=str_replace_all(invest$Scenario,"2C","2 °C")
#invest$Scenario=str_replace_all(invest$Scenario,"1.5C","1.5 °C")

#numbers
invest$mean<-as.numeric(as.character(invest$mean))
invest$min<-as.numeric(as.character(invest$min))
invest$max<-as.numeric(as.character(invest$max))

invest=select(invest,c(Scenario,Variable,mean,max,min))

# insert enter in variable names
invest=data.table(invest)
invest[Variable=="Fossil Fuels Extraction and Conversion"]$Variable<-paste("Fossil Fuels Extraction\nand Conversion")
invest[Variable=="Fossil Electricity and Hydrogen w/o CCS"]$Variable<-paste("Fossil Electricity and\nHydrogen w/o CCS")
invest[Variable=="Electricity T&D and Storage"]$Variable<-paste("Electricity T&D\nand Storage")

#order of variables
invest$Variable=  factor(invest$Variable, levels = c("Energy Efficiency","Renewables","Nuclear and CCS","Electricity T&D\nand Storage","Fossil Fuels Extraction\nand Conversion","Fossil Electricity and\nHydrogen w/o CCS","Incr-Investment","Disinvestment"), ordered = T)

# get the right numbers for the error bar for the 'difference' scenario
invest[Scenario=="1.5 °C - 2 °C"]$max=invest[Scenario=="1.5 °C"]$max
invest[Scenario=="1.5 °C - 2 °C"]$min=invest[Scenario=="1.5 °C"]$min
invest1=invest
invest1[Scenario=="1.5 °C - 2 °C"]$mean=invest1[Scenario=="1.5 °C"]$mean

#selection for plotting
invest=invest[Scenario%in%c("1.5 °C - 2 °C","2 °C")&Variable%in%c("Energy Efficiency","Renewables","Nuclear and CCS","Electricity T&D\nand Storage","Fossil Fuels Extraction\nand Conversion","Fossil Electricity and\nHydrogen w/o CCS")]
invest1=invest1[Scenario%in%c("1.5 °C - 2 °C","2 °C")&Variable%in%c("Energy Efficiency","Renewables","Nuclear and CCS","Electricity T&D\nand Storage","Fossil Fuels Extraction\nand Conversion","Fossil Electricity and\nHydrogen w/o CCS")]

# plot
# library(patternplot)
# pattern.color<-c("white","black")
# pattern.type<-c("blank","hatch")

F32=ggplot(invest)
F32=F32+geom_bar(data=invest,aes(x=Variable,y=mean,fill=Scenario),stat = "identity",position="stack",show.legend = T) #,group=interaction(Variable,Scenario) ,position="stack"
#F32=F32+patternbar(data=invest[Scenario=="1.5 °C - 2 °C"],x=Variable,y=mean,pattern.type=pattern.type, pattern.color = pattern.color) #,stat = "identity",show.legend = T
F32=F32+scale_fill_manual(values=c("2 °C"="#56B4E9","1.5 °C - 2 °C"="#008000"),labels=c("2 °C"="Baseline to 2 °C (model mean)","1.5 °C - 2 °C"="2 °C to 1.5 °C (model mean)"),name="Change in investment:")
F32=F32+geom_errorbar(aes(x=Variable,ymin=min,ymax=max,colour=Scenario),position=position_dodge(width=0.66),width=0.66) 
F32=F32+geom_point(data=invest1,aes(x=Variable,y=mean,colour=Scenario),position=position_dodge(width=0.66)) 
F32=F32+scale_colour_manual(values=c("2 °C"="dark blue","1.5 °C - 2 °C"="#7aeb7a"),labels=c("2 °C"="2 °C (model ranges)","1.5 °C - 2 °C"="1.5 °C (model ranges)"),name="Statistics")
F32=F32+theme_bw()+theme(strip.text=element_text(size=0),axis.text.y=element_text(size=22),axis.text.x=element_text(size=19),plot.title = element_text(size=26), #,axis.text.x=element_text(angle=45)
                         legend.text=element_text(size=22),legend.title=element_text(size=22),axis.title = element_text(size=22))
F32=F32+theme(legend.position = c(0.2,0.2))
F32=F32+ylab("Investment (Billion US$2015/year)")  + xlab("Mitigation Investment & Disinvestment (relative to the baseline, 2016-2050)")
F32=F32+ggtitle("Investments and disinvestments")
F32
ggsave(file=paste0(cfg$outdir,"/","F32",".png"),F32, width=17, height=12, dpi=300)


# Figure SDG investments --------------------------------------------------
investsdg <- read.csv2("data/SDG_investments_figure.csv",sep=",")

#change labels
investsdg$Scenario=str_replace_all(investsdg$Scenario,"2C-CPol","2 °C")
investsdg$Scenario=str_replace_all(investsdg$Scenario,"1.5C-CPol","1.5 °C")
investsdg$Scenario=str_replace_all(investsdg$Scenario,"2C","2 °C")
investsdg$Scenario=str_replace_all(investsdg$Scenario,"1.5C","1.5 °C")
investsdg$SDG=str_replace_all(investsdg$SDG,"Incr-Investment","Energy Incremental Investment")
investsdg$SDG=str_replace_all(investsdg$SDG,"Disinvestment","Energy Disinvestment")

#numbers
investsdg$Bar.value<-as.numeric(as.character(investsdg$Bar.value))
investsdg$Min<-as.numeric(as.character(investsdg$Min))
investsdg$Max<-as.numeric(as.character(investsdg$Max))

# insert enter in variable names
investsdg=data.table(investsdg)
investsdg[SDG=="Water availability"]$SDG<-paste("Water\navailability")
investsdg[SDG=="Food security"]$SDG<-paste("Food\nsecurity")
investsdg[SDG=="Energy Incremental Investment"]$SDG<-paste("Energy\nIncremental\nInvestment")
investsdg[SDG=="Energy Disinvestment"]$SDG<-paste("Energy\nDisinvestment")

#order of variables
investsdg$SDG=  factor(investsdg$SDG, levels = c("Energy\nIncremental\nInvestment","Energy\nDisinvestment","Energy","Access","Food\nsecurity","Water\navailability","Air pollution"), ordered = T)
investsdg$Scenario=  factor(investsdg$Scenario, levels = c("2 °C","1.5 °C"), ordered = T)

# plot
F37=ggplot(investsdg)
F37=F37+facet_wrap(~panel,scale="free")
F37=F37+geom_bar(aes(x=SDG,y=Bar.value,fill=Scenario),stat = "identity",position=position_dodge(width=0.66),width=0.66,show.legend = T)
F37=F37+scale_fill_manual(values=c("2 °C"="#56B4E9","1.5 °C"="#008000"))
F37=F37+geom_errorbar(aes(x=SDG,ymin=Min,ymax=Max,colour=Scenario),position=position_dodge(width=0.66),width=0.66)
F37=F37+scale_colour_manual(values=c("2 °C"="black","1.5 °C"="black"))
F37=F37+theme_bw()+theme(strip.text=element_text(size=0),axis.text.y=element_text(size=22),axis.text.x=element_text(size=19),plot.title = element_text(size=26), #,axis.text.x=element_text(angle=45)
                       legend.text=element_text(size=22),legend.title=element_text(size=22),axis.title.y = element_text(size=22))
F37=F37+theme(legend.position = c(0.4,0.8))
F37=F37+ylab("Billion US$2015/year")  + xlab("")
F37=F37+ggtitle("Energy investments in relation to SDGs")
ggsave(file=paste0(cfg$outdir,"/","F37",".png"),F37, width=22, height=9, dpi=300)



# Figure ADVANCE ----------------------------------------------------------
adv=data.table(read.csv2("data/ADVANCE.csv",sep=","))
adv <- invisible(melt(adv,measure.vars=names(adv)[grep("[0-9]+",names(adv))],variable.name = "period",variable.factor=FALSE))
adv$period <- substring(adv$period,2)
adv$period <- as.numeric(as.character(adv$period))
adv <- adv[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
adv$value<-as.numeric(adv$value)
adv$value=adv$value/1000

#rename scenarios
adv$Scenario=str_replace_all(adv$Scenario,"INDC2020_1000","2 °C")
adv$Scenario=str_replace_all(adv$Scenario,"INDC","NDC")

#add min/max
advr=adv[,list(min=min(value),max=max(value)),by=c("Scenario","Region","Variable","Unit","period")]
advr=na.omit(advr)

#plot
F31 = ggplot(adv)
F31 = F31 + geom_line(aes(x=period,y=value,colour=Scenario,linetype=Model),show.legend = F)
F31 = F31 + geom_ribbon(data=advr,aes(x=period,ymin=min,ymax=max,fill=Scenario),alpha=0.3)
F31=F31+scale_fill_manual(values=c("2 °C"="#56B4E9","NDC"="#ff7f00"))
F31=F31+scale_colour_manual(values=c("2 °C"="#56B4E9","NDC"="#ff7f00"))
F31=F31+geom_hline(aes(yintercept=0),linetype="dashed")
F31=F31+theme_bw()+theme(axis.text=element_text(size=24),plot.title = element_text(size=28), 
                         legend.text=element_text(size=24),legend.title=element_text(size=24),axis.title.y = element_text(size=26))
F31=F31+theme(legend.position = c(0.1,0.1))
F31=F31+ylab(bquote(paste(CO[2]," emissions (Gt",CO[2],")/yr")))  + xlab("")
F31=F31+ggtitle("Key characteristics of decarbonisation pathways")
ggsave(file=paste0(cfg$outdir,"/","F31",".png"),F31, width=16, height=12, dpi=400)


# Figure CDR --------------------------------------------------------------
cdr=read.csv2("data/CDR.csv",sep=",")
cdr$Scenario=str_replace_all(cdr$Scenario,"2C","2 °C")
cdr$Scenario=str_replace_all(cdr$Scenario,"1.5C","1.5 °C")

cdr=gather(cdr,annual,value,c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7,X.8,X.9,X.10,X.11,X.12,X.13))
cdr=spread(cdr,Variable,value)
setnames(cdr,"avg. CO2 reduction rate","redrate")
setnames(cdr,"Cumulative CDR","cdr")
cdr=data.table(cdr)
cdr$redrate=as.numeric(cdr$redrate)
cdr$cdr=as.numeric(cdr$cdr)
cdr=na.omit(cdr)
cdr[Scenario=="1.5 °C"]$cdr=cdr[Scenario=="1.5 °C"]$cdr*1000

cdrfill3=cdr[Scenario=="2 °C"&CO2_2030%in%c("39.1","18.4")]
cdrfill3$annual<-NULL
cdrfill3=spread(cdrfill3,CO2_2030,redrate)
library(zoo)
cdrfill3$`18.4`=na.approx(cdrfill3$`18.4`,rule=2)
cdrfill3$`39.1`=na.approx(cdrfill3$`39.1`,rule=2)
  
#plot
F23=ggplot(cdr[CO2_2030%in%c("39.1","18.4")])
F23=F23+geom_line(aes(x=cdr,y=redrate,linetype=CO2_2030,colour=Scenario),size=2)
F23=F23+scale_linetype_manual(values=c("39.1"="solid","18.4"="dotted"), #,"26.4"="dotted","30.6"="dotdash"
                              labels=c("39.1"="39.1 (NDCs)","18.4"="18.4"), #"26.4"="26.4","30.6"="30.6"
                              name=bquote(paste("2030 ",CO[2]," fossil fuel & industrial emissions (Gt ",CO[2],"/yr)")))
F23=F23+scale_colour_manual(values=c("2 °C"="#56B4E9","1.5 °C"="#008000"))
F23=F23+geom_ribbon(data=cdrfill3,aes(x=cdr,ymin=`18.4`,ymax=`39.1`),alpha=0.1,show.legend=F)
F23=F23+scale_fill_manual(values=c("2 °C"="#56B4E9","1.5 °C"="#008000"))
F23=F23+theme_bw()+theme(axis.text=element_text(size=26),plot.title = element_text(size=28), 
                         legend.text=element_text(size=26),legend.title=element_text(size=26),axis.title = element_text(size=26))
F23=F23+theme(legend.position = c(0.3,0.85),legend.background = element_blank())
F23=F23+ylab(bquote(paste("Average ",CO[2]," emissions reduction rate (%)")))  + xlab(bquote(paste("Cumulative CDR 2010-2100 (Gt",CO[2],")")))
F23=F23+ggtitle("2030-2050 transition speed vs. CDR requirement")
ggsave(file=paste0(cfg$outdir,"/","F23",".png"),F23, width=16, height=12, dpi=400)


# Figure good practice Mark -----------------------------------------------
# First run GPP_output_sector_article.R in Timer users Mark ClimatePolicies 6R

# **************** FIGURE 1a *****************************
# PREPARE DATA FOR FIGURE 1a
# graphs consists of two lines: current policies and good practice policies
# and nine areas of reductions
# PREPARE data for figure 1a
CPS_fig1a_1 <- select(CPS_Total, year, value, Scenario)
GPP_fig1a_1 <- select(GPP_Total, year, value, Scenario)
NDC_fig1a_1 <- CPS_fig1a_1
NDC_fig1a_1$value <- 0
NDC_fig1a_1$Scenario <- "NDC range (2030)"
TwoC_fig1a_1 <- SSP2_2_6 %>% mutate(Scenario="2-degree scenario (2.6 W/m2) with 66% probability") %>%
  filter(year>=2010, year<=2030)
TwoC_fig1a_1$value[1:9]=GPP_fig1a_1$value[1:9]

# collect data for lines graph
data_fig1a_1 <- rbind(CPS_fig1a_1, GPP_fig1a_1) %>% rbind(NDC_fig1a_1) %>% rbind(TwoC_fig1a_1)
data_fig1a_1$Scenario = factor(data_fig1a_1$Scenario, levels=Scenarios)
data_fig1a_1$value <- data_fig1a_1$value*10^-3
# collect data for areas graph
data_fig1a_2 <- #red_elec_demand %>% rbind(red_RENElectricity_cor_overlap) %>% 
  red_RENElectricity_cor_overlap %>% 
  rbind(red_OilGas_cor_overlap) %>% rbind(red_Industry_cor_overlap) %>% 
  rbind(red_FGases_cor_overlap) %>% rbind(red_BuildingCodes_cor_overlap) %>% 
  rbind(red_Appliances_cor_overlap) %>%
  rbind(red_CAFEStandards_cor_overlap) %>% rbind(red_ElectricCars_cor_overlap) %>%
  rbind(red_Deforestation_cor_overlap)
# total reductions
tmp_GPP_total <- GPP_Total %>% filter(year>=2015, year<=2030, Scenario=="Good practice policies") %>% setNames(c('year', 'value', 'measure'))
tmp_GPP_total$measure <- " "
data_fig1a_2 <- rbind(data_fig1a_2, tmp_GPP_total)
min <- 0
data_fig1a_2$value[data_fig1a_2$value < min] <- min

# show data in GtCO2eq
data_fig1a_2$value <- data_fig1a_2$value*10^-3

#FIGURE 1a graph
data_fig1a_2$measure = factor(data_fig1a_2$measure, levels=Measures)
fig1a_left <- ggplot() +
  geom_area(data=data_fig1a_2, aes(x=year, y=value, fill=measure)) +
  geom_line(data=data_fig1a_1, aes(x=year, y=value, linetype=Scenario, colour=Scenario), size=1.5) +
  geom_segment(mapping=aes(x=2030.25, y=53.4, xend=2030.25, yend=55.9), arrow=arrow(ends="both", type="closed", length=unit(0.1, "cm")), size=3, color="darkgreen") +
  # use blues palete (brewer), but white for total GPP policies
  scale_fill_manual("Reduction", values = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", 
                                            "#4292c6", "#2171b5", "#08519c", "#08306b", "white")) +
  scale_colour_manual("Scenario", values = c("black","black", "white","#56B4E9"),labels=c("Current policies"="National policies","Good practice policies"="Good practice policies",
                                                                                                   "NDC range (2030)"="NDC range (2030)","2-degree scenario (2.6 W/m2) with 66% probability"="2 °C")) + # colour lines 
  scale_linetype_manual("Scenario", values = c("solid","dotted","solid","dashed"), labels=c("Current policies"="National policies","Good practice policies"="Good practice policies",
                                 "NDC range (2030)"="NDC range (2030)","2-degree scenario (2.6 W/m2) with 66% probability"="2 °C"))+
  scale_y_continuous(breaks=seq(0,60,10)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote(paste("Gt",CO[2],"e/yr")))+xlab("")+
  theme(axis.title = element_text(face="bold", size=20)) +
  theme(axis.text = element_text(face="bold", size=20)) +
  theme(legend.text=element_text(size=20)) +
  theme(legend.title=element_text(size=20))
#scale_fill_brewer(palette="Blues")
setwd("~/disks/local/factsheet/src")
ggsave(file=paste0(cfg$outdir,"/","F24",".png"),fig1a_left, width=12, height=8, dpi=300)


