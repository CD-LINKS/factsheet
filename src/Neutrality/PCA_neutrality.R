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

# Data preparation --------------------------------------------------------
np=data.table(all)
np=np[Scope=="global"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5REF","R5OECD90+EU")&!model=="COPPE-COFFEE 1.0"]

# write output: overview of original scenarios per category, model and region
u=np[,list(unique(scenario)),by=c("model","Category","region")]
write.csv(u,paste("Neutrality","/Scenario overview.csv",sep=""))

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
p=p+geom_line(data=ghg[Category%in%c("1.5 °C","2 °C")&region%in%c("BRA","CAN","CHN","EU","IDN","IND","JPN","RUS","TUR","USA")], aes(x = period, y=value,linetype=Category))
p=p+geom_line(data=ghgextra[region%in%c("BRA","CAN","CHN","EU","IDN","IND","JPN","RUS","TUR","USA")], aes(x = period, y=value,linetype=Category), color="red") #[region=="CHN"&model=="AIM V2.1"&Category=="1.5 °C"]
p=p+theme_bw()
print(p)

#calculate phase-out year
poy=ghgextra[!duplicated(ghgextra[,list(model,Category,region),with=TRUE]),!c('value','period'),with=FALSE] #variable "Scope","Baseline","scenario"
poy=merge(poy,ghgextra[value<=0,min(period),by=c('model','Category','region')],by=c('model','Category','region'),all=TRUE) #,'variable'
#poy$unit<-NULL
poy[is.na(V1),]$V1="No phase-out" #TODO also with extrapolation some NA - check what it does in PCA?
setnames(poy,"V1","period")

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

## and replace 0 land cover for Japan in REMIND with POLES' value TODO move to adjust reporting?
land = rd[variable%in%c("Land Cover","Land Cover|Cropland")&region=="JPN"&model=="POLES CDL"]
land$model <-"REMIND-MAgPIE 1.7-3.0"
rd=rd[!c(model=="REMIND-MAgPIE 1.7-3.0"&region=="JPN"&variable%in%c("Land Cover","Land Cover|Cropland"))]
rd=rbind(rd,land)

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
#add 0 for AIM and REMIND (not reported) to make PCA work
aimforest = rd[variable=="Population" & model=="AIM V2.1"]
aimforest$value<-0
aimforest$variable<-"Land Cover|Forest|Afforestation and Reforestation"
remindforest = rd[variable=="Population" & model=="REMIND-MAgPIE 1.7-3.0"]
remindforest$value<-0
remindforest$variable<-"Land Cover|Forest|Afforestation and Reforestation"
forest=rbind(forest,aimforest,remindforest)
afforest = merge(forest,poy,by=c("model","Category","region"))

# Calculate Pearson correlation
afforestcor = afforest[,list(cor(value,period.y,method="pearson")),by=c("Category")]

## CCS
ccs = rd[variable=="Carbon Sequestration|CCS"]
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
setnames(poy,"period","value")
poy$period<-"x"
poy$unit<-"Year"
poy$variable<-"Emissions|Kyoto Gases"
setcolorder(poy,colnames(ccsx))
pca=rbind(popdx,nonco2x,prodx,forestx,ccsx,gdpcapx,cropsharex,forestsharex,emisintx,emiscapx,transportsharex,buildingsharex,industrysharex,blg50x,blg100x,poy)
pca=spread(pca[,!c('unit','period'),with=FALSE],variable,value)
pca=gather(pca,variable,value,c(`Emissions|Kyoto Gases`))
pca$variable<-NULL

# For all models together and per individual model 
pca=data.table(pca)
pca=pca[Category%in%c("2 °C","1.5 °C")&!region%in%c("World")]
pca$ID <-with(pca,paste0(region,"-",value))

# calculate principal components - TODO fix what it does with remaining NA phase-out year
pca.pca <- prcomp(pca[,c(4:18)], center = TRUE,scale. = TRUE)
summary(pca.pca)
str(pca.pca)

#plot TODO for other models individually?
library(ggbiplot)
# pI = ggbiplot(pcaI.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pcaI$ID, groups=pcaI$Category)  +
#   #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
#   ggtitle("PCA of regional phase-out years in IMAGE")+
#   theme_bw()+
#   theme(legend.position = "bottom")
# ggsave(file=paste(outdir,"/PCA_IMAGE",".png",sep=""),pI,height=12, width=16,dpi=500)

pall = ggbiplot(pca.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,labels=pca$ID, groups=pca$model)  +
  #scale_colour_manual(name="Scenario", values= c("forest green", "dark blue"))+
  ggtitle("PCA of regional phase-out years")+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave(file=paste(outdir,"/PCA_all",".png",sep=""),pall,height=12, width=16,dpi=500)

