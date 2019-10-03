# Settings and read data --------------------------------------------------
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob

data=invisible(fread(paste0("data/","cdlinks_effort_sharing_compare_20190926-135325",".csv"),header=TRUE))
data <- data.table(invisible(melt(data,measure.vars=names(data)[grep("[0-9]+",names(data))],variable.name = "period",variable.factor=FALSE)))
data$period <- as.numeric(data$period)
data <- data[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
setnames(data, "MODEL", "model")
setnames(data, "SCENARIO", "scenario")
setnames(data, "REGION", "region")
setnames(data, "VARIABLE", "variable")
setnames(data, "UNIT", "unit")
data=na.omit(data)
data$variable <- factor(data$variable)
# remove MESSAGE scenarios with wrong name, erroneously re-imported with R10 update - not needed with new snapshot excluding these
# data=data[!c(model=="MESSAGEix-GLOBIOM_1.1"&scenario%in%c("NPi2020_1000_flex_AP_V4","NPi2020_1000_flex_GF_V4","NPi2020_1000_flex_PCC_V4"))]
  
outdir <- "effort sharing"
if(!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}


# read native model region data -------------------------------------------
native=invisible(fread(paste0("data/","cdlinks_effort_sharing_native_20190926-135431",".csv"),header=TRUE))
native <- data.table(invisible(melt(native,measure.vars=names(native)[grep("[0-9]+",names(native))],variable.name = "period",variable.factor=FALSE)))
native$period <- as.numeric(native$period)
native <- native[!period %in% c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2001,2002,2003,2004,2006,2007,2008,2009,2011,2012,2013,2014,2016,2017,2018,2019,2021,2022,2023,2024,2026,2027,2028,2029,2031,2032,2033,2034,2036,2037,2038,2039,2041,2042,2043,2044,2046,2047,2048,2049,2051,2052,2053,2054,2056,2057,2058,2059,2061,2062,2063,2064,2066,2067,2068,2069,2071,2072,2073,2074,2076,2077,2078,2079,2081,2082,2083,2084,2086,2087,2088,2089,2091,2092,2093,2094,2096,2097,2098,2099,2101,2102,2103,2104,2106,2107,2108,2109)]
setnames(native, "MODEL", "model")
setnames(native, "SCENARIO", "scenario")
setnames(native, "REGION", "region")
setnames(native, "VARIABLE", "variable")
setnames(native, "UNIT", "unit")
native=na.omit(native)
native$variable <- factor(native$variable)
# remove MESSAGE scenarios with wrong name, erroneously re-imported with R10 update - not needed with new snapshot without these
#native=native[!c(model=="MESSAGEix-GLOBIOM_1.1"&scenario%in%c("NPi2020_1000_flex_AP_V4","NPi2020_1000_flex_GF_V4","NPi2020_1000_flex_PCC_V4"))]

# Prepare data for use ----------------------------------------------------
#IMAGE reporting only for effort sharing variables, need to get GDP and emissions from NPi2020_1000
config <-"config_effortsharing"
scencateg <- "scen_categ_V4"
variables <- "variables_xCut"
adjust <- "adjust_reporting_neutrality" #later use adjust reporting Mark? check what goes wrong first, and then need to explain what was done and why - prefer as little as possible adjustments for now, only used for the extra IMAGE CO data and baseline check anyway
addvars <- F
datafile <-"cdlinks_compare_20190904-204536"
source("load_data.R")
all=all[!duplicated(all)] #TODO check what goes wrong here: why is some data duplicated in load_data?

image=all[model=="IMAGE 3.0"&scenario=="NPi2020_1000_V4"&variable%in%c("Emissions|Kyoto Gases","GDP|PPP","Population")] #relabeled IMAGE V5 (the one with R10 level data) to V4 in load_data to make it work
image$Baseline<-NULL
image$Category<-NULL
image$Scope<-NULL
setcolorder(image,c("model","scenario","region","variable","unit","period","value"))
image$period<-as.numeric(image$period)
image1=image
image$scenario<-"NPi2020_1000_domestic_CO_V4"
image1$scenario<-"NPi2020_1000_flexibility_CO_V4"
data=rbind(data,image,image1)

# IMAGE use CO GDP & population also for effort sharing scenarios
ig <- data[model=="IMAGE 3.0"&variable%in%c("GDP|PPP","Population")&scenario=="NPi2020_1000_domestic_CO_V4"] #,"GDP|MER"
ig1=ig
ig2=ig
ig3=ig
ig4=ig
ig5=ig
ig6=ig
ig1$scenario <- "NPi2020_1000_domestic_AP_V4"
ig2$scenario <- "NPi2020_1000_flexibility_AP_V4"
ig3$scenario <- "NPi2020_1000_domestic_PCC_V4"
ig4$scenario <- "NPi2020_1000_flexibility_PCC_V4"
ig5$scenario <- "NPi2020_1000_domestic_GF_V4"
ig6$scenario <- "NPi2020_1000_flexibility_GF_V4"
data <- rbind(data,ig1,ig2,ig3,ig4,ig5,ig6)

#IMAGE use CO emissions for all flexibility scenarios
ie <- data[model=="IMAGE 3.0"&variable=="Emissions|Kyoto Gases"&scenario=="NPi2020_1000_flexibility_CO_V4"]
ie1=ie
ie2=ie
ie3=ie
ie1$scenario <- "NPi2020_1000_flexibility_AP_V4"
ie2$scenario <- "NPi2020_1000_flexibility_PCC_V4"
ie3$scenario <- "NPi2020_1000_flexibility_GF_V4"
data <- rbind(data,ie1,ie2,ie3)

# IMAGE use allowances as emissions for domestic scenarios
ia <- data[model=="IMAGE 3.0"&variable=="Emissions|GHG|Allowance Allocation"&scenario%in%c("NPi2020_1000_domestic_PCC_V4","NPi2020_1000_domestic_AP_V4","NPi2020_1000_domestic_GF_V4")]
ia$variable <- "Emissions|Kyoto Gases"
data <- rbind(data,ia)

# IMAGE policy cost reporting: add trade|value to polic cost|area under MAC curve to get not only the domestic costs in the flexibility scenario
ic = data[model=="IMAGE 3.0" & variable%in%c("Policy Cost|Area under MAC Curve","Trade|Emissions Allowances|Value")]
ic = spread(ic[,!c('unit'),with=FALSE],variable,value)
ic = ic %>% mutate(`Policy Cost|Area under MAC Curve`=`Policy Cost|Area under MAC Curve`-`Trade|Emissions Allowances|Value`)
ic = data.table(gather(ic,variable,value,c("Policy Cost|Area under MAC Curve","Trade|Emissions Allowances|Value")))
ic = ic[variable=="Policy Cost|Area under MAC Curve"]
ic$unit <- unique(data[variable=="Policy Cost|Area under MAC Curve"]$unit)
setcolorder(ic,c("model","scenario","region","variable","unit","period","value"))
data=data[!c(model=="IMAGE 3.0"&variable=="Policy Cost|Area under MAC Curve")]
data <- rbind(data,ic)

# Read in NoPolicy (SSP2) baseline and cost-optimal scenario from 'all' for AP formula check (not available for AIM/CGE[Japan]?)
nopolco = all[Category%in%c("NoPOL","2020_low")&!model=="MESSAGEix-GLOBIOM_1.0"]
nopolco$implementation<-"flexibility"
nopolco$regime<-""
nopolco[Category=="NoPOL"]$regime<-"Baseline"
nopolco[Category=="2020_low"]$regime<-"CO"
nopolco$Baseline<-NULL
nopolco$Category<-NULL
nopolco$Scope<-NULL
setcolorder(nopolco,c("model","scenario","region","variable","unit","period","value"))
nopolco$period<-as.numeric(nopolco$period)
#nopolco$model <- str_replace_all(nopolco$model,"MESSAGEix-GLOBIOM_1.0","MESSAGEix-GLOBIOM_1.1")
nopolco$model <- str_replace_all(nopolco$model,"REMIND-MAgPIE 1.7-3.0","REMIND 2.0")

# MESSAGE and WITCH use trade carbon net exports variable for trade emissions allowances variable (reported 0)
msg1=data[model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions|Value|Carbon|Net Exports")]
msg2=data[model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions|Volume|Carbon|Net Exports")]
msg1$variable<-"Trade|Emissions Allowances|Value"
msg2$variable<-"Trade|Emissions Allowances|Volume"
data=data[!c(model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions Allowances|Value","Trade|Emissions Allowances|Volume"))]
data=rbind(data,msg1,msg2)

msg3=native[model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions|Value|Carbon|Net Exports")]
msg4=native[model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions|Volume|Carbon|Net Exports")]
msg3$variable<-"Trade|Emissions Allowances|Value"
msg4$variable<-"Trade|Emissions Allowances|Volume"
native=native[!c(model%in%c("MESSAGEix-GLOBIOM_1.1","WITCH2016")&variable%in%c("Trade|Emissions Allowances|Value","Trade|Emissions Allowances|Volume"))]
native=rbind(native,msg3,msg4)

# REMIND use native model regions SSA and REF to complement missing R10 regions - TODO delete when region mapping is fixed
# remind=native[model=="REMIND 2.0"&region%in%c("SSA","REF")]
# remind$region=str_replace_all(remind$region,"SSA","R10AFRICA")
# remind$region=str_replace_all(remind$region,"REF","R10REF_ECON")
# data=rbind(data,remind)

# GEM-E3 only use the recSocialSecurity scenarios (latest version) and use flexibility CO also as domestic
data=data[!c(model=="GEM-E3"&scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_PCC"))]
native=native[!c(model=="GEM-E3"&scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_PCC"))]

gemco = data[model=="GEM-E3"&scenario=="NPi2020_1000_flexibility_CO_recSocialSecurity_V5"]
gemco$scenario <-"NPi2020_1000_domestic_CO_recSocialSecurity_V5"
data = rbind(data,gemco)
gemcon = native[model=="GEM-E3"&scenario=="NPi2020_1000_flexibility_CO_recSocialSecurity_V5"]
gemcon$scenario <-"NPi2020_1000_domestic_CO_recSocialSecurity_V5"
native = rbind(native,gemcon)

# TODO convert MER to PPP so calculation of costs as % of GDP|PPP works
ppp = invisible(fread(paste0("data/","PPPcorrectionR10",".csv"),header=TRUE))
ppp = gather(ppp,period,value,c(`2005`,`2010`,`2015`,`2020`,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`,`2055`,`2060`,`2070`,`2080`,`2090`,`2100`))
ppp$period = as.numeric(ppp$period)
gemppp = data[model=="GEM-E3"&variable=="GDP|MER"&region%in%c("R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM","R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")]
gemppp = merge(gemppp,ppp,by=c("region","period"))
gemppp = gemppp%>%mutate(value=value.x/value.y)
gemppp$variable <- "GDP|PPP"
gemppp$value.x<-NULL
gemppp$value.y<-NULL
setcolorder(gemppp,colnames(data))
data=rbind(data,gemppp)

#add implementation and regime for easier selection
data$implementation<-""
data[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_CO","NPi2020_1000_domestic_GF",
                   "NPi2020_1000_domestic_PCC", "NPi2020_1000_domestic_AP_V4","NPi2020_1000_domestic_CO_V4",
                   "NPi2020_1000_domestic_GF_V4","NPi2020_1000_domestic_PCC_V4",
                   'NPi2020_1000_domestic_PCC_recSocialSecurity_V5',"NPi2020_1000_domestic_AP_recSocialSecurity_V5",
                   "NPi2020_1000_domestic_CO_recSocialSecurity_V5")]$implementation<-"domestic"
data[scenario%in%c("NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_PCC",
                   "NPi2020_1000_flexibility_AP_V4","NPi2020_1000_flexibility_GF_V4",
                   "NPi2020_1000_flexibility_PCC_V4","NPi2020_1000_flexibility_CO","NPi2020_1000_flexibility_CO_V4",
                   "NPi2020_1000_flexibility_PCC_recSocialSecurity_V5","NPi2020_1000_flexibility_CO_recSocialSecurity_V5",
                   "NPi2020_1000_flexibility_AP_recSocialSecurity_V5")]$implementation<-"flexibility"
data$regime<-""
data[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_AP_V4","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_AP_V4",
                   "NPi2020_1000_domestic_AP_recSocialSecurity_V5","NPi2020_1000_flexibility_AP_recSocialSecurity_V5")]$regime<-"AP"
data[scenario%in%c("NPi2020_1000_domestic_PCC","NPi2020_1000_domestic_PCC_V4","NPi2020_1000_flexibility_PCC","NPi2020_1000_flexibility_PCC_V4",
                   'NPi2020_1000_domestic_PCC_recSocialSecurity_V5',"NPi2020_1000_flexibility_PCC_recSocialSecurity_V5")]$regime<-"PCC"
data[scenario%in%c("NPi2020_1000_domestic_GF","NPi2020_1000_domestic_GF_V4","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_GF_V4")]$regime<-"GF"
data[scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_CO","NPi2020_1000_domestic_CO_V4","NPi2020_1000_flexibility_CO_V4",
                   "NPi2020_1000_flexibility_CO_recSocialSecurity_V5","NPi2020_1000_domestic_CO_recSocialSecurity_V5")]$regime<-"CO"

native$implementation<-""
native[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_CO","NPi2020_1000_domestic_GF",
                     "NPi2020_1000_domestic_PCC", "NPi2020_1000_domestic_AP_V4","NPi2020_1000_domestic_CO_V4",
                     "NPi2020_1000_domestic_GF_V4","NPi2020_1000_domestic_PCC_V4",
                     'NPi2020_1000_domestic_PCC_recSocialSecurity_V5',"NPi2020_1000_domestic_AP_recSocialSecurity_V5",
                     "NPi2020_1000_domestic_CO_recSocialSecurity_V5")]$implementation<-"domestic"
native[scenario%in%c("NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_PCC",
                     "NPi2020_1000_flexibility_AP_V4","NPi2020_1000_flexibility_GF_V4",
                     "NPi2020_1000_flexibility_PCC_V4","NPi2020_1000_flexibility_CO","NPi2020_1000_flexibility_CO_V4",
                     "NPi2020_1000_flexibility_PCC_recSocialSecurity_V5","NPi2020_1000_flexibility_CO_recSocialSecurity_V5",
                     "NPi2020_1000_flexibility_AP_recSocialSecurity_V5")]$implementation<-"flexibility"
native$regime<-""
native[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_AP_V4","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_AP_V4",
                   "NPi2020_1000_domestic_AP_recSocialSecurity_V5","NPi2020_1000_flexibility_AP_recSocialSecurity_V5")]$regime<-"AP"
native[scenario%in%c("NPi2020_1000_domestic_PCC","NPi2020_1000_domestic_PCC_V4","NPi2020_1000_flexibility_PCC","NPi2020_1000_flexibility_PCC_V4",
                   'NPi2020_1000_domestic_PCC_recSocialSecurity_V5',"NPi2020_1000_flexibility_PCC_recSocialSecurity_V5")]$regime<-"PCC"
native[scenario%in%c("NPi2020_1000_domestic_GF","NPi2020_1000_domestic_GF_V4","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_GF_V4")]$regime<-"GF"
native[scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_CO","NPi2020_1000_domestic_CO_V4","NPi2020_1000_flexibility_CO_V4",
                   "NPi2020_1000_flexibility_CO_recSocialSecurity_V5","NPi2020_1000_domestic_CO_recSocialSecurity_V5")]$regime<-"CO"

#R5=data[region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]
data=data[region%in%c("World","JPN","BRA","CHN","EU","IND","RUS","USA",
                      "R10AFRICA","R10CHINA+","R10EUROPE","R10INDIA+","R10LATIN_AM","R10MIDDLE_EAST","R10NORTH_AM","R10PAC_OECD","R10REF_ECON","R10REST_ASIA")] 
                      #"R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF","ARG","AUS","CAN","MEX","IDN","ROK","SAF","SAU","TUR",
data$region=str_remove_all(data$region,"R10")
#native=native[region%in%c("JPN","BRA","CHN","EEU","EU15","IND","INDIA","JAP","EUR","CHINA","EUROPE","USA","RUS")]
#set constant to easily select data
r10=c("AFRICA","CHINA+","EUROPE","INDIA+","LATIN_AM","MIDDLE_EAST","NORTH_AM","PAC_OECD","REF_ECON","REST_ASIA")

#Order of regimes
data$regime = factor(data$regime,levels=c("CO","AP","PCC","GF"))


# Check AP implementation -------------------------------------------------
###   r_(i,t) 〖APbc〗^*=∛((〖gdp〗_(i,t)/〖pop〗_(i,t) )⁄(〖GDP〗_t/〖POP〗_t ))∙(〖BAU〗_t-A_t)/〖BAU〗_t ∙〖bau〗_(i,t)

# Check step 1 of formula 
AP <- data[regime=="AP"&variable%in%c("GDP|PPP","Population","Emissions|Kyoto Gases","Emissions|GHG|Allowance Allocation")]
AP$unit <-NULL

# first calculate GDP per capita
AP2 = spread(AP[variable%in%c("GDP|PPP","Population")], variable, value)
AP2 = AP2 %>% mutate(gdpcap=`GDP|PPP`/Population)
AP2 = data.table(gather(AP2,variable,value,c("GDP|PPP","Population","gdpcap")))
AP2 = AP2[variable=="gdpcap"]
AP2 = na.omit(AP2)
# then divide by global GDP per capita
AP3 = spread(AP2,region,value)
AP3 = AP3 %>% mutate(JPN=JPN/World,BRA=BRA/World,CHN=CHN/World,EU=EU/World,IND=IND/World,RUS=RUS/World,USA=USA/World) #R5ASIA=R5ASIA/World,R5LAM=R5LAM/World,R5MAF=R5MAF/World,`R5OECD90+EU`=`R5OECD90+EU`/World,
AP3 = data.table(gather(AP3,region,value,c("JPN","BRA","CHN","EU","IND","RUS","USA","World"))) #,"R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF"
AP3 = na.omit(AP3)
# 3rd-order root
AP3$value = AP3$value^(1/3)

#Second part of step 1: global BAU - global allowances / global BAU
AP1 = nopolco[variable%in%c("Emissions|Kyoto Gases")]
AP1 = na.omit(AP1)
AP1$scenario <- NULL
AP1$implementation <- NULL
AP1a = spread(AP1[region=="World"],regime,value)
AP1a = na.omit(AP1a)
AP1a = AP1a %>% mutate(necred=Baseline-CO,global=(Baseline-CO)/Baseline)
AP1a = data.table(gather(AP1a,regime,value,c("global","Baseline","CO","necred")))
AP1c=AP1a[regime=="necred"]
AP1a = AP1a[regime=="global"]
setnames(AP1a,"regime","factor")
setnames(AP1c,"regime","factor")

# multiplied by regional bau
AP1b = merge(AP1[regime=="Baseline"],AP1a,by=c("model","variable","unit","period")) #,"implementation"
AP1b = AP1b %>%mutate(totalfactor=value.x*value.y)
AP1b$value.x<-NULL
AP1b$value.y<-NULL
AP1b$region.y<-NULL
AP1b$regime<-NULL
AP1b$factor<-NULL
setnames(AP1b,"region.x","region")

# reductions before correction factor: AP3$value *AP1b$totalfactor
AP4 = merge(AP3,AP1b,by=c("model","region","period")) #,"implementation"
AP4 = AP4 %>% mutate(ritAPbc=value*totalfactor)
AP4 = data.table(AP4)
write.csv(AP4[period==2030],paste(outdir,"/APstep1.csv",sep=""))

# check also step 2 and 3 to compare allowance
# Step 2: calculate global correction factor
AP5 = AP4[,list(sum(ritAPbc)),by=c("model","period","regime","implementation")]
AP6 = merge(AP5,AP1c,by=c("period","model"))
AP6$corr = AP6$V1 / AP6$value
write.csv(AP6,paste(outdir,"/APstep2.csv",sep=""))

# Step 3: bau - (AP4$ritAPbc/AP6$corr)
AP6$regime<-NULL
AP6$V1<-NULL
AP6$variable<-NULL
AP6$unit<-NULL
AP6$region<-NULL
AP6$factor<-NULL
AP6$value<-NULL
AP4$scenario<-NULL
AP4$regime<-NULL
AP4$variable.x<-NULL
AP4$value<-NULL
AP4$variable.y<-NULL
AP4$unit<-NULL
AP4$totalfactor<-NULL
AP7 = merge(AP1[regime=="Baseline"],AP6,by=c("model","period")) #,"implementation"
AP7 = merge(AP7,AP4, by=c("model","region","period","implementation"))
AP7$allowance=AP7$value-(AP7$ritAPbc/AP7$corr)
write.csv(AP7,paste(outdir,"/APstep3.csv",sep=""))

#check against reported allowances
AP7$variable<-NULL
AP7$unit<-NULL
AP7$regime<-NULL
setnames(AP7,"value","baseline")
# AP7$value<-NULL
# AP7$corr<-NULL
# AP7$ritAPbc<-NULL
AP$regime<-NULL
AP8 = merge(AP7,AP[variable=="Emissions|GHG|Allowance Allocation"],by=c("model","region","period","implementation"))
AP8$check = ifelse(AP8$value==AP8$allowance,"same","diff")
write.csv(AP8,paste(outdir,"/APfinalcheck.csv",sep=""))

# Drivers: Population and GDP ---------------------------------------------
drivers = data[variable%in%c("Population","GDP|PPP")&region%in%r10] #!region=="World"&!
drivers[variable=="GDP|PPP"]$unit<-"billion US$2010/yr"
drivers$variable<-paste(drivers$variable," (",drivers$unit, ")")

# d = ggplot(drivers[implementation=="flexibility"&regime=="PCC"])
# d = d + geom_line(aes(x=period,y=value,colour=model),size=2) #,linetype=model
# d = d + scale_colour_manual(values=c("DNE21+ V.14"="#241E4E","IMAGE 3.0"="#ECA27C","MESSAGEix-GLOBIOM_1.1"="#6B0504",
#                                        "REMIND 2.0"="#73937E", "WITCH2016"="#515751","GEM-E3"="black"))
# d = d + facet_grid(variable~region,scales="free_y")
# d = d + labs(x="",y="")
# d = d + theme_bw() + theme(axis.text=element_text(size=22),strip.text=element_text(size=20),
#                            legend.text = element_text(size=22),legend.title = element_text(size=22),
#                            axis.text.x=element_text(angle=90)) #,axis.title = element_text(size=16)
# ggsave(file=paste(outdir,"/drivers.png",sep=""),d,width=20,height=12,dpi=200)

# Initial allocation ------------------------------------------------------
allocation = data[variable=="Emissions|GHG|Allowance Allocation"&region%in%r10] #!region=="World"&!

#REMIND 2005, 2010 and 2015 reported zero - taking CO there
remind=allocation[model=="REMIND 2.0"&period%in%c(2005,2010,2015,2020)&implementation=="domestic"]
remind$scenario<-NULL
remind=spread(remind,regime,value)
remind=remind%>%mutate(AP=CO,PCC=CO)
remind=gather(remind,regime,value,c("AP","CO","PCC"))
remind$scenario<-"NPi2020_1000_"
setcolorder(remind,colnames(allocation))
allocation=rbind(allocation[!c(model=="REMIND 2.0"&period%in%c(2005,2010,2015,2020)&regime%in%c("AP","PCC"))],remind)

a = ggplot(allocation[!regime=="GF"]) #[period%in%c(2050)]
#a = a + geom_bar(stat="identity", aes(x=regime, y=value,fill=implementation),position="dodge")
a = a + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=2)
a = a + geom_hline(aes(yintercept = 1),size=1)
a = a + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
a = a + facet_grid(region~model,scales="free_y")
a = a + theme_bw() + theme(axis.text=element_text(size=22),strip.text=element_text(size=22),
                           legend.text = element_text(size=20),legend.title = element_text(size=22),
                           axis.title = element_text(size=22))
a = a + ylab(allocation$unit) +xlab("")
ggsave(file=paste(outdir,"/Allowance allocation.png",sep=""),a,width=20,height=14,dpi=200)

# a1 = ggplot(allocation[regime=="PCC"]) #[period%in%c(2050)]
# #a = a + geom_bar(stat="identity", aes(x=regime, y=value,fill=implementation),position="dodge")
# a1 = a1 + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=2)
# a1 = a1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# a1 = a1 + facet_grid(region~model,scales="free_y")
# a1 = a1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# a1 = a1 + ylab(allocation$unit)
# ggsave(file=paste(outdir,"/Allowance allocation_PCC.png",sep=""),a1,width=20,height=12,dpi=200)

# Emissions ---------------------------------------------------------------
# TODO reductions relative to baseline? (get NoPolicy from 'all' - only Kyoto Gases)
# TODO check cumulative emissions in line with carbon budgets?

# e = ggplot(data[variable=="Emissions|Kyoto Gases"&region%in%r10&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #&!region=="World"
# e = e + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=1)
# e = e + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# e = e + facet_grid(region~model,scales="free_y")
# e = e + theme_bw() + theme(axis.text=element_text(size=22),strip.text=element_text(size=20),
#                            legend.text = element_text(size=20),legend.title = element_text(size=22),
#                            axis.title = element_text(size=22))
# e = e + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)+xlab("")
# ggsave(file=paste(outdir,"/GHGemissions.png",sep=""),e,width=20,height=12,dpi=200)

#PCC only
# e0 = ggplot(data[variable=="Emissions|Kyoto Gases"&!region%in%r10&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&regime=="PCC"]) #&!region=="World"
# e0 = e0 + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=1)
# e0 = e0 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# e0 = e0 + facet_grid(region~model,scales="free_y")
# e0 = e0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# e0 = e0 + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)
# ggsave(file=paste(outdir,"/GHGemissions_PCC.png",sep=""),e0,width=20,height=12,dpi=200)

# separately for Japan
# e3 = ggplot(data[variable=="Emissions|Kyoto Gases"&!region%in%r10&model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #&!region=="World"
# e3 = e3 + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=1)
# e3 = e3 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# e3 = e3 + facet_grid(region~model,scales="free_y")
# e3 = e3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# e3 = e3 + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)
# ggsave(file=paste(outdir,"/GHGemissions_JPN.png",sep=""),e3,width=20,height=12,dpi=200)

# Reduction targets
targets=data[variable=="Emissions|Kyoto Gases"&period%in%c(2010,2030,2050)]
targets=spread(targets,period,value)
targets=targets%>%mutate(rel2030=(`2030`-`2010`)/`2010`*100,rel2050=(`2050`-`2010`)/`2010`*100)
targets=data.table(gather(targets,period,value,c("2010","2030","2050","rel2030","rel2050")))
targets=targets[period%in%c("rel2030","rel2050")]
targets$unit<-"%"  
targets$period=str_replace_all(targets$period,"rel2030","2030")
targets$period=str_replace_all(targets$period,"rel2050","2050")

# e1 = ggplot(targets[period==2030&!region%in%r10&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #[implementation=="flexibility"]
# e1 = e1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# e1 = e1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# e1 = e1 + facet_grid(implementation~model)
# e1 = e1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# e1 = e1 + ylab(targets$unit)
# ggsave(file=paste(outdir,"/emissiontargets2030.png",sep=""),e1,width=20,height=12,dpi=200)

e2 = ggplot(targets[period==2050&!region%in%r10&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #[implementation=="flexibility"]
e2 = e2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
e2 = e2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
e2 = e2 + facet_grid(implementation~model)
e2 = e2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
e2 = e2 + ylab(targets$unit)
ggsave(file=paste(outdir,"/emissiontargets2050.png",sep=""),e2,width=20,height=12,dpi=200)

#Separately for Japan
e4 = ggplot(targets[period==2050&!region%in%r10&model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #[implementation=="flexibility"]
e4 = e4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
e4 = e4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
e4 = e4 + facet_grid(implementation~model)
e4 = e4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
e4 = e4 + ylab(targets$unit)
ggsave(file=paste(outdir,"/emissiontargets2050_JPN.png",sep=""),e4,width=20,height=12,dpi=200)

# e5 = ggplot(targets[period==2030&!region%in%r10&model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")&!regime=="GF"]) #[implementation=="flexibility"]
# e5 = e5 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# e5 = e5 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# e5 = e5 + facet_grid(implementation~model)
# e5 = e5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# e5 = e5 + ylab(targets$unit)
# ggsave(file=paste(outdir,"/emissiontargets2030_JPN.png",sep=""),e5,width=20,height=12,dpi=200)

# Trade ---------------------------------------------------------
###Value
finflow = data[variable=="Trade|Emissions Allowances|Value"&region%in%r10]

f = ggplot(finflow[period%in%c(2050)&implementation=="flexibility"]) #2030,2100 &regime%in%c("AP","PCC")
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f = f + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f = f + facet_grid(period~model)
f = f + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
                           axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
f = f + ylab(finflow$unit)
ggsave(file=paste(outdir,"/Trade-allowances-value_2050.png",sep=""),f,width=20,height=12,dpi=200)

fa = ggplot(finflow[period%in%c(2100)&implementation=="flexibility"]) #&regime%in%c("AP","PCC")
fa = fa + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
fa = fa + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
fa = fa + facet_grid(period~model)
fa = fa + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
                           axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
fa = fa + ylab(finflow$unit)
ggsave(file=paste(outdir,"/Trade-allowances-value_2100.png",sep=""),fa,width=20,height=12,dpi=200)

# per model
# for(mod in unique(finflow$model)){
#   f0 = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"&model==mod])
#   f0 = f0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
#   f0 = f0 + scale_fill_brewer(palette="Set3")
#   f0 = f0 + facet_grid(period~regime)
#   f0 = f0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                                axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
#   f0 = f0 + ylab(finflow$unit)
#   f0 = f0 + ggtitle(mod)
#   ggsave(file=paste(outdir,"/Trade-allowances-value_",mod,".png",sep=""),f0,width=20,height=12,dpi=200)
# }

#total financial flows (only summing the positive values (otherwise double counting))
finflowscheck = data[variable=="Trade|Emissions Allowances|Value"&region%in%r10,list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
finflowscheck2 = finflowscheck[V1!=0,list(unique(model))]

finflows = data[variable=="Trade|Emissions Allowances|Value"&value>0&region%in%r10,list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
setnames(finflows,"V1","value")
finflows$variable<-"Total financial flows"
finflowsstat=finflows[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

finflows$period<-as.factor(finflows$period)
# f5 = ggplot(finflows[period%in%c(2030,2050,2100)&implementation=="flexibility"])
# f5 = f5 + geom_bar(stat="identity", aes(x=regime, y=value,fill=period),position="dodge")
# f5 = f5 + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
# f5 = f5 + facet_grid(model~.,scale="free_y")
# f5 = f5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f5 = f5 + ylab(finflows$unit)
# ggsave(file=paste(outdir,"/total_financial_flows_models.png",sep=""),f5,width=20,height=12,dpi=200)

finflowsstat$period<-as.factor(finflowsstat$period)
f5b = ggplot(finflowsstat[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f5b = f5b + geom_bar(stat="identity", aes(x=regime, y=median,fill=period),position=position_dodge(width=0.66),width=0.66)
f5b = f5b + geom_errorbar(aes(x=regime,ymin=min,ymax=max,colour=period),position=position_dodge(width=0.66),width=0.66)
f5b = f5b + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
f5b = f5b + scale_colour_manual(values=c("2030"="black","2050"="black","2100"="black"))
f5b = f5b + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f5b = f5b + ylab(finflowsstat$unit)
ggsave(file=paste(outdir,"/total_financial_flows.png",sep=""),f5b,width=20,height=12,dpi=200)

#for native model regions
finflowsnative = native[variable=="Trade|Emissions Allowances|Value"&value>0,list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
setnames(finflowsnative,"V1","value")
finflowsnative$variable<-"Total financial flows"
finflowsnative$period<-as.factor(finflowsnative$period)
finflowsnativestat=finflowsnative[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

# f5c = ggplot(finflowsnative[period%in%c(2030,2050,2100)&implementation=="flexibility"])
# f5c = f5c + geom_bar(stat="identity", aes(x=regime, y=value,fill=period),position="dodge")
# f5c = f5c + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
# f5c = f5c + facet_grid(model~.,scale="free_y")
# f5c = f5c + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f5c = f5c + ylab(finflowsnative$unit)
# ggsave(file=paste(outdir,"/total_financial_flows_models_native.png",sep=""),f5c,width=20,height=12,dpi=200)
# 

# the other indicators
# f1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Absolute"&!region%in%r10])
# f1 = f1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# f1 = f1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# f1 = f1 + facet_grid(period~model)
# f1 = f1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f1 = f1 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Absolute"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_value-absolute.png",sep=""),f1,width=20,height=12,dpi=200)
# 
# f2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Exports"&!region%in%r10])
# f2 = f2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# f2 = f2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# f2 = f2 + facet_grid(period~model)
# f2 = f2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f2 = f2 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Exports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_value-exports.png",sep=""),f2,width=20,height=12,dpi=200)
# 
# f3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Imports"&!region%in%r10])
# f3 = f3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# f3 = f3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# f3 = f3 + facet_grid(period~model)
# f3 = f3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f3 = f3 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Imports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_value-imports.png",sep=""),f3,width=20,height=12,dpi=200)
# 
# f4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Net Exports"&!region%in%r10])
# f4 = f4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# f4 = f4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# f4 = f4 + facet_grid(period~model)
# f4 = f4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# f4 = f4 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Net Exports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_value-net exports.png",sep=""),f4,width=20,height=12,dpi=200)

###Volume
trade = data[variable=="Trade|Emissions Allowances|Volume"&!region%in%r10]

# t = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"&regime%in%c("AP","PCC")])
# t = t + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# t = t + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# t = t + facet_grid(period~model)
# t = t + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# t = t + ylab(trade$unit)
# ggsave(file=paste(outdir,"/Trade-allowances-volume.png",sep=""),t,width=20,height=12,dpi=200)

# per model
# for(mod in unique(trade$model)){
#   t0 = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"&model==mod])
#   t0 = t0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
#   t0 = t0 + scale_fill_brewer(palette="Accent")
#   t0 = t0 + facet_grid(period~regime)
#   t0 = t0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
#   t0 = t0 + ylab(trade$unit)
#   t0 = t0 + ggtitle(mod)
#   ggsave(file=paste(outdir,"/Trade-allowances-volume_",mod,".png",sep=""),t0,width=20,height=12,dpi=200)
# }

#the other indicators
# t1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Absolute"&!region%in%r10])
# t1 = t1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# t1 = t1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# t1 = t1 + facet_grid(period~model)
# t1 = t1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# t1 = t1 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Absolute"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_volume-absolute.png",sep=""),t1,width=20,height=12,dpi=200)
# 
# t2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Exports"&!region%in%r10])
# t2 = t2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# t2 = t2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# t2 = t2 + facet_grid(period~model)
# t2 = t2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# t2 = t2 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Exports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_volume-exports.png",sep=""),t2,width=20,height=12,dpi=200)
# 
# t3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Imports"&!region%in%r10])
# t3 = t3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# t3 = t3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# t3 = t3 + facet_grid(period~model)
# t3 = t3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# t3 = t3 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Imports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_volume-imports.png",sep=""),t3,width=20,height=12,dpi=200)
# 
# t4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Net Exports"&!region%in%r10])
# t4 = t4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# t4 = t4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# t4 = t4 + facet_grid(period~model)
# t4 = t4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# t4 = t4 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Net Exports"]$unit)
# ggsave(file=paste(outdir,"/Trade-carbon_volume-net exports.png",sep=""),t4,width=20,height=12,dpi=200)

# Costs -------------------------------------------------------------------
# Carbon price
# p = ggplot(data[variable=="Price|Carbon"&!region%in%r10]) #&regime%in%c("AP","CO")
# p = p + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1.5)
# p = p + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p = p + facet_grid(implementation~region)
# p = p + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p = p + ylab(data[variable=="Price|Carbon"]$unit)
# p = p + ylim(0,2000)
# ggsave(file=paste(outdir,"/carbon price.png",sep=""),p,width=20,height=12,dpi=200)

#for 1 country
# p1 = ggplot(data[variable=="Price|Carbon"&region=="CHN"])
# p1 = p1 + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1.5)
# p1 = p1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p1 = p1 + facet_grid(implementation~model)
# p1 = p1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p1 = p1 + ylab(data[variable=="Price|Carbon"]$unit)
# p1 = p1 + ylim(0,2000)
# ggsave(file=paste(outdir,"/carbon price_CHN.png",sep=""),p1,width=20,height=12,dpi=200)

#only 2030
# p2 = ggplot(data[variable=="Price|Carbon"&!region%in%r10&period==2030&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")]) #&regime%in%c("AP","CO")
# p2 = p2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# p2 = p2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p2 = p2 + facet_grid(implementation~model)
# p2 = p2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p2 = p2 + ylab(data[variable=="Price|Carbon"]$unit)
# p2 = p2 + ylim(0,2000)
# ggsave(file=paste(outdir,"/carbon price_2030.png",sep=""),p2,width=20,height=12,dpi=200)

#only 2050
p2a = ggplot(data[variable=="Price|Carbon"&region%in%r10&period==2050&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")]) #&regime%in%c("AP","CO")
p2a = p2a + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
p2a = p2a + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
p2a = p2a + facet_grid(implementation~model,scales="free_y")
p2a = p2a + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                               legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
p2a = p2a + ylab(data[variable=="Price|Carbon"]$unit) + xlab("")
#p2a = p2a + ylim(0,2000)
ggsave(file=paste(outdir,"/carbon price_2050.png",sep=""),p2a,width=20,height=12,dpi=200)

#Separately for Japan
# p3 = ggplot(data[variable=="Price|Carbon"&!region%in%r10&period==2030&model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")]) #&regime%in%c("AP","CO")
# p3 = p3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# p3 = p3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p3 = p3 + facet_grid(implementation~model)
# p3 = p3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p3 = p3 + ylab(data[variable=="Price|Carbon"]$unit)
# p3 = p3 + ylim(0,2000)
# ggsave(file=paste(outdir,"/carbon price_2030_JPN.png",sep=""),p3,width=20,height=12,dpi=200)

price=data[variable=="Price|Carbon"&!region%in%r10]
price[model=="AIM/CGE[Japan]"]$model<-"AIM-CGE[Japan]"
price[model=="AIM/Enduse[Japan]"]$model<-"AIM-Enduse[Japan]"
# for(mod in unique(price$model)){  
#   p0 = ggplot(price[period%in%c(2030,2050,2100)&model==mod]) #&implementation=="flexibility"
#   p0 = p0 + geom_path(aes(x=period, y=value,colour=region),size=1)
#   p0 = p0 + scale_color_brewer(palette="Accent")
#   p0 = p0 + facet_grid(implementation~regime)
#   p0 = p0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
#   p0 = p0 + ylab(price$unit)
#   p0 = p0 + ggtitle(mod)
#   ggsave(file=paste0(outdir,"/carbonprice_",mod,".png"),p0,width=20,height=12,dpi=200)
# }

# Carbon price overview
pricestat=price[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                      min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime","region")]

# p4 = ggplot(pricestat[period%in%c(2030,2050)])
# p4 = p4 + geom_bar(stat="identity", aes(x=region, y=median,fill=regime),position=position_dodge(width=0.66),width=0.66)
# p4 = p4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p4 = p4 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# p4 = p4 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# p4 = p4 + facet_grid(implementation~period,scale="free_y")
# p4 = p4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p4 = p4 + ylab(pricestat$unit)
# ggsave(file=paste(outdir,"/carbonprice_compare.png",sep=""),p4,width=20,height=12,dpi=200)

#without JPN national models
pricestat2=price[!c(model%in%c("AIM-CGE[Japan]","DNE21+ V.14")&region=="JPN"),list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                      min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime","region")]

# p5 = ggplot(pricestat2[period%in%c(2030,2050)])
# p5 = p5 + geom_bar(stat="identity", aes(x=region, y=median,fill=regime),position=position_dodge(width=0.66),width=0.66)
# p5 = p5 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# p5 = p5 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# p5 = p5 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# p5 = p5 + facet_grid(implementation~period,scale="free_y")
# p5 = p5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# p5 = p5 + ylab(pricestat$unit)
# ggsave(file=paste(outdir,"/carbonprice_compare_wo_JPNmodels.png",sep=""),p5,width=20,height=12,dpi=200)

# Policy costs as % of GDP (one policy cost indicator per model, but check with teams (TODO). 1) GDP loss, 2) direct costs (energy system, MAC), 3) consumption loss)
# Check who reports what and make ranking of variables to use
costvar = data[variable%in%c("Policy Cost|Welfare Change","Policy Cost|Additional Total Energy System Cost","Policy Cost|Area under MAC Curve","Policy Cost|Consumption Loss",
                           "Policy Cost|Equivalent Variation","Policy Cost|GDP Loss","Policy Cost|Other","Policy Cost|Default for CAV")]
costvar = costvar[,list(variable=unique(variable)),by=c("model")]
costvar$select=ifelse(costvar$variable=="Policy Cost|Consumption Loss",1,ifelse(costvar$variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Area under MAC Curve"),
                                                                  2,ifelse(costvar$variable=="Policy Cost|GDP Loss",3,4)))
costvar=costvar[select<4]
costvars=costvar[,list(select=min(select)),by=c("model")]
costvars=merge(costvars,costvar,by=c("select","model"))
costvars$select<-NULL

# One variable per model
costs = data[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve")] 
costs = merge(costs,costvars,by=c("variable","model"))

# discounting (to discuss: should it indeed be in 2020 as present value? decreasing discount rate over time?). Tavoni LIMITS: GDP discounted at 5% over 2010-2100
costsd = costs
costsd$discounted = ifelse(costsd$period>2019,costsd$value * (1/(1+0.05)^(costsd$period-2020)),costsd$value)

# divide costs by GDP
gdp = data[variable=="GDP|PPP"] 
gdp$unit <- unique(costs$unit)
costs = rbind(costs,gdp)
costs[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve")]$variable<-"Policy Cost"
costs = spread(costs,variable,value) 
costs = costs%>%mutate(CostGDP=`Policy Cost`/`GDP|PPP`*100)
costs = data.table(gather(costs,variable,value,c("Policy Cost","GDP|PPP","CostGDP")))
costs = costs[variable%in%c("CostGDP")]
costs$unit <- '%'
setnames(costvars,"variable","costvariable")
costs=merge(costs,costvars,by=c("model"))

# also for discounted costs (with discounted GDP)
gdpd = gdp
gdpd$discounted = ifelse(gdpd$period>2019,gdpd$value * (1/(1+0.05)^(gdpd$period-2020)),gdpd$value)
gdpdi = gdpd
gdpdi$value = gdpdi$discounted
gdpdi$discounted <- NULL

costsdi = costsd
costsdi$value = costsdi$discounted
costsdi$discounted <- NULL
setcolorder(costsdi,colnames(gdpdi))
costsdi = rbind(costsdi,gdpdi)

costsdi[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve")]$variable<-"Policy Cost"
costsdis=costsdi
costsdi = spread(costsdi,variable,value) 
costsdi = costsdi%>%mutate(CostGDP=`Policy Cost`/`GDP|PPP`*100)
costsdi = data.table(gather(costsdi,variable,value,c("Policy Cost","GDP|PPP","CostGDP")))
costsdi = costsdi[variable%in%c("CostGDP")]
costsdi$unit <- '%'
costsdi=merge(costsdi,costvars,by=c("model"))

# c = ggplot(costs[implementation=="flexibility"&!region%in%r10])
# c = c + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable),size=1)
# c = c + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c = c + facet_grid(model~region,scale="fixed")
# c = c + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c = c + ylab(costs$unit)
# ggsave(file=paste(outdir,"/costs_GDP_flexibility.png",sep=""),c,width=20,height=12,dpi=200)

# ca = ggplot(costsdi[implementation=="flexibility"&!region%in%r10])
# ca = ca + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable),size=1)
# ca = ca + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# ca = ca + facet_grid(model~region,scale="free_y")
# ca = ca + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# ca = ca + ylab(costsdi$unit)
# ggsave(file=paste(outdir,"/costs_GDP_flexibility_discounted.png",sep=""),ca,width=20,height=12,dpi=200)

cb = ggplot(costs[region%in%r10&period==2050&implementation=="flexibility"])
cb = cb + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
cb = cb + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
cb = cb + facet_grid(regime~region,scale="fixed")
cb = cb + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
cb = cb + ylab(costs$unit)+xlab("")
ggsave(file=paste(outdir,"/costs_GDP_flexibility_2050.png",sep=""),cb,width=20,height=12,dpi=200)

# c1 = ggplot(costs[implementation=="domestic"&!region%in%r10])
# c1 = c1 + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable))
# c1 = c1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c1 = c1 + facet_grid(model~region,scale="free_y")
# c1 = c1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c1 = c1 + ylab(costs$unit)
# ggsave(file=paste(outdir,"/costs_GDP_domestic.png",sep=""),c1,width=20,height=12,dpi=200)
# 
# c1a = ggplot(costsdi[implementation=="domestic"&!region%in%r10])
# c1a = c1a + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable))
# c1a = c1a + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c1a = c1a + facet_grid(model~region,scale="free_y")
# c1a = c1a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c1a = c1a + ylab(costsdi$unit)
# ggsave(file=paste(outdir,"/costs_GDP_domestic_discounted.png",sep=""),c1a,width=20,height=12,dpi=200)

c1b = ggplot(costs[region%in%r10&period==2050&implementation=="domestic"])
c1b = c1b + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
c1b = c1b + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c1b = c1b + facet_grid(regime~region,scale="fixed")
c1b = c1b + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
c1b = c1b + ylab(costs$unit) + xlab("")
ggsave(file=paste(outdir,"/costs_GDP_domestic_2050.png",sep=""),c1b,width=20,height=12,dpi=200)

# statistics over models
costsstat=costs[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                              min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime","region")]
costsdistat=costsdi[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                      min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime","region")]

# c3 = ggplot(costsstat[region%in%r10&period%in%c(2030,2050)])
# c3 = c3 + geom_bar(stat="identity", aes(x=region, y=median,fill=regime),position=position_dodge(width=0.66),width=0.66)
# c3 = c3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c3 = c3 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# c3 = c3 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# c3 = c3 + facet_grid(implementation~period,scale="free_y")
# c3 = c3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),
#                              legend.title = element_text(size=16),axis.title = element_text(size=16),axis.text.x=element_text(angle=90))
# c3 = c3 + ylab(costsstat$unit)
# ggsave(file=paste(outdir,"/costs_GDP_compare.png",sep=""),c3,width=20,height=12,dpi=200)
# 
# c3a = ggplot(costsdistat[!region%in%r10&period%in%c(2030,2050)])
# c3a = c3a + geom_bar(stat="identity", aes(x=region, y=median,fill=regime),position=position_dodge(width=0.66),width=0.66)
# c3a = c3a + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c3a = c3a + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# c3a = c3a + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# c3a = c3a + facet_grid(implementation~period,scale="free_y")
# c3a = c3a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c3a = c3a + ylab(costsdistat$unit)
# ggsave(file=paste(outdir,"/costs_GDP_compare_discounted.png",sep=""),c3a,width=20,height=12,dpi=200)

# Cumulative discounted costs divided by cumulative discounted GDP
costsdicu=costsdis[variable=="Policy Cost"]
yy=seq(2020,2100)
costsdicu = costsdicu[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','model','region','unit','variable','implementation','regime')]
setnames(costsdicu,"V1","value")
setnames(costsdicu,"V2","period")
costsdicu=costsdicu[period %in% c(2020:2100),sum(value,na.rm=TRUE),by=c('scenario','model','region','unit','variable','implementation','regime')]
setnames(costsdicu,"V1","value")

gdpdicu=costsdis[variable=="GDP|PPP"]
yy=seq(2020,2100)
gdpdicu = gdpdicu[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','model','region','unit','variable','implementation','regime')]
setnames(gdpdicu,"V1","value")
setnames(gdpdicu,"V2","period")
gdpdicu=gdpdicu[period %in% c(2020:2100),sum(value,na.rm=TRUE),by=c('scenario','model','region','unit','variable','implementation','regime')]
setnames(gdpdicu,"V1","value")

costsgdpdicu=merge(costsdicu,gdpdicu,by=c("scenario","model","region","unit","implementation","regime"))
costsgdpdicu$value = costsgdpdicu$value.x / costsgdpdicu$value.y *100
costsgdpdicu$unit <- "%"

# c4 = ggplot(costsgdpdicu[region%in%r10&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")])
# c4 = c4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position=position_dodge(width=0.66),width=0.66)
# c4 = c4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# #c4 = c4 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# #c4 = c4 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# c4 = c4 + facet_grid(implementation~model,scale="free_y")
# c4 = c4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),
#                              legend.title = element_text(size=16),axis.title = element_text(size=16),axis.text.x=element_text(angle=90))
# c4 = c4 + ylab(costsgdpdicu$unit)
# ggsave(file=paste(outdir,"/costs_GDP_cumulative_discounted.png",sep=""),c4,width=20,height=12,dpi=200)
# 
# c4a = ggplot(costsgdpdicu[!region%in%r10&model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")])
# c4a = c4a + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position=position_dodge(width=0.66),width=0.66)
# c4a = c4a + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# #c4 = c4 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
# #c4 = c4 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
# c4a = c4a + facet_grid(implementation~model,scale="free_y")
# c4a = c4a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c4a = c4a + ylab(costsgdpdicu$unit)
# ggsave(file=paste(outdir,"/costs_GDP_cumulative_discounted_JPN.png",sep=""),c4a,width=20,height=12,dpi=200)

# costsrel = spread(costs[period%in%c(2020,2030,2050,2100)],period,value)
# costsrel = costsrel%>%mutate(rel2030=(`2030`-`2020`)/`2020`*100,rel2050=(`2050`-`2020`)/`2020`*100,rel2100=(`2100`-`2020`)/`2020`*100)
# costsrel=data.table(gather(costsrel,period,value,c("2020","2030","2050","2100","rel2030","rel2050","rel2100")))
# costsrel=costsrel[period%in%c("rel2030","rel2050","rel2100")]
# costsrel$unit<-"%"  
# costsrel$period=str_replace_all(costsrel$period,"rel2030","2030")
# costsrel$period=str_replace_all(costsrel$period,"rel2050","2050")
# costsrel$period=str_replace_all(costsrel$period,"rel2100","2100")
# costsrel<-na.omit(costsrel)
# 
# c4 = ggplot(costsrel[period%in%c(2030,2050)&implementation=="flexibility"&!model=="IMAGE 3.0"]) #TODO: check what goes wrong with IMAGE
# c4 = c4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# c4 = c4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c4 = c4 + facet_grid(period~model)
# c4 = c4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16),
#                              axis.text.x = element_text(angle=45))
# c4 = c4 + ylab(costsrel$unit)
# ggsave(file=paste(outdir,"/costs_GDP_rel2020.png",sep=""),c4,width=20,height=12,dpi=200)

#Relative to global average 
costsworld = spread(costs[region%in%c(r10,"World")],region,value)
costsworld = costsworld%>%mutate(AFRICAworld=AFRICA/World,CHINAworld=`CHINA+`/World,EUROPEworld=EUROPE/World,INDIAworld=`INDIA+`/World,
                                 LATIN_AMworld=LATIN_AM/World,MIDDLE_EASTworld=MIDDLE_EAST/World,NORTH_AMworld=NORTH_AM/World,PAC_OECDworld=PAC_OECD/World,
                                 REF_ECONworld=REF_ECON/World,REST_ASIAworld=REST_ASIA/World) #BRAworld=BRA/World,CHNworld=CHN/World,EUworld=EU/World,INDworld=IND/World,JPNworld=JPN/World,RUSworld=RUS/World,USAworld=USA/World
costsworld=data.table(gather(costsworld,region,value,c(r10,"AFRICAworld","CHINAworld","EUROPEworld","INDIAworld","LATIN_AMworld","MIDDLE_EASTworld",
                                                       "NORTH_AMworld","PAC_OECDworld","REF_ECONworld","REST_ASIAworld"))) #"BRA","CHN","EU","IND","JPN","RUS","USA","World","BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld"
costsworld=costsworld[region%in%c("AFRICAworld","CHINAworld","EUROPEworld","INDIAworld","LATIN_AMworld","MIDDLE_EASTworld","NORTH_AMworld","PAC_OECDworld","REF_ECONworld","REST_ASIAworld")] #"BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld"
costsworld$unit<-"relative to world"  
costsworld$region=str_replace_all(costsworld$region,"AFRICAworld","AFRICA")
costsworld$region=str_replace_all(costsworld$region,"CHINAworld","CHINA+")
costsworld$region=str_replace_all(costsworld$region,"EUROPEworld","EUROPE")
costsworld$region=str_replace_all(costsworld$region,"INDIAworld","INDIA+")
costsworld$region=str_replace_all(costsworld$region,"LATIN_AMworld","LATIN_AM")
costsworld$region=str_replace_all(costsworld$region,"MIDDLE_EASTworld","MIDDLE_EAST")
costsworld$region=str_replace_all(costsworld$region,"NORTH_AMworld","NORTH_AM")
costsworld$region=str_replace_all(costsworld$region,"PAC_OECDworld","PAC_OECD")
costsworld$region=str_replace_all(costsworld$region,"REF_ECONworld","REF_ECON")
costsworld$region=str_replace_all(costsworld$region,"REST_ASIAworld","REST_ASIA")
costsworld<-na.omit(costsworld)

# also for discounted costs TODO for R10
costsworlddi = spread(costsdi[!region%in%r10],region,value)
costsworlddi = costsworlddi%>%mutate(BRAworld=BRA/World,CHNworld=CHN/World,EUworld=EU/World,INDworld=IND/World,JPNworld=JPN/World,RUSworld=RUS/World,USAworld=USA/World)
costsworlddi=data.table(gather(costsworlddi,region,value,c("BRA","CHN","EU","IND","JPN","RUS","USA","World","BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld")))
costsworlddi=costsworlddi[region%in%c("BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld")]
costsworlddi$unit<-"relative to world"  
costsworlddi$region=str_replace_all(costsworlddi$region,"BRAworld","BRA")
costsworlddi$region=str_replace_all(costsworlddi$region,"CHNworld","CHN")
costsworlddi$region=str_replace_all(costsworlddi$region,"EUworld","EU")
costsworlddi$region=str_replace_all(costsworlddi$region,"INDworld","IND")
costsworlddi$region=str_replace_all(costsworlddi$region,"JPNworld","JPN")
costsworlddi$region=str_replace_all(costsworlddi$region,"RUSworld","RUS")
costsworlddi$region=str_replace_all(costsworlddi$region,"USAworld","USA")
costsworlddi<-na.omit(costsworlddi)

# c5 = ggplot(costsworld[period%in%c(2030,2050)&implementation=="flexibility"])
# c5 = c5 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# c5 = c5 + geom_hline(aes(yintercept=1),size=1)
# c5 = c5 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c5 = c5 + facet_grid(period~model)
# c5 = c5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),
#                              legend.title = element_text(size=16),axis.title = element_text(size=16),axis.text.x=element_text(angle=90))
# c5 = c5 + ylab(costsworld$unit)
# c5 = c5 + ggtitle("Flexibility")
# ggsave(file=paste(outdir,"/costs_GDP_relworld_flexibility.png",sep=""),c5,width=20,height=12,dpi=200)
# 
# c5a = ggplot(costsworld[period%in%c(2030,2050)&implementation=="domestic"])
# c5a = c5a + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
# c5a = c5a + geom_hline(aes(yintercept=1),size=1)
# c5a = c5a + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c5a = c5a + facet_grid(period~model)
# c5a = c5a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),
#                                legend.title = element_text(size=16),axis.title = element_text(size=16),axis.text.x=element_text(angle=90))
# c5a = c5a + ylab(costsworld$unit)
# c5a = c5a + ggtitle("Domestic")
# ggsave(file=paste(outdir,"/costs_GDP_relworld_domestic.png",sep=""),c5a,width=20,height=12,dpi=200)
# 
# c5b = ggplot(costsworld)
# c5b = c5b + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1)
# c5b = c5b + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c5b = c5b + facet_grid(implementation~region,scale="free_y")
# #c5 = c5 + ylim(-3,8)
# c5b = c5b + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),
#                                legend.title = element_text(size=16),axis.title = element_text(size=16),axis.text.x=element_text(angle=90))
# c5b = c5b + ylab(costsworld$unit)
# ggsave(file=paste(outdir,"/costs_GDP_relworld.png",sep=""),c5b,width=20,height=12,dpi=200)
# 
# c5c = ggplot(costsworlddi)
# c5c = c5c + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1)
# c5c = c5c + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c5c = c5c + facet_grid(implementation~region,scale="free_y")
# #c5 = c5 + ylim(-3,8)
# c5c = c5c + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c5c = c5c + ylab(costsworlddi$unit)
# ggsave(file=paste(outdir,"/costs_GDP_relworld_discounted.png",sep=""),c5c,width=20,height=12,dpi=200)

# costs Annex I fraction GDP / fraction GDP non-Annex I. Now for R10 (LIMITS) (Pacific OECD + Europe + North America) / rest (7). Previously R5OECD90+EU / R5REF+R5ASIA+R5LAM+R5MAF. 
# to do for OECD countries / native model regions? not enough countries reported separately, and by not enough models... (delete country filter in data preparation): JPN, AUS, CAN, EU, MEX, TUR, USA (non-OECD: ARG, BRA, CHN, IDN, IND, ROK, RUS, SAF, SAU). 
# TODO check REMIND AP very high...

costratio=spread(costs[region%in%r10],region,value)
costratio=costratio%>%mutate(R10nonOECD=(`AFRICA`+`CHINA+`+`INDIA+`+`LATIN_AM`+`MIDDLE_EAST`+`REF_ECON`+`REST_ASIA`)/7,
                             R10OECD = (`EUROPE`+`NORTH_AM`+`PAC_OECD`)/3,
                             ratio=ifelse(R10nonOECD==0&`R10OECD`==0,0,`R10OECD`/R10nonOECD))
costratio=data.table(gather(costratio,region,value,c("ratio","R10OECD","R10nonOECD","AFRICA","CHINA+","EUROPE","INDIA+","LATIN_AM","MIDDLE_EAST","NORTH_AM","PAC_OECD","REF_ECON","REST_ASIA")))
costratio=costratio[region=="ratio"]
costratio$region<-"OECD/non-OECD"
costratio$variable<-"%GDP-OECD/%GDP-non-OECD"

# c2 = ggplot(costratio[period%in%c(2030,2050)])
# c2 = c2 + geom_bar(stat="identity", aes(x=implementation, y=value,fill=regime),position="dodge")
# c2 = c2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c2 = c2 + facet_grid(period~model)
# c2 = c2 + geom_hline(aes(yintercept = 1),size=1)
# c2 = c2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c2 = c2 + ylab(costratio$variable)
# ggsave(file=paste(outdir,"/costratio_R10_OECD_non-OECD.png",sep=""),c2,width=20,height=12,dpi=200)
# 
# c2b = ggplot(costratio[period%in%c(2030,2050)&!c(model=="REMIND 2.0"&regime=="AP")])
# c2b = c2b + geom_bar(stat="identity", aes(x=implementation, y=value,fill=regime),position="dodge")
# c2b = c2b + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c2b = c2b + facet_grid(period~model)
# c2b = c2b + geom_hline(aes(yintercept = 1),size=1)
# c2b = c2b + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c2b = c2b + ylab(costratio$variable)
# ggsave(file=paste(outdir,"/costratio_R10_OECD_non-OECD_exclREMIND.png",sep=""),c2b,width=20,height=12,dpi=200)

# with discounted costs
costratiodi=spread(costsdi[region%in%r10],region,value)
costratiodi=costratiodi%>%mutate(R10nonOECD=(`AFRICA`+`CHINA+`+`INDIA+`+`LATIN_AM`+`MIDDLE_EAST`+`REF_ECON`+`REST_ASIA`)/7,
                                 R10OECD = (`EUROPE`+`NORTH_AM`+`PAC_OECD`)/3,
                                 ratio=ifelse(R10nonOECD==0&`R10OECD`==0,0,`R10OECD`/R10nonOECD))
costratiodi=data.table(gather(costratiodi,region,value,c("ratio","R10OECD","R10nonOECD","AFRICA","CHINA+","EUROPE","INDIA+","LATIN_AM","MIDDLE_EAST","NORTH_AM","PAC_OECD","REF_ECON","REST_ASIA")))
costratiodi=costratiodi[region=="ratio"]
costratiodi$region<-"OECD/non-OECD"
costratiodi$variable<-"%GDP-OECD/%GDP-non-OECD"

# c2a = ggplot(costratiodi[period%in%c(2030,2050)])
# c2a = c2a + geom_bar(stat="identity", aes(x=implementation, y=value,fill=regime),position="dodge")
# c2a = c2a + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c2a = c2a + facet_grid(period~model)
# c2a = c2a + geom_hline(aes(yintercept = 1),size=1)
# c2a = c2a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# c2a = c2a + ylab(costratio$variable)
# ggsave(file=paste(outdir,"/costratio_R10_OECD_non-OECD_discounted.png",sep=""),c2a,width=20,height=12,dpi=200)

# Cost ratio vs financial flows -------------------------------------------
# model median
costratiostat=costratio[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

finflowsstat$period<-as.numeric(as.character(finflowsstat$period))
indicator=merge(finflowsstat,costratiostat,by=c("implementation","regime","period"))
indicator$period<-as.factor(indicator$period)

# i = ggplot(indicator[implementation=="flexibility"&period%in%c(2030,2050,2100)])
# i = i + geom_point(aes(x=median.x,y=median.y,fill=regime,colour=regime,shape=period),size=5)
# i = i + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# i = i + geom_hline(aes(yintercept=1),size=1)
# i = i + geom_vline(aes(xintercept=100),size=1)
# i = i + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# i = i + ylab(costratio$variable)
# i = i + xlab(finflowsstat$unit)
# ggsave(file=paste(outdir,"/costratio_financialflows.png",sep=""),i,width=20,height=12,dpi=200)

# for the native model regions (to do for cost ratio?)
finflowsnativestat$period<-as.numeric(as.character(finflowsnativestat$period))
indicatorn=merge(finflowsnativestat,costratiostat,by=c("implementation","regime","period"))
indicatorn$period<-as.factor(indicatorn$period)

# i2 = ggplot(indicatorn[implementation=="flexibility"&period%in%c(2030,2050,2100)])
# i2 = i2 + geom_point(aes(x=median.x,y=median.y,fill=regime,colour=regime,shape=period),size=5)
# i2 = i2 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# i2 = i2 + geom_hline(aes(yintercept=1),size=1)
# i2 = i2 + geom_vline(aes(xintercept=100),size=1)
# i2 = i2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# i2 = i2 + ylab(costratio$variable)
# i2 = i2 + xlab(finflowsnativestat$unit)
# ggsave(file=paste(outdir,"/costratio_financialflows_native.png",sep=""),i2,width=20,height=12,dpi=200)

# per model
finflows$period<-as.numeric(as.character(finflows$period))
indicatorm=merge(finflows,costratio,by=c("implementation","regime","period","model"))
indicatorm$period<-as.factor(indicatorm$period)

# for(mod in unique(indicatorm$model)){  
#   i0 = ggplot(indicatorm[implementation=="flexibility"&period%in%c(2030,2050,2100)&model==mod])
#   i0 = i0 + geom_point(aes(x=value.x,y=value.y,fill=regime,colour=regime,shape=period),size=5)
#   i0 = i0 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
#   i0 = i0 + geom_hline(aes(yintercept=1),size=1)
#   i0 = i0 + geom_vline(aes(xintercept=100),size=1)
#   i0 = i0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
#   i0 = i0 + ylab(costratio$variable)
#   i0 = i0 + xlab(finflowsstat$unit)
#   i0 = i0 + ggtitle(mod)
#   ggsave(file=paste0(outdir,"/costratio_financialflows_",mod,".png"),i0,width=20,height=12,dpi=200)
# }

finflowsnative$period<-as.numeric(as.character(finflowsnative$period))
indicatormn=merge(finflowsnative,costratio,by=c("implementation","regime","period","model"))
indicatormn$period<-as.factor(indicatormn$period)

# for(mod in unique(indicatormn$model)){  
#   i1 = ggplot(indicatormn[implementation=="flexibility"&period%in%c(2030,2050,2100)&model==mod])
#   i1 = i1 + geom_point(aes(x=value.x,y=value.y,fill=regime,colour=regime,shape=period),size=5)
#   i1 = i1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
#   i1 = i1 + geom_hline(aes(yintercept=1),size=1)
#   i1 = i1 + geom_vline(aes(xintercept=100),size=1)
#   i1 = i1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
#   i1 = i1 + ylab(costratio$variable)
#   i1 = i1 + xlab(finflowsnative$unit)
#   i1 = i1 + ggtitle(mod)
#   ggsave(file=paste0(outdir,"/costratio_financialflows_native",mod,".png"),i1,width=20,height=12,dpi=200)
# }

#all in one plot
i3 = ggplot(indicatorm[implementation=="flexibility"&period%in%c(2030,2050,2100)])
i3 = i3 + geom_point(aes(x=value.x,y=value.y,colour=regime,alpha=period, shape=model),size=5)
i3 = i3 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
i3 = i3 + scale_alpha_manual(values=c("2030"=0.4,"2050"=0.7,"2100"=1))
i3 = i3 + scale_shape_manual(values=c("WITCH2016"=15,"REMIND 2.0"=16,"IMAGE 3.0"=17,"MESSAGEix-GLOBIOM_1.1"=18))
i3 = i3 + geom_hline(aes(yintercept=1),size=1)
i3 = i3 + geom_vline(aes(xintercept=100),size=1)
i3 = i3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
i3 = i3 + ylab(costratio$variable)
i3 = i3 + xlab(finflowsstat$unit)
ggsave(file=paste(outdir,"/costratio_financialflows_allmodels.png",sep=""),i3,width=20,height=12,dpi=200)
# 
# i4 = ggplot(indicatorm[implementation=="flexibility"&period%in%c(2030,2050,2100)&!c(model=="REMIND 2.0"&regime=="AP")])
# i4 = i4 + geom_point(aes(x=value.x,y=value.y,colour=regime,alpha=period, shape=model),size=5)
# i4 = i4 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# i4 = i4 + scale_alpha_manual(values=c("2030"=0.4,"2050"=0.7,"2100"=1))
# i4 = i4 + scale_shape_manual(values=c("WITCH2016"=15,"REMIND 2.0"=16,"IMAGE 3.0"=17,"MESSAGEix-GLOBIOM_1.1"=18))
# i4 = i4 + geom_hline(aes(yintercept=1),size=1)
# i4 = i4 + geom_vline(aes(xintercept=100),size=1)
# i4 = i4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
# i4 = i4 + ylab(costratio$variable)
# i4 = i4 + xlab(finflowsstat$unit)
# ggsave(file=paste(outdir,"/costratio_financialflows_allmodels_exclREMIND.png",sep=""),i4,width=20,height=12,dpi=200)

# Socioeconomic impacts ---------------------------------------------------
# The % change in GDP, the % change in private consumption  or a welfare indicator such as equivalent variation and maybe also a % change in employment.
soc = data[variable%in%c("Employment","Employment|Agriculture","Employment|Industry","Employment|Service",
                         "Policy Cost|Equivalent Variation","Consumption")]

###Employment
em = soc[variable%in%c("Employment","Employment|Agriculture","Employment|Industry","Employment|Service")]
ema = em
em = spread(em[,!c('scenario'),with=FALSE],regime,value)
em = em%>%mutate(PCCrel=(PCC-CO)/CO*100,APrel=(AP-CO)/CO*100) #GFrel=(GF-CO)/CO*100,
em = data.table(gather(em,regime,value,c("AP","CO","PCC","PCCrel","APrel"))) #,"GF" "GFrel",
em = em[regime%in%c("PCCrel","APrel")] #"GFrel",

#change relative to CO
# emp = ggplot(em[period%in%c(2050)&!region=="World"]) #[period%in%c(2020,2030,2050)]
# emp = emp + geom_bar(aes(x=variable,y=value,fill=regime),stat="identity",position="dodge")
# emp = emp + scale_fill_manual(values=c("APrel"="#003162","CO"="#b31b00","GF"="#b37400","PCCrel"="#4ed6ff"),labels=c("APrel"="AP","PCCrel"="PCC"))
# emp = emp + facet_grid(implementation~region)
# emp = emp + theme_bw()+ theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                                 axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
# emp = emp + xlab("") + ylab("% (relative to CO)")
# ggsave(file=paste(outdir,"/employment_relativeCO.png",sep=""),emp,width=20,height=12,dpi=200)

#absolute
# empl = ggplot(ema[period%in%c(2020,2030,2040,2050)&!variable=="Employment"&!region=="World"&implementation=="flexibility"])
# empl = empl + geom_bar(aes(x=period,y=value,fill=variable),stat="identity")
# empl = empl + facet_grid(regime~region)
# empl = empl + theme_bw()+ theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                               axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
# empl = empl + xlab("") + ylab(soc[variable=="Employment"]$unit)
# ggsave(file=paste(outdir,"/employment_absolute.png",sep=""),empl,width=20,height=12,dpi=200)

###Consumption
# cons = ggplot(soc[variable=="Consumption"&period%in%c(2030,2050)&implementation=="flexibility"&!model=="AIM/CGE[Japan]"]) 
# cons = cons + geom_bar(aes(x=region,y=value,fill=regime),stat="identity",position="dodge")
# cons = cons + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# cons = cons + facet_grid(period~model)
# cons = cons + theme_bw()+ theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                                 axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
# cons = cons + xlab("") + ylab(soc[variable=="Consumption"]$unit)
# ggsave(file=paste(outdir,"/Consumption_2030_2050.png",sep=""),cons,width=20,height=12,dpi=200)

coc = soc[variable=="Consumption"&period%in%c(2015,2020,2030,2050)]
coc = spread(coc,period,value)
coc = coc%>%mutate(`2020`= (`2020`-`2015`)/`2015`*100,`2030`= (`2030`-`2015`)/`2015`*100,`2050`= (`2050`-`2015`)/`2015`*100)
coc = data.table(gather(coc,period,value,c(`2015`,`2020`,`2030`,`2050`)))
coc = coc[!period==2015]

# con = ggplot(coc[implementation=="flexibility"&!period==2020]) 
# con = con + geom_bar(aes(x=model,y=value,fill=regime),stat="identity",position="dodge")
# con = con + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# con = con + facet_grid(period~region)
# con = con + theme_bw()+ theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                                 axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
# con = con + xlab("") + ylab("% (relative to 2015)")
# ggsave(file=paste(outdir,"/Consumption_change.png",sep=""),con,width=20,height=12,dpi=200)

###Equivalent variation
# ev = ggplot(soc[variable=="Policy Cost|Equivalent Variation"&period==2050])  #&implementation=='flexibility'
# ev = ev + geom_bar(aes(x=region,y=value,fill=regime),stat="identity",position="dodge")
# ev = ev + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# ev = ev + facet_grid(implementation~model)
# ev = ev + theme_bw()+ theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),
#                                 axis.title = element_text(size=16),axis.text.x = element_text(angle=90))
# ev = ev + xlab("") + ylab(soc[variable=="Policy Cost|Equivalent Variation"]$unit)
# ggsave(file=paste(outdir,"/Equivalent_variation_2050.png",sep=""),ev,width=20,height=12,dpi=200)

# Technologies employed ---------------------------------------------------

#National models? TODO

# All figures in simple layout - 2050, flexibility only -------------------
### 1. Drivers
F1 = ggplot(drivers[region%in%r10&period==2050&implementation=="flexibility"&regime=="PCC"])
F1 = F1 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F1 = F1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F1 = F1 + facet_grid(variable~region,scale="free_y")
F1 = F1 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F1 = F1 + ylab("")+xlab("")
ggsave(file=paste(outdir,"/drivers_flexibility_2050.png",sep=""),F1,width=20,height=12,dpi=200)

### 2. Allowances
F2 = ggplot(allocation[region%in%r10&period==2050&implementation=="flexibility"])
F2 = F2 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F2 = F2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F2 = F2 + facet_grid(regime~region,scale="fixed")
F2 = F2 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F2 = F2 + ylab(allocation$unit)+xlab("")
ggsave(file=paste(outdir,"/allowances_flexibility_2050.png",sep=""),F2,width=20,height=12,dpi=200)

### 3. Emissions
F3 = ggplot(data[variable=="Emissions|Kyoto Gases"&region%in%r10&period==2050&implementation=="flexibility"&!model%in%c("AIM/CGE[Japan]","AIM/Enduse[Japan]")])
F3 = F3 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F3 = F3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F3 = F3 + facet_grid(regime~region,scale="fixed")
F3 = F3 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F3 = F3 + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)+xlab("")
ggsave(file=paste(outdir,"/emissions_flexibility_2050.png",sep=""),F3,width=20,height=12,dpi=200)

### 4. Carbon price
F4 = ggplot(data[variable=="Price|Carbon"&region%in%r10&period==2050&implementation=="flexibility"])
F4 = F4 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F4 = F4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F4 = F4 + facet_grid(regime~region,scale="fixed")
F4 = F4 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F4 = F4 + ylab(data[variable=="Price|Carbon"]$unit)+xlab("")
ggsave(file=paste(outdir,"/cprice_flexibility_2050.png",sep=""),F4,width=20,height=12,dpi=200)

### 5. Costs relative to world
F5 = ggplot(costsworld[region%in%r10&period==2050&implementation=="flexibility"])
F5 = F5 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F5 = F5 + geom_hline(aes(yintercept=1),size=1)
F5 = F5 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F5 = F5 + facet_grid(regime~region,scale="fixed")
F5 = F5 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F5 = F5 + ylab(costsworld$unit)+xlab("")
ggsave(file=paste(outdir,"/costsrelworld_flexibility_2050.png",sep=""),F5,width=20,height=12,dpi=200)

### 6. Cost ratio OECD/non-OECD
F6 = ggplot(costratio[period%in%c(2050)&implementation=="flexibility"])
F6 = F6 + geom_bar(stat="identity", aes(x=implementation, y=value,fill=regime),position="dodge")
F6 = F6 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F6 = F6 + facet_grid(period~model)
F6 = F6 + geom_hline(aes(yintercept = 1),size=1)
F6 = F6 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
F6 = F6 + ylab(costratio$variable)
ggsave(file=paste(outdir,"/costratio_R10_OECD_non-OECD_flexibility_2050.png",sep=""),F6,width=20,height=12,dpi=200)

### 7. Financial flows
F7 = ggplot(finflow[region%in%r10&period==2050&implementation=="flexibility"])
F7 = F7 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F7 = F7 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F7 = F7 + facet_grid(regime~region,scale="fixed")
F7 = F7 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F7 = F7 + ylab(finflow$unit)+xlab("")
ggsave(file=paste(outdir,"/financialflows_flexibility_2050.png",sep=""),F7,width=20,height=12,dpi=200)

### 10. Equivalent variation
F10 = ggplot(soc[variable=="Policy Cost|Equivalent Variation"&period==2050&implementation=="flexibility"&region%in%r10]) #&region%in%r10 TODO put back when R10 GEM-E3 snapshot ready
F10 = F10 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F10 = F10 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F10 = F10 + facet_grid(regime~region,scale="fixed")
F10 = F10 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F10 = F10 + ylab(soc[variable=="Policy Cost|Equivalent Variation"]$unit)+xlab("")
ggsave(file=paste(outdir,"/equivvariation_flexibility_2050.png",sep=""),F10,width=20,height=12,dpi=200)

### 11. Consumption change
F11 = ggplot(coc[region%in%r10&period==2050&implementation=="flexibility"]) 
F11 = F11 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F11 = F11 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F11 = F11 + facet_grid(regime~region,scale="fixed")
F11 = F11 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F11 = F11 + ylab("% (relative to 2015)")+xlab("")
ggsave(file=paste(outdir,"/consumptionchange_flexibility_2050.png",sep=""),F11,width=20,height=12,dpi=200)

### 12. Employment absolute
F12 = ggplot(ema[period==2050&implementation=="flexibility"&region%in%r10]) #&region%in%r10 TODO put back when R10 GEM-E3 snapshot ready
F12 = F12 + geom_bar(aes(x=variable,y=value,fill=regime),stat="identity")
F12 = F12 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F12 = F12 + facet_grid(regime~region,scale="fixed")
F12 = F12 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F12 = F12 + ylab(ema$unit)+xlab("")
ggsave(file=paste(outdir,"/employment_flexibility_2050.png",sep=""),F12,width=20,height=12,dpi=200)

### 13. Employment relative to CO
F13 = ggplot(em[period==2050&implementation=="flexibility"&region%in%r10]) #&region%in%r10 TODO put back when R10 GEM-E3 snapshot ready
F13 = F13 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F13 = F13 + scale_fill_manual(values=c("APrel"="#003162","CO"="#b31b00","GF"="#b37400","PCCrel"="#4ed6ff"),labels=c("APrel"="AP","PCCrel"="PCC"))
F13 = F13 + facet_grid(regime~region,scale="fixed")
F13 = F13 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F13 = F13 + ylab("% (relative to CO)")+xlab("")
ggsave(file=paste(outdir,"/employmentrelCO_flexibility_2050.png",sep=""),F13,width=20,height=12,dpi=200)

### 14. Costs %GDP (model statistics)
F14 = ggplot(costsstat[region%in%r10&period==2050&implementation=="flexibility"])
F14 = F14 + geom_bar(aes(x=implementation,y=median,fill=regime),stat="identity",position=position_dodge(width=0.66),width=0.66)
F14 = F14 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F14 = F14 + geom_errorbar(aes(x=implementation,ymin=min,ymax=max,colour=regime),stat="identity",position=position_dodge(width=0.66),width=0.66)
F14 = F14 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
F14 = F14 + facet_grid(regime~region,scale="fixed")
F14 = F14 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F14 = F14 + ylab(costsstat$unit)+xlab("")
ggsave(file=paste(outdir,"/costsGDPstat_flexibility_2050.png",sep=""),F14,width=20,height=12,dpi=200)

### 15. Cumulative discounted costs
F15 = ggplot(costsgdpdicu[region%in%r10&implementation=="flexibility"])
F15 = F15 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F15 = F15 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F15 = F15 + facet_grid(regime~region,scale="fixed")
F15 = F15 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F15 = F15 + ylab(costsgdpdicu$unit)+xlab("")
ggsave(file=paste(outdir,"/cumuldisccosts_flexibility_2050.png",sep=""),F15,width=20,height=12,dpi=200)

### 16. Financial flows 2100
F16 = ggplot(finflow[region%in%r10&period==2100&implementation=="flexibility"])
F16 = F16 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F16 = F16 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F16 = F16 + facet_grid(regime~region,scale="fixed")
F16 = F16 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F16 = F16 + ylab(finflow$unit)+xlab("")
ggsave(file=paste(outdir,"/financialflows_flexibility_2100.png",sep=""),F16,width=20,height=12,dpi=200)

### 17. Ctax vs. reductions (compare national global) TODO


### 18. Compare national global other indicator TODO
F18 = ggplot(data[region%in%r10&period==2050&implementation=="flexibility"])
F18 = F18 + geom_bar(aes(x=model,y=value,fill=regime),stat="identity")
F18 = F18 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
F18 = F18 + facet_grid(regime~region,scale="fixed")
F18 = F18 + theme_bw() + theme(axis.text=element_text(size=18),strip.text=element_text(size=18),legend.text = element_text(size=18),
                             legend.title = element_text(size=20),axis.title = element_text(size=20),axis.text.x=element_text(angle=90))
F18 = F18 + ylab(data$unit)+xlab("")
ggsave(file=paste(outdir,"/natglobcomp_flexibility_2050.png",sep=""),F18,width=20,height=12,dpi=200)

### 19. One figure to show time dimension TODO
