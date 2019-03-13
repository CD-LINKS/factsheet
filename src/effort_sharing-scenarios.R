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

data=invisible(fread(paste0("data/","cdlinks_effort_sharing_compare_20190220-120059",".csv"),header=TRUE))
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

outdir <- "effort sharing"
if(!file.exists(outdir)) {
  dir.create(outdir, recursive = TRUE)
}


# read native model region data -------------------------------------------
native=invisible(fread(paste0("data/","cdlinks_effort_sharing_native_20190226-141849",".csv"),header=TRUE))
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

# Prepare data for use ----------------------------------------------------
#IMAGE reporting only for effort sharing variables, need to get GDP and emissions from NPi2020_1000 (only works if 'all' exists in workspace - by running load_data)
image=all[model=="IMAGE 3.0"&scenario=="NPi2020_1000_V4"]
image$Baseline<-NULL
image$Category<-NULL
image$Scope<-NULL
setcolorder(image,c("model","scenario","region","variable","unit","period","value"))
image$period<-as.numeric(image$period)
image1=image
image$scenario<-"NPi2020_1000_domestic_CO"
image1$scenario<-"NPi2020_1000_flexibility_CO"
data=rbind(data,image,image1)
# IMAGE use CO GDP also for effort sharing scenarios
ig <- data[model=="IMAGE 3.0"&variable=="GDP|MER"&scenario=="NPi2020_1000_domestic_CO"]
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

#add implementation and regime for easier selection
data$implementation<-""
data[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_CO","NPi2020_1000_domestic_GF",
                   "NPi2020_1000_domestic_PCC", "NPi2020_1000_domestic_AP_V4","NPi2020_1000_domestic_GF_V4",
                   "NPi2020_1000_domestic_PCC_V4")]$implementation<-"domestic"
data[scenario%in%c("NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_PCC",
                   "NPi2020_1000_flexibility_AP_V4","NPi2020_1000_flexibility_GF_V4",
                   "NPi2020_1000_flexibility_PCC_V4","NPi2020_1000_flexibility_CO")]$implementation<-"flexibility"
data$regime<-""
data[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_AP_V4","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_AP_V4")]$regime<-"AP"
data[scenario%in%c("NPi2020_1000_domestic_PCC","NPi2020_1000_domestic_PCC_V4","NPi2020_1000_flexibility_PCC","NPi2020_1000_flexibility_PCC_V4")]$regime<-"PCC"
data[scenario%in%c("NPi2020_1000_domestic_GF","NPi2020_1000_domestic_GF_V4","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_GF_V4")]$regime<-"GF"
data[scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_CO")]$regime<-"CO"

native$implementation<-""
native[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_CO","NPi2020_1000_domestic_GF",
                   "NPi2020_1000_domestic_PCC", "NPi2020_1000_domestic_AP_V4","NPi2020_1000_domestic_GF_V4",
                   "NPi2020_1000_domestic_PCC_V4")]$implementation<-"domestic"
native[scenario%in%c("NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_PCC",
                   "NPi2020_1000_flexibility_AP_V4","NPi2020_1000_flexibility_GF_V4",
                   "NPi2020_1000_flexibility_PCC_V4","NPi2020_1000_flexibility_CO")]$implementation<-"flexibility"
native$regime<-""
native[scenario%in%c("NPi2020_1000_domestic_AP","NPi2020_1000_domestic_AP_V4","NPi2020_1000_flexibility_AP","NPi2020_1000_flexibility_AP_V4")]$regime<-"AP"
native[scenario%in%c("NPi2020_1000_domestic_PCC","NPi2020_1000_domestic_PCC_V4","NPi2020_1000_flexibility_PCC","NPi2020_1000_flexibility_PCC_V4")]$regime<-"PCC"
native[scenario%in%c("NPi2020_1000_domestic_GF","NPi2020_1000_domestic_GF_V4","NPi2020_1000_flexibility_GF","NPi2020_1000_flexibility_GF_V4")]$regime<-"GF"
native[scenario%in%c("NPi2020_1000_domestic_CO","NPi2020_1000_flexibility_CO")]$regime<-"CO"

#R5=data[region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]
data=data[region%in%c("World","JPN","BRA","CHN","EU","IND","RUS","USA","R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")] #,"ARG","AUS","CAN","MEX","IDN","ROK","SAF","SAU","TUR",
#native=native[region%in%c("JPN","BRA","CHN","EEU","EU15","IND","INDIA","JAP","EUR","CHINA","EUROPE","USA","RUS")]

# Initial allocation ------------------------------------------------------
allocation = data[variable=="Emissions|GHG|Allowance Allocation"&!region=="World"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]

a = ggplot(allocation) #[period%in%c(2050)]
#a = a + geom_bar(stat="identity", aes(x=regime, y=value,fill=implementation),position="dodge")
a = a + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=2)
a = a + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
a = a + facet_grid(region~model,scales="free_y")
a = a + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
a = a + ylab(allocation$unit)
ggsave(file=paste(outdir,"/Allowance allocation.png",sep=""),a,width=20,height=12,dpi=200)

# Emissions ---------------------------------------------------------------
# TODO reductions relative to baseline? (get NoPolicy from 'all' - only Kyoto Gases)
# TODO check cumulative emissions in line with carbon budgets?

e = ggplot(data[variable=="Emissions|Kyoto Gases"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]) #&!region=="World"
e = e + geom_line(aes(x=period,y=value,linetype=implementation,colour=regime),size=1)
e = e + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
e = e + facet_grid(region~model,scales="free_y")
e = e + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
e = e + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)
ggsave(file=paste(outdir,"/GHGemissions.png",sep=""),e,width=20,height=12,dpi=200)

# Reduction targets
targets=data[variable=="Emissions|Kyoto Gases"&period%in%c(2010,2030,2050)]
targets=spread(targets,period,value)
targets=targets%>%mutate(rel2030=(`2030`-`2010`)/`2010`*100,rel2050=(`2050`-`2010`)/`2010`*100)
targets=data.table(gather(targets,period,value,c("2010","2030","2050","rel2030","rel2050")))
targets=targets[period%in%c("rel2030","rel2050")]
targets$unit<-"%"  
targets$period=str_replace_all(targets$period,"rel2030","2030")
targets$period=str_replace_all(targets$period,"rel2050","2050")

e1 = ggplot(targets[period==2030&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]) #[implementation=="flexibility"]
e1 = e1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
e1 = e1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
e1 = e1 + facet_grid(implementation~model)
e1 = e1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
e1 = e1 + ylab(targets$unit)
ggsave(file=paste(outdir,"/emissiontargets2030.png",sep=""),e1,width=20,height=12,dpi=200)

e2 = ggplot(targets[period==2050&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]) #[implementation=="flexibility"]
e2 = e2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
e2 = e2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
e2 = e2 + facet_grid(implementation~model)
e2 = e2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
e2 = e2 + ylab(targets$unit)
ggsave(file=paste(outdir,"/emissiontargets2050.png",sep=""),e2,width=20,height=12,dpi=200)

# Trade ---------------------------------------------------------
###Value
finflow = data[variable=="Trade|Emissions Allowances|Value"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]

f = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"&regime%in%c("AP","PCC")])
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f = f + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f = f + facet_grid(period~model)
f = f + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f = f + ylab(finflow$unit)
ggsave(file=paste(outdir,"/Trade-allowances-value.png",sep=""),f,width=20,height=12,dpi=200)

# per model
for(mod in unique(finflow$model)){
  f0 = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"&model==mod])
  f0 = f0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
  f0 = f0 + scale_fill_brewer(palette="Accent")
  f0 = f0 + facet_grid(period~regime)
  f0 = f0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
  f0 = f0 + ylab(finflow$unit)
  f0 = f0 + ggtitle(mod)
  ggsave(file=paste(outdir,"/Trade-allowances-value_",mod,".png",sep=""),f0,width=20,height=12,dpi=200)
}

#total financial flows (only summing the positive values (otherwise double counting))
finflowscheck = data[variable=="Trade|Emissions Allowances|Value"&region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF"),list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
finflowscheck2 = finflowscheck[V1!=0,list(unique(model))]

finflows = data[variable=="Trade|Emissions Allowances|Value"&value>0&region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF"),list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
setnames(finflows,"V1","value")
finflows$variable<-"Total financial flows"
finflowsstat=finflows[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

finflows$period<-as.factor(finflows$period)
f5 = ggplot(finflows[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f5 = f5 + geom_bar(stat="identity", aes(x=regime, y=value,fill=period),position="dodge")
f5 = f5 + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
f5 = f5 + facet_grid(model~.,scale="free_y")
f5 = f5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f5 = f5 + ylab(finflows$unit)
ggsave(file=paste(outdir,"/total_financial_flows_models.png",sep=""),f5,width=20,height=12,dpi=200)

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

f5c = ggplot(finflowsnative[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f5c = f5c + geom_bar(stat="identity", aes(x=regime, y=value,fill=period),position="dodge")
f5c = f5c + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
f5c = f5c + facet_grid(model~.,scale="free_y")
f5c = f5c + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f5c = f5c + ylab(finflowsnative$unit)
ggsave(file=paste(outdir,"/total_financial_flows_models_native.png",sep=""),f5c,width=20,height=12,dpi=200)


# the other indicators
f1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Absolute"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
f1 = f1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f1 = f1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f1 = f1 + facet_grid(period~model)
f1 = f1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f1 = f1 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Absolute"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-absolute.png",sep=""),f1,width=20,height=12,dpi=200)

f2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Exports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
f2 = f2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f2 = f2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f2 = f2 + facet_grid(period~model)
f2 = f2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f2 = f2 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-exports.png",sep=""),f2,width=20,height=12,dpi=200)

f3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Imports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
f3 = f3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f3 = f3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f3 = f3 + facet_grid(period~model)
f3 = f3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f3 = f3 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Imports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-imports.png",sep=""),f3,width=20,height=12,dpi=200)

f4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Net Exports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
f4 = f4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f4 = f4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
f4 = f4 + facet_grid(period~model)
f4 = f4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
f4 = f4 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Net Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-net exports.png",sep=""),f4,width=20,height=12,dpi=200)

###Volume
trade = data[variable=="Trade|Emissions Allowances|Volume"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]

t = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"&regime%in%c("AP","PCC")])
t = t + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t = t + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
t = t + facet_grid(period~model)
t = t + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
t = t + ylab(trade$unit)
ggsave(file=paste(outdir,"/Trade-allowances-volume.png",sep=""),t,width=20,height=12,dpi=200)

# per model
for(mod in unique(trade$model)){
  t0 = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"&model==mod])
  t0 = t0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
  t0 = t0 + scale_fill_brewer(palette="Accent")
  t0 = t0 + facet_grid(period~regime)
  t0 = t0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
  t0 = t0 + ylab(trade$unit)
  t0 = t0 + ggtitle(mod)
  ggsave(file=paste(outdir,"/Trade-allowances-volume_",mod,".png",sep=""),t0,width=20,height=12,dpi=200)
}

#the other indicators
t1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Absolute"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
t1 = t1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t1 = t1 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
t1 = t1 + facet_grid(period~model)
t1 = t1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
t1 = t1 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Absolute"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-absolute.png",sep=""),t1,width=20,height=12,dpi=200)

t2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Exports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
t2 = t2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t2 = t2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
t2 = t2 + facet_grid(period~model)
t2 = t2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
t2 = t2 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-exports.png",sep=""),t2,width=20,height=12,dpi=200)

t3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Imports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
t3 = t3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t3 = t3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
t3 = t3 + facet_grid(period~model)
t3 = t3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
t3 = t3 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Imports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-imports.png",sep=""),t3,width=20,height=12,dpi=200)

t4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Net Exports"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
t4 = t4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t4 = t4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
t4 = t4 + facet_grid(period~model)
t4 = t4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
t4 = t4 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Net Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-net exports.png",sep=""),t4,width=20,height=12,dpi=200)

# Costs -------------------------------------------------------------------
# Carbon price
p = ggplot(data[variable=="Price|Carbon"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]) #&regime%in%c("AP","CO")
p = p + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1.5)
p = p + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
p = p + facet_grid(implementation~region)
p = p + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
p = p + ylab(data[variable=="Price|Carbon"]$unit)
p = p + ylim(0,2000)
ggsave(file=paste(outdir,"/carbon price.png",sep=""),p,width=20,height=12,dpi=200)

price=data[variable=="Price|Carbon"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")]
price[model=="AIM/CGE[Japan]"]$model<-"AIM-CGE[Japan]"
price[model=="AIM/Enduse[Japan]"]$model<-"AIM-Enduse[Japan]"
for(mod in unique(price$model)){  
  p0 = ggplot(price[period%in%c(2030,2050,2100)&model==mod]) #&implementation=="flexibility"
  p0 = p0 + geom_path(aes(x=period, y=value,colour=region),size=1)
  p0 = p0 + scale_color_brewer(palette="Accent")
  p0 = p0 + facet_grid(implementation~regime)
  p0 = p0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
  p0 = p0 + ylab(price$unit)
  p0 = p0 + ggtitle(mod)
  ggsave(file=paste0(outdir,"/carbonprice_",mod,".png"),p0,width=20,height=12,dpi=200)
}

# Policy costs as % of GDP (one policy cost indicator per model, but check with teams (TODO). 1) GDP loss, 2) direct costs (energy system, MAC), 3) consumption loss)
# Check who reports what and make ranking of variables to use
costvar = data[variable%in%c("Policy Cost|Welfare Change","Policy Cost|Additional Total Energy System Cost","Policy Cost|Area under MAC Curve","Policy Cost|Consumption Loss",
                           "Policy Cost|Equivalent Variation","Policy Cost|GDP Loss","Policy Cost|Other","Policy Cost|Default for CAV")]
costvar = costvar[,list(variable=unique(variable)),by=c("model")]
costvar$select=ifelse(costvar$variable=="Policy Cost|GDP Loss",1,ifelse(costvar$variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Area under MAC Curve"),
                                                                  2,ifelse(costvar$variable=="Policy Cost|Consumption Loss",3,4)))
costvar=costvar[select<4]
costvars=costvar[,list(select=min(select)),by=c("model")]
costvars=merge(costvars,costvar,by=c("select","model"))
costvars$select<-NULL

# One variable per model, divide by GDP
costs = data[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve")] 
costs = merge(costs,costvars,by=c("variable","model"))
gdp = data[variable=="GDP|MER"]
gdp$unit <- unique(costs$unit)
costs = rbind(costs,gdp)
costs[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve")]$variable<-"Policy Cost"
costs = spread(costs,variable,value) 
costs = costs%>%mutate(CostGDP=`Policy Cost`/`GDP|MER`*100)
costs = data.table(gather(costs,variable,value,c("Policy Cost","GDP|MER","CostGDP")))
costs = costs[variable%in%c("CostGDP")]
costs$unit <- '%'
setnames(costvars,"variable","costvariable")
costs=merge(costs,costvars,by=c("model"))

c = ggplot(costs[implementation=="flexibility"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
c = c + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable),size=1)
c = c + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c = c + facet_grid(model~region,scale="free_y")
c = c + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
c = c + ylab(costs$unit)
ggsave(file=paste(outdir,"/costs_GDP_flexibility.png",sep=""),c,width=20,height=12,dpi=200)
  
c1 = ggplot(costs[implementation=="domestic"&!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")])
c1 = c1 + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable))
c1 = c1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c1 = c1 + facet_grid(model~region,scale="free_y")
c1 = c1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
c1 = c1 + ylab(costs$unit)
ggsave(file=paste(outdir,"/costs_GDP_domestic.png",sep=""),c1,width=20,height=12,dpi=200)

costsstat=costs[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                              min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime","region")]

c3 = ggplot(costsstat[!region%in%c("R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF")&period%in%c(2030,2050)])
c3 = c3 + geom_bar(stat="identity", aes(x=region, y=median,fill=regime),position=position_dodge(width=0.66),width=0.66)
c3 = c3 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c3 = c3 + geom_errorbar(aes(x=region,ymin=min,ymax=max,colour=regime),position=position_dodge(width=0.66),width=0.66)
c3 = c3 + scale_colour_manual(values=c("AP"="black","CO"="black","GF"="black","PCC"="black"))
c3 = c3 + facet_grid(implementation~period,scale="free_y")
c3 = c3 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
c3 = c3 + ylab(costsstat$unit)
ggsave(file=paste(outdir,"/costs_GDP_compare.png",sep=""),c3,width=20,height=12,dpi=200)

costsrel = spread(costs[period%in%c(2020,2030,2050,2100)],period,value)
costsrel = costsrel%>%mutate(rel2030=(`2030`-`2020`)/`2020`*100,rel2050=(`2050`-`2020`)/`2020`*100,rel2100=(`2100`-`2020`)/`2020`*100)
costsrel=data.table(gather(costsrel,period,value,c("2020","2030","2050","2100","rel2030","rel2050","rel2100")))
costsrel=costsrel[period%in%c("rel2030","rel2050","rel2100")]
costsrel$unit<-"%"  
costsrel$period=str_replace_all(costsrel$period,"rel2030","2030")
costsrel$period=str_replace_all(costsrel$period,"rel2050","2050")
costsrel$period=str_replace_all(costsrel$period,"rel2100","2100")
costsrel<-na.omit(costsrel)

c4 = ggplot(costsrel[period%in%c(2030,2050,2100)&implementation=="flexibility"&!model=="IMAGE 3.0"]) #TODO: check what goes wrong with IMAGE
c4 = c4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
c4 = c4 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c4 = c4 + facet_grid(period~model)
c4 = c4 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16),
                             axis.text.x = element_text(angle=45))
c4 = c4 + ylab(costsrel$unit)
ggsave(file=paste(outdir,"/costs_GDP_rel2020.png",sep=""),c4,width=20,height=12,dpi=200)

#Relative to global average - leave out relative to 2020?
costsworld = spread(costs[!region%in%c("R5ASIA","R5REF","R5LAM","R5MAF","R5OECD90+EU")],region,value)
costsworld = costsworld%>%mutate(BRAworld=BRA/World,CHNworld=CHN/World,EUworld=EU/World,INDworld=IND/World,JPNworld=JPN/World,RUSworld=RUS/World,USAworld=USA/World)
costsworld=data.table(gather(costsworld,region,value,c("BRA","CHN","EU","IND","JPN","RUS","USA","World","BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld")))
costsworld=costsworld[region%in%c("BRAworld","CHNworld","EUworld","INDworld","JPNworld","RUSworld","USAworld")]
costsworld$unit<-"fraction of world"  
costsworld$region=str_replace_all(costsworld$region,"BRAworld","BRA")
costsworld$region=str_replace_all(costsworld$region,"CHNworld","CHN")
costsworld$region=str_replace_all(costsworld$region,"EUworld","EU")
costsworld$region=str_replace_all(costsworld$region,"INDworld","IND")
costsworld$region=str_replace_all(costsworld$region,"JPNworld","JPN")
costsworld$region=str_replace_all(costsworld$region,"RUSworld","RUS")
costsworld$region=str_replace_all(costsworld$region,"USAworld","USA")
costsworld<-na.omit(costsworld)

c5 = ggplot(costsworld[period%in%c(2030,2050,2100)&implementation=="domestic"])
c5 = c5 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
c5 = c5 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c5 = c5 + facet_grid(period~model)
# c5 = c5 + geom_path(aes(x=period,y=value,colour=regime,linetype=model),size=1)
# c5 = c5 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
# c5 = c5 + facet_grid(implementation~region,scale="free_y")
#c5 = c5 + ylim(-3,8)
c5 = c5 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
c5 = c5 + ylab(costsworld$unit)
c5 = c5 + ggtitle("Domestic")
ggsave(file=paste(outdir,"/costs_GDP_relworld_domestic.png",sep=""),c5,width=20,height=12,dpi=200)

# costs Annex I fraction GDP / fraction GDP non-Annex I. Now for R5OECD90+EU / R5REF+R5ASIA+R5LAM+R5MAF. 
# TODO for OECD countries / native model regions? (delete country filter in data preparation): JPN, AUS, CAN, EU, MEX, TUR, USA (non-OECD: ARG, BRA, CHN, IDN, IND, ROK, RUS, SAF, SAU). 

costratio=spread(costs[region%in%c("R5ASIA","R5REF","R5LAM","R5MAF","R5OECD90+EU")],region,value)
costratio=costratio%>%mutate(R5mean=(`R5ASIA`+`R5LAM`+`R5MAF`+`R5REF`)/4,ratio=ifelse(R5mean==0&`R5OECD90+EU`==0,0,`R5OECD90+EU`/R5mean))
costratio=data.table(gather(costratio,region,value,c("R5ASIA","R5REF","R5LAM","R5MAF","R5OECD90+EU","R5mean","ratio")))
costratio=costratio[region=="ratio"]
costratio$region<-"OECD90+EU/non-OECD"
costratio$variable<-"%GDP-OECD/%GDP-non-OECD"

c2 = ggplot(costratio[period%in%c(2030,2050)])
c2 = c2 + geom_bar(stat="identity", aes(x=implementation, y=value,fill=regime),position="dodge")
c2 = c2 + scale_fill_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
c2 = c2 + facet_grid(period~model)
c2 = c2 + geom_hline(aes(yintercept = 1),size=1)
c2 = c2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
c2 = c2 + ylab(costratio$variable)
ggsave(file=paste(outdir,"/costratio_OECD_R5rest.png",sep=""),c2,width=20,height=12,dpi=200)


# Cost ratio vs financial flows -------------------------------------------
# model median
costratiostat=costratio[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

finflowsstat$period<-as.numeric(as.character(finflowsstat$period))
indicator=merge(finflowsstat,costratiostat,by=c("implementation","regime","period"))
indicator$period<-as.factor(indicator$period)

i = ggplot(indicator[implementation=="flexibility"&period%in%c(2030,2050,2100)])
i = i + geom_point(aes(x=median.x,y=median.y,fill=regime,colour=regime,shape=period),size=5)
i = i + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
i = i + geom_hline(aes(yintercept=1),size=1)
i = i + geom_vline(aes(xintercept=100),size=1)
i = i + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
i = i + ylab(costratio$variable)
i = i + xlab(finflowsstat$unit)
ggsave(file=paste(outdir,"/costratio_financialflows.png",sep=""),i,width=20,height=12,dpi=200)

# for the native model regions
finflowsnativestat$period<-as.numeric(as.character(finflowsnativestat$period))
indicatorn=merge(finflowsnativestat,costratiostat,by=c("implementation","regime","period"))
indicatorn$period<-as.factor(indicatorn$period)

i2 = ggplot(indicatorn[implementation=="flexibility"&period%in%c(2030,2050,2100)])
i2 = i2 + geom_point(aes(x=median.x,y=median.y,fill=regime,colour=regime,shape=period),size=5)
i2 = i2 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
i2 = i2 + geom_hline(aes(yintercept=1),size=1)
i2 = i2 + geom_vline(aes(xintercept=100),size=1)
i2 = i2 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
i2 = i2 + ylab(costratio$variable)
i2 = i2 + xlab(finflowsnativestat$unit)
ggsave(file=paste(outdir,"/costratio_financialflows_native.png",sep=""),i2,width=20,height=12,dpi=200)

# per model
finflows$period<-as.numeric(as.character(finflows$period))
indicatorm=merge(finflows,costratio,by=c("implementation","regime","period","model"))
indicatorm$period<-as.factor(indicatorm$period)

for(mod in unique(indicatorm$model)){  
  i0 = ggplot(indicatorm[implementation=="flexibility"&period%in%c(2030,2050,2100)&model==mod])
  i0 = i0 + geom_point(aes(x=value.x,y=value.y,fill=regime,colour=regime,shape=period),size=5)
  i0 = i0 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
  i0 = i0 + geom_hline(aes(yintercept=1),size=1)
  i0 = i0 + geom_vline(aes(xintercept=100),size=1)
  i0 = i0 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
  i0 = i0 + ylab(costratio$variable)
  i0 = i0 + xlab(finflowsstat$unit)
  i0 = i0 + ggtitle(mod)
  ggsave(file=paste0(outdir,"/costratio_financialflows_",mod,".png"),i0,width=20,height=12,dpi=200)
}

finflowsnative$period<-as.numeric(as.character(finflowsnative$period))
indicatormn=merge(finflowsnative,costratio,by=c("implementation","regime","period","model"))
indicatormn$period<-as.factor(indicatormn$period)

for(mod in unique(indicatormn$model)){  
  i1 = ggplot(indicatormn[implementation=="flexibility"&period%in%c(2030,2050,2100)&model==mod])
  i1 = i1 + geom_point(aes(x=value.x,y=value.y,fill=regime,colour=regime,shape=period),size=5)
  i1 = i1 + scale_colour_manual(values=c("AP"="#003162","CO"="#b31b00","GF"="#b37400","PCC"="#4ed6ff"))
  i1 = i1 + geom_hline(aes(yintercept=1),size=1)
  i1 = i1 + geom_vline(aes(xintercept=100),size=1)
  i1 = i1 + theme_bw() + theme(axis.text=element_text(size=14),strip.text=element_text(size=14),legend.text = element_text(size=14),legend.title = element_text(size=16),axis.title = element_text(size=16))
  i1 = i1 + ylab(costratio$variable)
  i1 = i1 + xlab(finflowsnative$unit)
  i1 = i1 + ggtitle(mod)
  ggsave(file=paste0(outdir,"/costratio_financialflows_native",mod,".png"),i1,width=20,height=12,dpi=200)
}

# Socioeconomic impacts ---------------------------------------------------

# TODO the % change in GDP, the % change in private consumption  or a welfare indicator such as equivalent variation and maybe also a % change in employment.


# Technologies employed ---------------------------------------------------

#National models? TODO
