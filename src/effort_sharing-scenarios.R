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

data=invisible(fread(paste0("data/","cdlinks_effort_sharing_compare_20190131-132310",".csv"),header=TRUE))
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

# Prepare data for use ----------------------------------------------------
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

data=data[region%in%c("World","JPN","BRA","CHN","EU","IND","RUS","USA")] #,"ARG","AUS","CAN","MEX","IDN","ROK","SAF","SAU","TUR",

# Initial allocation ------------------------------------------------------
allocation = data[variable=="Emissions|GHG|Allowance Allocation"]

a = ggplot(allocation[period%in%c(2050)])
a = a + geom_bar(stat="identity", aes(x=regime, y=value,fill=implementation),position="dodge")
a = a + facet_grid(model~region)
a = a + theme_bw()
a = a + ylab(allocation$unit)
ggsave(file=paste(outdir,"/Allowance allocation.png",sep=""),a,width=20,height=12,dpi=200)

# Trade ---------------------------------------------------------
#Value
finflow = data[variable=="Trade|Emissions Allowances|Value"]

f = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f = f + facet_grid(period~model)
f = f + theme_bw()
f = f + ylab(finflow$unit)
ggsave(file=paste(outdir,"/Trade-allowances-value.png",sep=""),f,width=20,height=12,dpi=200)

f1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Absolute"])
f1 = f1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f1 = f1 + facet_grid(period~model)
f1 = f1 + theme_bw()
f1 = f1 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Absolute"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-absolute.png",sep=""),f1,width=20,height=12,dpi=200)

f2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Exports"])
f2 = f2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f2 = f2 + facet_grid(period~model)
f2 = f2 + theme_bw()
f2 = f2 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-exports.png",sep=""),f2,width=20,height=12,dpi=200)

f3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Imports"])
f3 = f3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f3 = f3 + facet_grid(period~model)
f3 = f3 + theme_bw()
f3 = f3 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Imports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-imports.png",sep=""),f3,width=20,height=12,dpi=200)

f4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Value|Carbon|Net Exports"])
f4 = f4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f4 = f4 + facet_grid(period~model)
f4 = f4 + theme_bw()
f4 = f4 + ylab(data[variable=="Trade|Emissions|Value|Carbon|Net Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_value-net exports.png",sep=""),f4,width=20,height=12,dpi=200)

#Volume
trade = data[variable=="Trade|Emissions Allowances|Volume"]

t = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"])
t = t + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t = t + facet_grid(period~model)
t = t + theme_bw()
t = t + ylab(trade$unit)
ggsave(file=paste(outdir,"/Trade-allowances-volume.png",sep=""),t,width=20,height=12,dpi=200)

t1 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Absolute"])
t1 = t1 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t1 = t1 + facet_grid(period~model)
t1 = t1 + theme_bw()
t1 = t1 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Absolute"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-absolute.png",sep=""),t1,width=20,height=12,dpi=200)

t2 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Exports"])
t2 = t2 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t2 = t2 + facet_grid(period~model)
t2 = t2 + theme_bw()
t2 = t2 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-exports.png",sep=""),t2,width=20,height=12,dpi=200)

t3 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Imports"])
t3 = t3 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t3 = t3 + facet_grid(period~model)
t3 = t3 + theme_bw()
t3 = t3 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Imports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-imports.png",sep=""),t3,width=20,height=12,dpi=200)

t4 = ggplot(data[period%in%c(2030,2050,2100)&implementation=="flexibility"&variable=="Trade|Emissions|Volume|Carbon|Net Exports"])
t4 = t4 + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t4 = t4 + facet_grid(period~model)
t4 = t4 + theme_bw()
t4 = t4 + ylab(data[variable=="Trade|Emissions|Volume|Carbon|Net Exports"]$unit)
ggsave(file=paste(outdir,"/Trade-carbon_volume-net exports.png",sep=""),t4,width=20,height=12,dpi=200)

# Costs -------------------------------------------------------------------
# Carbon price
p = ggplot(data[variable=="Price|Carbon"])
p = p + geom_path(aes(x=period,y=value,colour=regime,linetype=model))
p = p + facet_grid(implementation~region)
p = p + theme_bw()
p = p + ylab(data[variable=="Price|Carbon"]$unit)
ggsave(file=paste(outdir,"/carbon price.png",sep=""),p,width=20,height=12,dpi=200)

# Policy costs as % of GDP (which policy cost indicator? Now one per model, but check with teams - TODO)
# Check who reports what
costvar = data[variable%in%c("Policy Cost|Welfare Change","Policy Cost|Additional Total Energy System Cost","Policy Cost|Area under MAC Curve","Policy Cost|Consumption Loss",
                           "Policy Cost|Equivalent Variation","Policy Cost|GDP Loss","Policy Cost|Other","Policy Cost|Default for CAV")]
costvar = costvar[,list(unique(variable)),by=c("model")]
#Other options: "Policy Cost|Welfare Change","Policy Cost|Equivalent Variation","Policy Cost|Other","Policy Cost|GDP Loss","Policy Cost|Area under MAC Curve"

# One per model, divide by GDP
costs = data[variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|Default for CAV","GDP|MER")] 
costs = costs[!c(model=="WITCH2016"&variable%in%c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss"))]
costs = spread(costs,variable,value) #to be fixed
costs = costs%>%mutate(ATESC=`Policy Cost|Additional Total Energy System Cost`/`GDP|MER`*100,Consloss=`Policy Cost|Consumption Loss`/`GDP|MER`*100,default=`Policy Cost|Default for CAV`/`GDP|MER`*100)
costs = gather(costs,variable,value,c("Policy Cost|Additional Total Energy System Cost","Policy Cost|Consumption Loss","Policy Cost|Default for CAV","GDP|MER",
                                      "ATESC",'Consloss',"default"))
costs = costs[variable%in%c("ATESC","Consloss","default")]
costs$unit <- '%'

c = ggplot(costs[implementation=="flexibility"])
c = c + geom_path(aes(x=period,y=value,colour=regime,linetype=model))
c = c + facet_grid(variable~region,scale="free_y")
c = c + theme_bw()
c = c + ylab(costs$unit)
  
c1 = ggplot(costs[implementation=="domestic"])
c1 = c1 + geom_path(aes(x=period,y=value,colour=regime,linetype=model))
c1 = c1 + facet_grid(variable~region,scale="free_y")
c1 = c1 + theme_bw()
c1 = c1 + ylab(costs$unit)


# and costs Annex I / non-Annex I, X 2030 & 2050, facet/dodge trade/no trade, fill regime
