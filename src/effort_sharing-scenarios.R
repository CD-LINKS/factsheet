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
#IMAGE reporting only for effort sharing variables, need to get GDP and emissions from NPi2020_1000 (only works if 'all' exists in workspace - by running load_data)
image=all[model=="IMAGE 3.0"&scenario=="NPi2020_1000_V4"]
image$Baseline<-NULL
image$Category<-NULL
image$Scope<-NULL
setcolorder(image,c("model","scenario","region","variable","unit","period","value"))
image1=image
image$scenario<-"NPi2020_1000_domestic_CO"
image1$scenario<-"NPi2020_1000_flexibility_CO"
data=rbind(data,image)
#IMAGE allowance allocation data mistakenly reported in Gt instead of Mt
data[model=="IMAGE 3.0"&variable=="Emissions|GHG|Allowance Allocation"]$value <- data[model=="IMAGE 3.0"&variable=="Emissions|GHG|Allowance Allocation"]$value*1000
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

data=data[region%in%c("World","JPN","BRA","CHN","EU","IND","RUS","USA")] #,"ARG","AUS","CAN","MEX","IDN","ROK","SAF","SAU","TUR",

# Initial allocation ------------------------------------------------------
allocation = data[variable=="Emissions|GHG|Allowance Allocation"&!region=="World"]

a = ggplot(allocation) #[period%in%c(2050)]
#a = a + geom_bar(stat="identity", aes(x=regime, y=value,fill=implementation),position="dodge")
a = a + geom_line(aes(x=period,y=value,colour=implementation,linetype=regime))
a = a + facet_grid(region~model,scales="free_y")
a = a + theme_bw()
a = a + ylab(allocation$unit)
ggsave(file=paste(outdir,"/Allowance allocation.png",sep=""),a,width=20,height=12,dpi=200)

# Emissions ---------------------------------------------------------------

# TODO Another indicator to look at is just emissions: first you can check how much the global profiles are still met in the flexibility cases, 
# and in the domestic scenarios, you might have lower global emissions due to hot air, or higher due to the infeasibilities in your model 
# (if I understand correctly, infeasibility in IMAGE means you hit the max allowable carbon price of 1200$/t CO2.
# TODO reductions relative to 2010! relative to baseline?
# TODO check cumulative emissions in line with carbon budgets?

e = ggplot(data[variable=="Emissions|Kyoto Gases"]) #&!region=="World"
e = e + geom_line(aes(x=period,y=value,colour=implementation,linetype=regime))
e = e + facet_grid(region~model,scales="free_y")
e = e + theme_bw()
e = e + ylab(data[variable=="Emissions|Kyoto Gases"]$unit)
ggsave(file=paste(outdir,"/GHGemissions.png",sep=""),e,width=20,height=12,dpi=200)


# Trade ---------------------------------------------------------
###Value
finflow = data[variable=="Trade|Emissions Allowances|Value"]

f = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f = f + facet_grid(period~model)
f = f + theme_bw()
f = f + ylab(finflow$unit)
ggsave(file=paste(outdir,"/Trade-allowances-value.png",sep=""),f,width=20,height=12,dpi=200)

# per model
for(mod in unique(finflow$model)){
  f0 = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"])
  f0 = f0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
  f0 = f0 + facet_grid(period~regime)
  f0 = f0 + theme_bw()
  f0 = f0 + ylab(finflow$unit)
  f0 = f0 + ggtitle(mod)
  ggsave(file=paste(outdir,"/Trade-allowances-value_",mod,".png",sep=""),f0,width=20,height=12,dpi=200)
}

#total financial flows
finflows = finflow[,list(sum(value)),by=c("model","variable","unit","period","implementation","regime")]
setnames(finflows,"V1","value")
finflows$variable<-"Total financial flows"
finflowsstat=finflows[,list(median=median(value,na.rm=T),mean=mean(value,na.rm=T),minq=quantile(value,prob=0.1,na.rm = T),maxq=quantile(value,prob=0.9,na.rm = T),
                            min=min(value,na.rm=T),max=max(value,na.rm=T)),by=c("variable","unit","period","implementation","regime")]

finflows$period<-as.factor(finflows$period)
f5 = ggplot(finflows[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f5 = f5 + geom_bar(stat="identity", aes(x=regime, y=value,fill=period),position="dodge")
f5 = f5 + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
f5 = f5 + facet_grid(model~.,scale="free_y")
f5 = f5 + theme_bw()
f5 = f5 + ylab(finflows$unit)
ggsave(file=paste(outdir,"/total_financial_flows_models.png",sep=""),f5,width=20,height=12,dpi=200)

finflowsstat$period<-as.factor(finflowsstat$period)
f5b = ggplot(finflowsstat[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f5b = f5b + geom_bar(stat="identity", aes(x=regime, y=median,fill=period),position=position_dodge(width=0.66),width=0.66)
f5b = f5b + geom_errorbar(aes(x=regime,ymin=min,ymax=max,colour=period),position=position_dodge(width=0.66),width=0.66)
f5b = f5b + scale_fill_manual(values=c("2030"="dark blue","2050"="light blue","2100"="grey"))
f5b = f5b + scale_colour_manual(values=c("2030"="black","2050"="black","2100"="black"))
f5b = f5b + theme_bw()
f5b = f5b + ylab(finflowsstat$unit)
ggsave(file=paste(outdir,"/total_financial_flows.png",sep=""),f5b,width=20,height=12,dpi=200)

# the other indicators
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

###Volume
trade = data[variable=="Trade|Emissions Allowances|Volume"]

t = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"])
t = t + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t = t + facet_grid(period~model)
t = t + theme_bw()
t = t + ylab(trade$unit)
ggsave(file=paste(outdir,"/Trade-allowances-volume.png",sep=""),t,width=20,height=12,dpi=200)

# per model
for(mod in unique(trade$model)){
  t0 = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"])
  t0 = t0 + geom_bar(stat="identity", aes(x=region, y=value,fill=region),position="dodge")
  t0 = t0 + facet_grid(period~regime)
  t0 = t0 + theme_bw()
  t0 = t0 + ylab(trade$unit)
  t0 = t0 + ggtitle(mod)
  ggsave(file=paste(outdir,"/Trade-allowances-volume_",mod,".png",sep=""),t0,width=20,height=12,dpi=200)
}

#the other indicators
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

price=data[variable=="Price|Carbon"]
price[model=="AIM/CGE[Japan]"]$model<-"AIM-CGE[Japan]"
price[model=="AIM/Enduse[Japan]"]$model<-"AIM-Enduse[Japan]"
for(mod in unique(price$model)){  
  p0 = ggplot(price[period%in%c(2030,2050,2100)]) #&implementation=="flexibility"
  p0 = p0 + geom_path(aes(x=period, y=value,colour=region))
  p0 = p0 + facet_grid(implementation~regime)
  p0 = p0 + theme_bw()
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

c = ggplot(costs[implementation=="flexibility"])
c = c + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable))
c = c + facet_grid(model~region,scale="free_y")
c = c + theme_bw()
c = c + ylab(costs$unit)
ggsave(file=paste(outdir,"/costs_GDP_flexibility.png",sep=""),c,width=20,height=12,dpi=200)
  
c1 = ggplot(costs[implementation=="domestic"])
c1 = c1 + geom_path(aes(x=period,y=value,colour=regime,linetype=costvariable))
c1 = c1 + facet_grid(model~region,scale="free_y")
c1 = c1 + theme_bw()
c1 = c1 + ylab(costs$unit)
ggsave(file=paste(outdir,"/costs_GDP_domestic.png",sep=""),c1,width=20,height=12,dpi=200)

# TODO and costs Annex I fraction GDP / fraction GDP non-Annex I, X 2030 & 2050, facet/dodge trade/no trade, fill regime. horizontal line at 1
# TODO and cost ratio vs. financial flows

# Socioeconomic impacts ---------------------------------------------------

# TODO the % change in GDP, the % change in private consumption  or a welfare indicator such as equivalent variation and maybe also a % change in employment.


# Technologies employed ---------------------------------------------------

#National models? TODO
