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

data=invisible(fread(paste0("data/","cdlinks_effort_sharing_compare_20190110-161346",".csv"),header=TRUE))
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

# Trade ---------------------------------------------------------
finflow = data[variable=="Trade|Emissions Allowances|Value"]

f = ggplot(finflow[period%in%c(2030,2050,2100)&implementation=="flexibility"])
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
f = f + facet_grid(period~model)
f = f + theme_bw()

trade = data[variable=="Trade|Emissions Allowances|Volume"]

t = ggplot(trade[period%in%c(2030,2050,2100)&implementation=="flexibility"])
t = t + geom_bar(stat="identity", aes(x=region, y=value,fill=regime),position="dodge")
t = t + facet_grid(period~model)
t = t + theme_bw()
    
#Also check:
# Emissions|GHG|Allowance Allocation	Mt CO2-equiv/yr
# Trade|Emissions Allowances|Volume	Mt CO2-equiv/yr
# Trade|Emissions Allowances|Value	billion US$2010/yr
# Trade|Emissions|Value|Carbon|Absolute	billion US$2010/yr
# Trade|Emissions|Value|Carbon|Exports	billion US$2010/yr
# Trade|Emissions|Value|Carbon|Imports	billion US$2010/yr
# Trade|Emissions|Value|Carbon|Net Exports	billion US$2010/yr
# Trade|Emissions|Volume|Carbon|Absolute	Mt CO2-equiv/yr
# Trade|Emissions|Volume|Carbon|Exports	Mt CO2-equiv/yr
# Trade|Emissions|Volume|Carbon|Imports	Mt CO2-equiv/yr
# Trade|Emissions|Volume|Carbon|Net Exports	Mt CO2-equiv/yr

# Costs -------------------------------------------------------------------

#as % of GDP

# and costs Annex I / non-Annex I, X 2030 & 2050, facet/dodge trade/no trade, fill regime
