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

data$implementation<-
data$regime<-

# Financial flows ---------------------------------------------------------
finflow = data[variable=="Trade|Emissions Allowances|Value"]

f = ggplot(finflow)
f = f + geom_bar(stat="identity", aes(x=region, y=value,fill=scenario))
f = f + facet_grid(~model)
f = f + theme_bw()
    
# Costs -------------------------------------------------------------------


