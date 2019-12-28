library(data.table) # setnames, nice view optio
library(dplyr)

all_tmp <- as.data.table(all_import_before_add_variables)
source("functions/calcPeak.R")
all_tmp <- calcPeak(all_tmp,'Emissions|CO2','Peak year|CO2')
source("functions/calcZeroEmissionsYear.R")
all_tmp <- calcZeroEmissionsYear(all_tmp,'Emissions|CO2','Zero Emissions Year|CO2')


# calculate Zero emissions year using Reshape package and dplyr package
all_zero_tmp1 <- filter(all_tmp, variable=='Zero Emissions Year|CO2')

all_zero_tmp2 <- filter(all_import_before_add_variables, variable=='Emissions|CO2', value < 0) %>%
            group_by(scenario,Category,Baseline,model,region,Scope,unit,variable) %>%
            filter(rank(period) == 1) 
all_zero_tmp2$variable <-"Zero Emissions Year|CO2"
all_zero_tmp2$value <- all_zero_tmp2$period
all_zero_tmp2$unit <- "Year" 
all_zero_tmp_compare <- left_join(all_zero_tmp1, all_zero_tmp2, by=c('Category', 'scenario', 'region', 'model', 'Baseline', 'Scope', 'variable')) %>%
                        mutate(period_diff=period.x-period.y)

# ONDERSTAANDE klopt nog niet !!!!!!!!!!!!
# calculate Peak year year using Reshape package and dplyr package
all_peak_tmp1 <- filter(all_tmp, variable=='Peak year|CO2')

all_peak_tmp2 <- filter(all_import_before_add_variables, variable=='Emissions|CO2') %>%
                 group_by(scenario,Category,Baseline,model,region,Scope,unit,variable) %>%
                 arrange(desc(value)) %>%
                 filter(rank(period) == 1) 
all_peak_tmp2$variable <-"Peak year|CO2"
all_peak_tmp2$value <- all_peak_tmp2$period
all_peak_tmp2$unit <- "Year" 
all_peak_tmp_compare <- left_join(all_peak_tmp1, all_peak_tmp2, by=c('Category', 'scenario', 'region', 'model', 'Baseline', 'Scope', 'variable')) %>%
                        mutate(period_diff=period.x-period.y)
