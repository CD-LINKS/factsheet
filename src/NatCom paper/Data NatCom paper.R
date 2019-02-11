# create CD-LINKS data
# set variables
config <- "config_WP2_3_indicators"
scencateg <- "scen_categ_V4"
variables <- "variables_xCut"

adjust <- "adjust_reporting_empty"
#memory.limit(size=20000)
source('load_data.R')
all_before_adj <- all
write.table(all_before_adj, "data/all_import_Mark.csv", sep=";", row.names = F)

# save variables in original all file
v <- unique(all_import$VARIABLE)
s <- unique(all_import$SCENARIO)
m <- unique(all_import$MODEL)
write.table(v, "data/import_variables.csv", sep=";", row.names=F)
write.table(s, "data/import_scenarios.csv", sep=";", row.names=F)
write.table(m, "data/import_models.csv", sep=";", row.names=F)

#memory.limit(size=20000)
adjust <- "adjust_reporting_indc_Mark"
source('load_data.R')
write.table(all, "data/all_Mark.csv", sep=";", row.names = F)

# Create historical data
source('functions/CreateHistoricalData.R')

