# create CD-LINKS data
# set variables
config <- "config_WP2_3_indicators"
scencateg <- "scen_categ_V4"
variables <- "variables_xCut"

adjust <- "adjust_reporting_empty"
memory.limit(size=20000)
source('load_data.R')
all_import <- all
write.table(all_import, "all_import_Mark.csv", sep=";", row.names = F)

memory.limit(size=20000)
adjust <- "adjust_reporting_indc_Mark"
source('load_data.R')
write.table(all, "all_Mark.csv", sep=";", row.names = F)
