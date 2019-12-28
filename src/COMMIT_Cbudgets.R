setwd("~/disks/local/factsheet/src")
config <-"config_COMMIT"
scencateg <- "scen_categ_COMMIT"
variables <- "variables_xCut"
adjust <- "adjust_reporting_COMMIT"
addvars <- F
datafile <-"commit_cd-links_compare_20191015-114544"
source("load_data.R")

#TODO fix this in load data - don't use the V5 as long it is not transferred and check all correct model names used?
all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("CD-LINKS-INDCi_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
  all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("CD-LINKS-INDCi_recGenTaxation_V4")]$SCENARIO,"CD-LINKS-INDCi_recGenTaxation_V4","CD-LINKS-INDCi_V4")
all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("CD-LINKS-INDC2030i_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
  all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("CD-LINKS-INDC2030i_1000_recGenTaxation_V4")]$SCENARIO,"CD-LINKS-INDC2030i_1000_recGenTaxation_V4","CD-LINKS-INDC2030i_1000_V4")
all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("CD-LINKS-NPi2020_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
  all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("CD-LINKS-NPi2020_1000_recGenTaxation_V4")]$SCENARIO,"CD-LINKS-NPi2020_1000_recGenTaxation_V4","CD-LINKS-NPi2020_1000_V4")
all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("CD-LINKS-NPi2020_400_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
  all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("CD-LINKS-NPi2020_400_recGenTaxation_V4")]$SCENARIO,"CD-LINKS-NPi2020_400_recGenTaxation_V4","CD-LINKS-NPi2020_400_V4")
# all[MODEL %in% c("GEM-E3")&SCENARIO=="CD-LINKS-NPi_V4"]$SCENARIO <- str_replace_all(all[MODEL %in% c("GEM-E3")&SCENARIO=="CD-LINKS-NPi_V4"]$SCENARIO,"_V4","_V4real")
# all[MODEL %in% c("GEM-E3")&SCENARIO=="CD-LINKS-NPi_V5"]$SCENARIO <- str_replace_all(all[MODEL %in% c("GEM-E3")&SCENARIO=="CD-LINKS-NPi_V5"]$SCENARIO,"_V5","_V4")


source("functions/calcBudget_2015.R")
all <- calcBudget_2015(all,'Emissions|CO2','Carbon budget_2015')
all <- calcBudget_2015(all,'Emissions|CO2|Energy and Industrial Processes','Carbon budget_2015|Energy and Industry')
all <- calcBudget_2015(all,'Emissions|CO2|Energy','Carbon budget_2015|Energy')

budgets = all[variable%in%c("Carbon budget_2015","Carbon budget_2015|Energy and Industry","Carbon budget_2015|Energy")&
                Category%in%c("2030_low","2020_low")&period%in%c(2050,2100)]
budgets = spread(budgets,variable,value)
write.csv(budgets,paste("COMMIT","/NationalCbudgets.csv",sep=""))
