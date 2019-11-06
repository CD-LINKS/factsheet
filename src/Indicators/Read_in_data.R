# This R-script creates CD-LINKS data based on csv file from IIASA database
# It makes two versions: 1) before model adjustments (all_bfore_adj) in R, 2) after model adjustments in R (all_paper)
# Then it creates historical data for GHG emissions, Energy use and GPD based in PRIMAP, EDGAR (bunkers), IEA and OECD data
# Scenario names are renamed for NatCom paper

# create CD-LINKS data
# set variables
config <- "config_WP2_3_indicators"
scencateg <- "scen_categ_V4"
variables <- "variables_xCut"
addvars <- TRUE
keep_original <- FALSE
#datafile <- "cdlinks_compare_20190403-172904" #--> draft sent to all authors
datafile <- "cdlinks_compare_20190509-161701" #--> udpate after draft, new runs by IMAGE, WITCH
datafile <- "cdlinks_compare_20190510-164650" #--> udpate after draft, new runs by IMAGE, WITCH
before_May_2019 = FALSE

# I. retrieve data before changes (adjust_reporting_indc_Mark)
if (keep_original == FALSE){
  cat("load original data\n")
  adjust <- "adjust_reporting_indc_Mark"
  rm(all);
  source('load_data_NatCom_paper.R')
  all_indicators <- all 
  all_indicators$period <- as.integer(all_indicators$period)
  # Change scenario names 
  all_indicators$Category=str_replace_all(all_indicators$Category,"NoPOL","No policy")
  all_indicators$Category=str_replace_all(all_indicators$Category,"INDC","NDC")
  all_indicators$Category=str_replace_all(all_indicators$Category,"NPip","National policies planned")
  all_indicators$Category=str_replace_all(all_indicators$Category,"NPi","National policies")
  all_indicators$Category=str_replace_all(all_indicators$Category,"2020_low","Carbon budget 1000")
  all_indicators$Category=str_replace_all(all_indicators$Category,"2020_verylow","Carbon budget 400")
  all_indicators$Category=str_replace_all(all_indicators$Category,"2030_low","Carbon budget 1000 (2030)")
  write.table(all_indicators, "data/all_indicators.csv", sep=";", row.names = F)
}
all_indicators$period <- as.integer(all_indicators$period)


# data from IMAGE
currentdir <- getwd()
setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mathijs/Projects/CD-LINKS/CD-LINKS/6_R/TIMER_output")
Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/users/mathijs/Projects/CD-LINKS", sep="")
Project=paste("CD-LINKS")
TIMERGeneration = 'TIMER_2015'
# Source scripts (after setting working directory)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')
# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDCi <- ImportTimerScenario('INDCi','INDCi', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1600 <- ImportTimerScenario('NPi2020_1600','NPi2020_1600', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_400 <- ImportTimerScenario('NPi2020_400','NPi2020_400', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDC2030i_1000 <- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)

NoPolicy_indicators <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=TRUE)
NPi_indicators <- ProcessTimerScenario(NPi, Rundir, Project, Policy=TRUE)
INDCi_indicators <- ProcessTimerScenario(INDCi, Rundir, Project, Policy=TRUE)
NPi2020_1600_indicators <- ProcessTimerScenario(NPi2020_1600, Rundir, Project, Policy=TRUE)
NPi2020_1000_indicators <- ProcessTimerScenario(NPi2020_1000, Rundir, Project, Policy=TRUE)
NPi2020_400_indicators <- ProcessTimerScenario(NPi2020_400, Rundir, Project, Policy=TRUE)
INDC2030i_1000_indicators <- ProcessTimerScenario(INDC2030i_1000, Rundir, Project, Policy=TRUE)

setwd(currentdir)