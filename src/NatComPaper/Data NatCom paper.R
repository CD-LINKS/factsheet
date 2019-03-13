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

# I. retrieve data before changes (adjust_reporting_indc_Mark)
if (keep_original == TRUE){
  cat("load original data\n")
  adjust <- "adjust_reporting_empty"
  source('load_data.R')
  all_paper_before_adj <- all 
  all_paper_before_adj$period <- as.integer(all_paper_before_adj$period)
  # Change scenario names 
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NoPOL","No policy")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"INDC","NDC")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NPip","National policies planned")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NPi","National policies")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2020_low","Carbon budget 1000")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2020_verylow","Carbon budget 400")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2030_low","Carbon budget 1000 (2030)")
  write.table(all_before_adj, "data/all_before_Mark.csv", sep=";", row.names = F)


  # save variables in original all file
  v <- unique(all_import$VARIABLE)
  s <- unique(all_import$SCENARIO)
  m <- unique(all_import$MODEL)
  write.table(v, "data/import_variables.csv", sep=";", row.names=F)
  write.table(s, "data/import_scenarios.csv", sep=";", row.names=F)
  write.table(m, "data/import_models.csv", sep=";", row.names=F)
}

# II. retrieve data after changes (adjust_reporting_indc_Mark)
cat("load adjusted data\n")
adjust <- "adjust_reporting_indc_Mark"
rm(all); rm(all_paper)
source('load_data.R')
all_paper <- all
all_paper$Category=str_replace_all(all_paper$Category,"NoPOL","No policy")
all_paper$Category=str_replace_all(all_paper$Category,"INDC","NDC")
all_paper$Category=str_replace_all(all_paper$Category,"NPip","National policies planned")
all_paper$Category=str_replace_all(all_paper$Category,"NPi","National policies")
all_paper$Category=str_replace_all(all_paper$Category,"2020_low","Carbon budget 1000")
all_paper$Category=str_replace_all(all_paper$Category,"2020_verylow","Carbon budget 400")
all_paper$Category=str_replace_all(all_paper$Category,"2030_low","Carbon budget 1000 (2030)")
all_paper$period <- as.integer(all_paper$period)
write.table(all_paper, "data/all_paper.csv", sep=";", row.names = F)
# III. Retrieve historical data
rm(all_hist); rm(all_hist_paper)
source('functions/CreateHistoricalData.R')
all_hist_paper <- all_hist
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"NoPOL","No policy")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"INDC","NDC")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"NPip","National policies planned")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"NPi","National policies")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"2020_low","Carbon budget 1000")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"2020_verylow","Carbon budget 400")
all_hist_paper$Category=str_replace_all(all_hist_paper$Category,"2030_low","Carbon budget 1000 (2030)")
all_hist_paper$period <- as.integer(all_hist_paper$period)
#write.table(all_hist_paper, "data/all_hist_paper.csv", sep=";", row.names = F)

# data from IMAGE
currentdir <- getwd()
setwd("~/disks/y/Kennisbasis/IMAGE/model/Timer/ontwapps_Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet")
Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/Timer/ontwapps_Timer/Users/Mathijs/Projects/CD-LINKS", sep="")
Project=paste("CD-LINKS")
TIMERGeneration = 'TIMER_2015'
# Source scripts (after setting working directory)
source('../TIMER_output/functions/Settings.R')
source('../TIMER_output/functions/General Functions.R')
source('../TIMER_output/functions/Import_TIMER_output.R')
source('../TIMER_output/functions/Process_TIMER_output.R')
# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy','NoPolicy', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi <- ImportTimerScenario('NPi','NPi', Rundir, Project, TIMERGeneration, Policy=FALSE)
INDCi <- ImportTimerScenario('INDCi','INDCi', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000', Rundir, Project, TIMERGeneration, Policy=FALSE)
INDC2030i_1000 <- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000', Rundir, Project, TIMERGeneration, Policy=FALSE)
setwd(currentdir)

# paper data
emissions <- c("Emissions|Kyoto Gases", "Emissions|CO2", 
               "Emissions|CO2|Energy", "Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|AFOLU", "Emissions|CH4","Emissions|N2O","Emissions|F-Gases",
               "Emissions|CO2|Energy|Demand",
               "Emissions|CO2|Energy|Supply",
               "Emissions|CO2|Energy|Demand|Transportation",
               "Emissions|CO2|Energy|Demand|Residential and Commercial",
               "Emissions|CO2|Industry",
               "Emissions|CO2|AFOLU",
               "Emissions|Non-CO2")
final_energy <- c("Final Energy", "Final Energy|Transportation", "Final Energy|Residential and Commercial", "Final Energy|Industry",
                  "Final Energy|Non-fossil share","Final Energy|Transportation|Non-fossil share","Final Energy|Residential and Commercial|Non-fossil share","Final Energy|Industry|Non-fossil share")
kaya <- c("Energy intensity of GDP","Conversion efficiency", "Carbon intensity of fossil-fuel use","Fossil fuel utilisation rate","Energy utilisation rate",
          "Primary Energy", "Primary Energy|Fossil", "Final Energy|Fossil", "GDP|MER")
nf_share <- c("Final Energy|Solids|Biomass", "Final Energy|Solids|Biomass|Traditional",
              "Final Energy|Transportation|Liquids|Biomass",
              "Final Energy|Solar", "Final Energy|Wind", "Final Energy|Geothermal")
red <- c("Emissions|Kyoto Gases|rel2010")

vars_natcom <- c(emissions, final_energy, kaya)
regs_natcom <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "Bunkers")
scens_natcom <- c("No policy", "National policies", "NDC", "Carbon budget 1000", "Carbon budget 400")
results_nactcom <- filter(all_paper, variable%in%vars_natcom, 
                          region%in%regs_natcom,
                          Category%in%scens_natcom,
                          Scope=="global",
                          period>=2010)
write.table(results_nactcom, "NatComPaper/data/data_NatCOM.csv", sep=";", row.names=F)
results_natcom_stat <- group_by(results_nactcom, Category, region, period, variable) %>% summarise(average=mean(value, na.rm=T),
                                                                                                   median=median(value, na.rm=T), 
                                                                                                   perc_10=quantile(value,0.1, na.rm=T),
                                                                                                   perc_90=quantile(value,0.9, na.rm=T),
                                                                                                   min=min(value),
                                                                                                   max=max(value))
write.table(results_natcom_stat, "NatComPaper/data/data_NatCOM_stat.csv", sep=";", row.names=F)

