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
keep_original <- TRUE

# I. retrieve data before changes (adjust_reporting_indc_Mark)
if (keep_original == TRUE){
  cat("load original data\n")
  adjust <- "adjust_reporting_empty"
  rm(all);
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
  write.table(all_paper_before_adj, "data/all_before_Mark.csv", sep=";", row.names = F)


  # save variables in original all file
  v <- unique(all_import$VARIABLE) %>% as.data.frame()
  colnames(v) <- c('variable')
  s <- unique(all_import$SCENARIO)
  m <- unique(all_import$MODEL)
  write.table(v, "data/import_variables.csv", sep=";", row.names=F)
  write.table(s, "data/import_scenarios.csv", sep=";", row.names=F)
  write.table(m, "data/import_models.csv", sep=";", row.names=F)
}
all_paper_before_adj$period <- as.integer(all_paper_before_adj$period)

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
#write.table(all_paper, "data/all_paper.csv", sep=";", row.names = F)

# additional adjustments
all_paper<-all_paper[model!="GEM-E3_EU"]
all_paper<-all_paper[model!="AIM/CGE"]
all_paper<-all_paper[!(model=="COPPE-COFFEE 1.0" & region=="EU")]
all_paper<-all_paper[!(model=="MESSAGEix-GLOBIOM_1.0" & region=="EU")]
all_paper<-all_paper[!(model=="MESSAGEix-GLOBIOM_1.0" & region=="USA")]
all_paper<-all_paper[!(model=="MESSAGEix-GLOBIOM_1.1")]

all_paper1 <- all_paper

regs_paper <- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
vars_RoW <- c("Emissions|Kyoto Gases", "Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|AFOLU", 
              "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases", 
              "Emissions|CO2|Energy", "Emissions|CO2|Industrial Processes", "Emissions|CO2|Energy and Industrial Processes",
              "Secondary Energy|Electricity",
              "Final Energy", "Final Energy|Other",  
              "Final Energy|Residential and Commercial", 
              "Final Energy|Transportation",  
              "Final Energy|Industry",
              "GDP|MER", "Population")
# calcualte RoW
all_tmp <- filter(all_paper, region%in%regs_paper, variable%in%vars_RoW, period>=2010, period<=2050, Scope=="global") %>% as.data.frame()
all_tmp$value <- -1*all_tmp$value
all_tmp_world <- filter(all_paper, region%in%c('World'), variable%in%vars_RoW, period>=2010, period<=2050, Scope=="global") %>% as.data.frame()
all_tmp <- rbind(all_tmp, all_tmp_world)
all_tmp <- spread(all_tmp, key=region, value=value) %>%
           rowwise() %>%
           mutate(RoW = sum(c(`World`, `BRA`,`CHN`, `EU`,`IND`, `JPN`, `RUS`, `USA`), na.rm = TRUE)) %>%
           mutate(region="RoW") %>%
           select(scenario, Category, Baseline, model, region, period, Scope, unit, variable, RoW) %>%
           rename(value=RoW) %>%
           as.data.frame()
all_paper <- rbind(all_paper, all_tmp)
all_paper$period <- as.integer(all_paper$period)

# III. Retrieve historical data
rm(all_hist); rm(all_hist_paper)
source('functions/CreateHistoricalData.R')
all_hist_paper <- all_hist
all_hist_paper$period <- as.integer(all_hist_paper$period)
all_hist_paper$period <- as.integer(all_hist_paper$period)
write.table(all_hist_paper, "data/all_hist_paper.csv", sep=";", row.names = F)

keep_original <- TRUE
# IV. retrieve data for SSP1, SSP2, SSP3 (NoPolicy, NPi, INDCi)
if (keep_original == TRUE){
  cat("load SSP data\n")
  adjust <- "adjust_reporting_empty"
  source('load_data.R')
  all_import_SSP <- all 
  all_import_SSP$period <- as.integer(all_import_SSP$period)
  # Change scenario names 
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"NoPOL","No policy")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"INDC","NDC")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"NPip","National policies planned")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"NPi","National policies")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"2020_low","Carbon budget 1000")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"2020_verylow","Carbon budget 400")
  all_import_SSP$Category=str_replace_all(all_import_SSP$Category,"2030_low","Carbon budget 1000 (2030)")
  write.table(all_import_SSP, "data/all_SSP.csv", sep=";", row.names = F)
}
scens_SSP1 <- c("NoPolicy_SSP1_V4", "NPi_SSP1_V4", "INDCi_SSP1_V4")
scens_SSP2 <- c("NoPolicy_V4",  "NPi_V4","INDCi_V4")
scens_SSP3 <- c("INDCi_SSP3_V4","NPi_SSP3_V4","NoPolicy_SSP3_V4")

all_paper_SSP <- all_import
all_paper_SSP <- gather(all_paper_SSP, 6:ncol(all_paper_SSP), key="period", value=value) 
all_paper_SSP <- filter(all_paper_SSP, !(is.na(value)))
setnames(all_paper_SSP, "MODEL", "model")
setnames(all_paper_SSP, "SCENARIO", "scenario")
setnames(all_paper_SSP, "REGION", "region")
setnames(all_paper_SSP, "VARIABLE", "variable")
setnames(all_paper_SSP, "UNIT", "unit")
all_paper_SSP <- filter(all_paper_SSP, scenario%in%c(scens_SSP1, scens_SSP2, scens_SSP3))
all_paper_SSP <- mutate(all_paper_SSP, SSP=ifelse(scenario%in%scens_SSP1, "SSP1", ifelse(scenario%in%scens_SSP2,"SSP2", "SSP3")))
all_paper_SSP <- mutate(all_paper_SSP, Category=str_extract(scenario, "[^_]+"))
all_paper_SSP$period <- as.integer(all_paper_SSP$period)

# data from IMAGE
currentdir <- getwd()
setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet")
Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/users/mathijs/Projects/CD-LINKS", sep="")
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

NoPolicy_indicators <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=FALSE)
NPi_indicators <- ProcessTimerScenario(NPi, Rundir, Project, Policy=FALSE)
INDCi_indicators <- ProcessTimerScenario(INDCi, Rundir, Project, Policy=FALSE)

NoPolicy_V4 <- ImportTimerScenario('NoPolicy_V4','NoPolicy', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi_V4 <- ImportTimerScenario('NPi_V4','NPi', Rundir, Project, TIMERGeneration, Policy=FALSE)
INDCi_V4 <- ImportTimerScenario('INDCi_V4','INDCi', Rundir, Project, TIMERGeneration, Policy=FALSE)

NoPolicy_V4_indicators <- ProcessTimerScenario(NoPolicy_V4, Rundir, Project, Policy=FALSE)
NPi_V4_indicators <- ProcessTimerScenario(NPi_V4, Rundir, Project, Policy=FALSE)
INDCi_V4_indicators <- ProcessTimerScenario(INDCi_V4, Rundir, Project, Policy=FALSE)

setwd(currentdir)

# updated CD-LINKS runs
currentdir <- getwd()
setwd("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer/CD_LINKSupdate/6_R/factsheet")
Rundir=paste("~/disks/y/Kennisbasis/IMAGE/model/users/mark/timer", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'
# Read no policy scenario
NoPolicy_update <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi_update <- ImportTimerScenario('NPi_update','NPi_update', Rundir, Project, TIMERGeneration, Policy=FALSE)
NPi_update_indicators <- ProcessTimerScenario(NPi_update, Rundir, Project, Policy=FALSE)
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
                          period>=2010, period<=2050)
write.table(results_nactcom, "NatComPaper/data/data_NatCOM.csv", sep=";", row.names=F)
results_natcom_stat <- group_by(results_nactcom, Category, region, period, variable) %>% summarise(average=mean(value, na.rm=T),
                                                                                                   median=median(value, na.rm=T), 
                                                                                                   perc_10=quantile(value,0.1, na.rm=T),
                                                                                                   perc_90=quantile(value,0.9, na.rm=T),
                                                                                                   min=min(value, na.rm=T),
                                                                                                   max=max(value, na.rm=T))
write.table(results_natcom_stat, "NatComPaper/data/data_NatCOM_stat.csv", sep=";", row.names=F)

