# Data processing ---------------------------------------------------------
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
library(gridExtra) #arrangeGrob
library(xtable)
library(grid)
library(scales)
library(readxl)
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

#datafile <- "cdlinks_compare_20190403-172904" #--> draft sent to all authors
#datafile <- "cdlinks_compare_20190509-161701" #--> DID NOT WORK udpate after draft, new runs by IMAGE, WITCH
#datafile <- "cdlinks_compare_20190510-164650" #--> udpate after draft, new runs by IMAGE, WITCH
#datafile <- "cdlinks_compare_20190516-144819"#--> now including NoPolicy_V5 for WITCH2016
#datafile <- "cdlinks_compare_20190517-094157"# ---> NPi2020_1600_V4 was not included
datafile <- "cdlinks_compare_20190519-203202"# ---> NPi2020_1600_V5 was not included
Before_May_2019 = FALSE

#0. read file
data <- read.csv(paste0("data/", datafile, ".csv"))


# I. retrieve data before changes (adjust_reporting_indc_Mark)
if (keep_original == TRUE){
  cat("load original data\n")
  adjust <- "adjust_reporting_empty"
  rm(all);
  source('load_data_NatCom_paper.R')
  all_paper_before_adj <- all 
  all_paper_before_adj$period <- as.integer(all_paper_before_adj$period)
  # Change scenario names 
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NoPOL","No policy")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"INDC","NDC")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NPip","National policies planned")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"NPi","National policies")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2020_high","Carbon budget 1600")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2020_low","Carbon budget 1000")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2020_verylow","Carbon budget 400")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2030_low","Carbon budget 1000 (2030)")
  all_paper_before_adj$Category=str_replace_all(all_paper_before_adj$Category,"2030_high","Carbon budget 1600 (2030)")
  write.table(all_paper_before_adj, "data/all_before_Mark.csv", sep=";", row.names = F)

  # save variables in original all file
  v <- unique(all_import$VARIABLE) %>% as.data.frame()
  colnames(v) <- c('variable')
  s <- unique(all_import$SCENARIO)
  m <- unique(all_import$MODEL)
  write.table(v, paste0("data/import_variables_", datafile, ".csv"), sep=";", row.names=F)
  write.table(s, paste0("data/import_scenarios_", datafile, ".csv"), sep=";", row.names=F)
  write.table(m, paste0("data/import_models_", datafile, ".csv"), sep=";", row.names=F)
  write.table(unique(all_import[,c('SCENARIO','MODEL')]), paste0("data/import_original_scenario_models_", datafile, ".csv"), sep=";", row.names=F)
  write.table(unique(all_paper_before_adj[,c('scenario','model')]), paste0("data/import_scenario_models_", datafile, ".csv"), sep=";", row.names=F)
}
all_paper_before_adj$period <- as.integer(all_paper_before_adj$period)
assign(paste0("all_paper_before_adj_", datafile), all_paper_before_adj)

# II. retrieve data after changes (adjust_reporting_indc_Mark)
cat("load adjusted data\n")
adjust <- "adjust_reporting_indc_Mark"
rm(all); rm(all_paper)
source('load_data_NatCom_paper.R')
all_paper <- all
all_paper$Category=str_replace_all(all_paper$Category,"NoPOL","No new policies")
all_paper$Category=str_replace_all(all_paper$Category,"INDC","NDC")
all_paper$Category=str_replace_all(all_paper$Category,"NPip","National policies planned")
all_paper$Category=str_replace_all(all_paper$Category,"NPi","National policies")
all_paper$Category=str_replace_all(all_paper$Category,"2020_high","Carbon budget 1600")
all_paper$Category=str_replace_all(all_paper$Category,"2020_low","Carbon budget 1000")
all_paper$Category=str_replace_all(all_paper$Category,"2020_verylow","Carbon budget 400")
all_paper$Category=str_replace_all(all_paper$Category,"2030_low","Carbon budget 1000 (2030)")
all_paper$Category=str_replace_all(all_paper$Category,"2030_high","Carbon budget 1600 (2030)")
all_paper$period <- as.integer(all_paper$period)
all_paper$model=str_replace_all(all_paper$model,"\\*COPPE-MSB_v2.0","*BLUES")
write.table(all_paper, "NatComPaper/data/all_paper.csv", sep=";", row.names = F)

all_paper_emissions <- filter(all_paper, grepl("Emissions\\|", variable))
write.table(all_paper_emissions, "NatComPaper/data/all_paper_emissions.csv", sep=";", row.names = F)

all_paper_emissions_IMAGE <- filter(all_paper_emissions, model=="IMAGE 3.0")
write.table(all_paper_emissions_IMAGE, "NatComPaper/data/all_paper_emissions_IMAGE.csv", sep=";", row.names = F)

all_paper_energy <- filter(all_paper, grepl("Energy\\|", variable))
write.table(all_paper_energy, "NatComPaper/data/all_paper_energy.csv", sep=";", row.names = F)

all_paper_energy_IMAGE <- filter(all_paper_energy, model=="IMAGE 3.0")
write.table(all_paper_energy_IMAGE, "NatComPaper/data/all_paper_energy_IMAGE.csv", sep=";", row.names = F)

all_paper_final_energy <- filter(all_paper, grepl("Final Energy\\|", variable))
write.table(all_paper_energy, "NatComPaper/data/all_paper_final_energy.csv", sep=";", row.names = F)
all_paper_secondary_energy <- filter(all_paper, grepl("Secondary Energy\\|", variable))
write.table(all_paper_energy, "NatComPaper/data/all_paper_secondary_energy.csv", sep=";", row.names = F)

# additional adjustments
#all_paper<-all_paper[model!="GEM-E3_EU"]
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
assign(paste0("all_paper_", datafile), all_paper)

# III. Retrieve historical data
rm(all_hist); rm(all_hist_paper)
source('functions/CreateHistoricalData.R')
all_hist_paper <- all_hist
all_hist_paper$period <- as.integer(all_hist_paper$period)
all_hist_paper$period <- as.integer(all_hist_paper$period)
write.table(all_hist_paper, "data/all_hist_paper.csv", sep=";", row.names = F)

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
NPi_update <- ImportTimerScenario('NPi_update','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDCi <- ImportTimerScenario('INDCi','INDCi', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1600 <- ImportTimerScenario('NPi2020_1600','NPi2020_1600', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_400 <- ImportTimerScenario('NPi2020_400','NPi2020_400', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDC2030i_1000 <- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)

NoPolicy_indicators <- ProcessTimerScenario(NoPolicy, Rundir, Project, Policy=TRUE)
NPi_indicators <- ProcessTimerScenario(NPi, Rundir, Project, Policy=TRUE)
NPi_update_indicators <- ProcessTimerScenario(NPi_update, Rundir, Project, Policy=TRUE)
INDCi_indicators <- ProcessTimerScenario(INDCi, Rundir, Project, Policy=TRUE)
NPi2020_1600_indicators <- ProcessTimerScenario(NPi2020_1600, Rundir, Project, Policy=TRUE)
NPi2020_1000_indicators <- ProcessTimerScenario(NPi2020_1000, Rundir, Project, Policy=TRUE)
NPi2020_400_indicators <- ProcessTimerScenario(NPi2020_400, Rundir, Project, Policy=TRUE)
INDC2030i_1000_indicators <- ProcessTimerScenario(INDC2030i_1000, Rundir, Project, Policy=TRUE)

# old versions (with forecast)
NPi_V4 <- ImportTimerScenario('NPi_V4','NPi', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDCi_V4 <- ImportTimerScenario('INDCi_V4','INDCi', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi2020_1000_V4 <- ImportTimerScenario('NPi2020_1000_V4','NPi2020_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
INDC2030i_1000_V4 <- ImportTimerScenario('INDC2030i_1000_V4','INDC2030i_1000', Rundir, Project, TIMERGeneration, Policy=TRUE)
NPi_V4_indicators <- ProcessTimerScenario(NPi_V4, Rundir, Project, Policy=TRUE)
INDCi_V4_indicators <- ProcessTimerScenario(INDCi_V4, Rundir, Project, Policy=TRUE)
NPi2020_1000_V4_indicators <- ProcessTimerScenario(NPi2020_1000_V4, Rundir, Project, Policy=TRUE)
INDC2030i_1000_V4_indicators <- ProcessTimerScenario(INDC2030i_1000_V4, Rundir, Project, Policy=TRUE)

#NoPolicy_V4 <- ImportTimerScenario('NoPolicy_V4','NoPolicy', Rundir, Project, TIMERGeneration, Policy=FALSE)
#NPi_V4 <- ImportTimerScenario('NPi_V4','NPi', Rundir, Project, TIMERGeneration, Policy=FALSE)
#INDCi_V4 <- ImportTimerScenario('INDCi_V4','INDCi', Rundir, Project, TIMERGeneration, Policy=FALSE)

#NoPolicy_V4_indicators <- ProcessTimerScenario(NoPolicy_V4, Rundir, Project, Policy=FALSE)
#NPi_V4_indicators <- ProcessTimerScenario(NPi_V4, Rundir, Project, Policy=FALSE)
#INDCi_V4_indicators <- ProcessTimerScenario(INDCi_V4, Rundir, Project, Policy=FALSE)

setwd(currentdir)

keep_original <- TRUE
# IV. retrieve data for SSP1, SSP2, SSP3 (NoPolicy, NPi, INDCi)
if (keep_original == TRUE){
  cat("load SSP data\n")
  adjust <- "adjust_reporting_empty"
  source('load_data_NatCom_paper.R')
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
