#############################################################
########## Main file that loads and processes data ##########
########## for x-Cut analyisis    ##########
#############################################################

library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")

#source configuration file for region-specific data
source("settings/config_xCut.R")

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for overwriting a dataframe for a subset of variables
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if _proc.rdata file is available
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load stock-taking data ##############
#############################################################

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,".Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,"_proc.Rdata"))
  Sys.sleep(2)#give everybody the chance to read the above message
} else {

  if (file.exists(paste0("data/",cfg$infile,".Rdata"))) {
    cat("Loading file", paste0("data/",cfg$infile,".Rdata"),"\n")
    load(paste0("data/",cfg$infile,".Rdata"))
  } else {
    cat("Reading data from file",paste0("data/",cfg$infile,".csv"),"\n")
    all <- invisible(fread(paste0("data/",cfg$infile,".csv"),header=TRUE))
    save("all",file = paste0("data/",cfg$infile,".Rdata"))
  }

  
  #reduce size of the data frame
  vars <- fread("settings/variables_xCut.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$regions]


  #####################################
  ####### Process stock-taking data ###
  #####################################
  cat("Processing stocktaking data\n")

  # Add information for new column "Category"
  scens <- fread("settings/scen_categ_cdlinks_xCut.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]


  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  # model specific adjustments
  source("adjust_reporting.R")

  #### add variables
  all <- add_variables(all,scens)

  #### manual changes after addition of variables
  #Change Category for AMPERE3-scenarios for GEM-E3 for regions where the results should feature (because this model has no LIMITS data)
  all[scenario == "MILES-AMPERE3-CF450" & model == "GEM-E3_V1" & region %in% c("BRA","EU")]$Category <- "Global 450 / S2-3"
  all[scenario == "MILES-AMPERE3-Base" & model == "GEM-E3_V1" & region %in% c("BRA","EU")]$Category <- "Baseline / S0-1"
  all[scenario == "MILES-AMPERE3-RefPol" & model == "GEM-E3_V1" & region %in% c("BRA","EU")]$Category <- "Reference"
  # categorize national models
  all[all$model %in% cfg$models_nat,]$model <- paste0("*",all[all$model %in% cfg$models_nat,]$model)
  nat_models <- paste0("*",cfg$models_nat)
  all[all$model %in% nat_models,]$Scope <- "national"
  
  # for the purpose of the cross cut analysis of model elasticities and behaviour, the R5REF region is ok for Russia
  all[model=="MESSAGE V.4" & region=="R5REF"]$region <- "RUS"
  all[model=="WITCH" & region=="R5REF"]$region <- "RUS"
  
  all[model=="WITCH" & region %in% c("RUS", "EU") & variable == "Price|Carbon"]$value <- NA
  
  
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))

}# end if-else: load and process stocktaking data

#############################################################
####################### Load diagnostics data  ##############
#############################################################

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$diag_infile,"_proc.Rdata")) & !b.procdata){
  cat("Loading processed data from file", paste0("data/",cfg$diag_infile,".Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$diag_infile,"_proc.Rdata"))
  Sys.sleep(2)#give everybody the chance to read the above message
} else {
  
  if (file.exists(paste0("data/",cfg$diag_infile,".Rdata"))) {
    cat("Loading file", paste0("data/",cfg$diag_infile,".Rdata"),"\n")
    load(paste0("data/",cfg$diag_infile,".Rdata"))
  } else {
    cat("Reading data from file",paste0("data/",cfg$diag_infile,".csv"),"\n")
    diag <- invisible(fread(paste0("data/",cfg$diag_infile,".csv"),header=TRUE))
    #re-factorize all character and numeric columns
    diag <- factor.data.frame(diag)
    save("diag",file = paste0("data/",cfg$diag_infile,".Rdata"))
  }
  
  #reduce size of the data frame
  vars <- fread("settings/variables_xCut.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  diag  <- diag[VARIABLE %in% vars$variable & REGION %in% cfg$regions]
  #get rid of unnecessary year columns
  source("functions/reduce_diag_t.R")
  diag <- reduce_diag_t(diag)
  
  ######################################
  ##### Process diagnostics data #######
  ######################################
  cat("Processing diagnostics data\n")
  


  
  # FIXME: use Emissions|CO2 as a proxy for Emission|CO2|FFI
 
  # Add information for new column "Category"
  diag_scens <- fread("settings/scen_categ_diag.csv", header=TRUE)
  #### from raw wide format to long format with additional columns
 
  diag <- process_data(diag,diag_scens)
 
  #re-factorize all character and numeric columns
  diag <- factor.data.frame(diag)
  
  
  tmp1 <- diag %>% filter( model == "COPPE-MSB_v1.3.2", variable == "Emissions|CO2")
  tmp1$variable ="Emissions|CO2|Energy and Industrial Processes"
  
  diag <-  overwrite(tmp1, diag)
  
  diag <- add_variables(diag,diag_scens)
  
  
  d_nat_models <- c("COPPE-MSB_v1.3.2")  #,"China TIMES","IPAC-AIM/technology V1.0","PRIMES_V1",
  #"AIM/Enduse[Japan]","DNE21+ V.14","GCAM4_MILES")
  
  #mark all national models with asterisk
  diag[diag$model %in% d_nat_models,]$model <- paste0("*",diag[diag$model %in% d_nat_models,]$model)
  d_nat_models <- paste0("*",d_nat_models)
  diag[diag$model %in% d_nat_models,]$Scope <- "national"
  
  save("diag",file = paste0("data/",cfg$diag_infile,"_proc.Rdata"))
} # end if-else: load and process diagnostics data


#############################################################
################## Do plots for cross-cut analysis ##########
#############################################################

source("cross_cut.R")
