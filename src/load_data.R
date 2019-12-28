#############################################################
########## Main file that loads and processes data ##########
########## and produces the national fact sheet    ##########
#############################################################

# INPUT needed: variables (your settings/variables csv file), scencateg (your settings/scenario categorisation file), 
# config (your settings/configuration file), adjust (your adjust_reporting file)

library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all
#library(tidyverse)

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("~/disks/local/factsheet/src")

if(!exists("variables")){
  stop("Please specify 'variables', i.e. the name of the settings/variables_....csv file you want to use (without the extension)")
}
if(!exists("config")){
  stop("Please specify 'config', i.e. the name of the settings/config_....R file you want to use (without the extension)")
}
if(!exists("scencateg")){
  stop("Please specify 'scencateg', i.e. the name of the settings/scenario categorisation_....csv file you want to use (without the extension)")
}
if(!exists("adjust")){
  stop("Please specify 'adjust', i.e. the name of the src/adjust_reporting_....R file you want to use (without the extension)")
}

if(!exists("addvars")){
  stop("Please set flag 'addvars' to TRUE or FALSE, i.e. whether you want to add calculated indicators from reported variables (takes some time)")
}

#source configuration file for region-specific data
source(paste("settings/",config,".R",sep=""))

#overwrite file to be used for analysis
cfg$infile<-datafile

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for rbinding a dataframe with another, removing duplicates
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if .._reg_proc.RData file is available (i.e. overwrite existing RData)
# set to true if you always want data re-processed
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

# keep old version of data
if (exists('all')) {all_old <- all}

#############################################################
####################### Load data ###########################
#############################################################
if(length(cfg$r)>0){reg=""}else{reg=paste0("_",cfg$r)}
#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, please set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,reg,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,".Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,reg,"_proc.Rdata"))
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

  # keep original data
  all_import <- all  
  
  # Add new column "Category" and fill with name according to scenario-to-Category-mapping in "scens"
  scens <- fread(paste("settings/",scencateg,".csv",sep=""), header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]
  
  #reduce size of the data frame
  vars <- fread(paste("settings/",variables,".csv",sep=""),header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$r]
  

  #############################################################
  ####################### Process data ########################
  #############################################################
  cat("Processing data\n")
  
  # Change scenario names for some models to V4 to not mix up old global model results with new ones, while using latest version of other models (V3)
  # Special case for RU_TIMES: no V3/V4 at all
  all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO,'_V4',sep="")
  # Special case for GEM-E3: select the recGenTaxation scenarios for standard analysis
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO,"INDCi_recGenTaxation_V4","INDCi_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO,"INDC2030i_1000_recGenTaxation_V4","INDC2030i_1000_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO,"NPi2020_1000_recGenTaxation_V4","NPi2020_1000_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO,"NPi2020_400_recGenTaxation_V4","NPi2020_400_V4")
  
  # COPPE-MSB --> skip V4 as these are not used, use V3. This works with current implementation
  
  # Special case for IMAGE and GEM-E3 (NPi only): for the effort sharing analysis use V5 (label as V4) because that one has 10 region level data (re-imported by Peter) - later use V5 for all models?
  # Use V5 for WITCH2016 and IMAGE 3.0 and AIM V2.1
  all[MODEL %in% c("WITCH2016")]$SCENARIO <- str_replace_all(all[MODEL %in% c("WITCH2016")]$SCENARIO,"_V4","_V4real")
  all[MODEL %in% c("WITCH2016")]$SCENARIO <- str_replace_all(all[MODEL %in% c("WITCH2016")]$SCENARIO,"_V5","_V4")
  all[MODEL %in% c("IMAGE 3.0")]$SCENARIO <- str_replace_all(all[MODEL %in% c("IMAGE 3.0")]$SCENARIO,"_V4","_V4real")
  all[MODEL %in% c("IMAGE 3.0")]$SCENARIO <- str_replace_all(all[MODEL %in% c("IMAGE 3.0")]$SCENARIO,"_V5","_V4")
  all[MODEL %in% c("AIM V2.1")]$SCENARIO <- str_replace_all(all[MODEL %in% c("AIM V2.1")]$SCENARIO,"_V4","_V4real")
  all[MODEL %in% c("AIM V2.1")]$SCENARIO <- str_replace_all(all[MODEL %in% c("AIM V2.1")]$SCENARIO,"_V5","_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO=="NPi_V4"]$SCENARIO <- str_replace_all(all[MODEL %in% c("GEM-E3")&SCENARIO=="NPi_V4"]$SCENARIO,"_V4","_V4real")
  all[MODEL %in% c("GEM-E3")&SCENARIO=="NPi_V5"]$SCENARIO <- str_replace_all(all[MODEL %in% c("GEM-E3")&SCENARIO=="NPi_V5"]$SCENARIO,"_V5","_V4")
  # Rest: use latest version V3 for national (and some global) models, rename to V4
  all[MODEL %in% c("AIM/Enduse[Japan]","China TIMES","COPPE-COFFEE 1.0","DNE21+ V.14 (national)","GCAM-USA_CDLINKS","IPAC-AIM/technology V1.0","PRIMES_V1","REMIND-MAgPIE 1.7-3.0")]$SCENARIO <- str_replace_all(
  all[MODEL %in% c("AIM/Enduse[Japan]","China TIMES","COPPE-COFFEE 1.0","DNE21+ V.14 (national)","GCAM-USA_CDLINKS","IPAC-AIM/technology V1.0","PRIMES_V1","REMIND-MAgPIE 1.7-3.0")]$SCENARIO,"_V3","_V4")

  # Exclude Globiom and Magpie (used for food security analysis only), and AIM/CGE (newest scenarios are under AIM V2.1)
  all <- all[!MODEL%in%c("MAgPIE 3.0","GLOBIOM 1.0","AIM/CGE")]
  # GEM-E3 used as global and national model
  gem <- all[MODEL=="GEM-E3"&REGION=="EU"]
  gem$MODEL <- "GEM-E3_EU"
  all <- rbind(all,gem)
 
  #### from raw wide format to long format with additional columns
  cat("- change format data\n")
  all <- process_data(all,scens)

  #re-factorize all character and numeric columns
  cat("- factorise data\n")
  all <- factor.data.frame(all)
 
  ###### Manual changes before addition of calculated variables  
  cat("- make adjustments to data\n")
  source(paste(adjust,".R",sep=""))  
 
  #### add variables
  if(addvars){
    cat("- add variables to data\n")
    all <- add_variables(all,scens)
  }

  #set scope to "national" for national models
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"
  #change model name for national models, so that they appear first
  all<-data.table(all)
  if(!substr(cfg$models_nat[1],1,1)=="*"){
    all[model %in% cfg$models_nat]$model <- paste0("*",all[model %in% cfg$models_nat]$model)
    cfg$models_nat <- paste0("*",cfg$models_nat)
  }
  
  #get rid of Historical duplicates
  #all <- all[Category!="Historical"]
  
  #save country specific file with processed data
  save("all",file = paste0("data/",cfg$infile,reg,"_proc.Rdata"))
  
}# end if-else: process data
