#############################################################
########## Main file that loads and processes data ##########
########## and produces the national fact sheet    ##########
#############################################################

#Load libraries - if needed, install them first: install.packages("name of library")
library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots
library(stringr) #str_replace_all

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")

#source configuration file for region-specific data - adjust region code here (currently, settings files exist for BRA, CAN, CHN, EUR, IND, JPN, RUS, USA, World)
source("settings/config_CAN.R")

#overwrite file to be used for analysis - adjust name of database snapshot here
cfg$infile    <- "cdlinks_compare_20180615-153808"

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
# flag to adjust reporting, set to T if you want to call the script with manual corrections of (missing) data, set to F if you don't want to use this functionality
b.adjustrep = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load data ###########################
#############################################################

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, please set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,"_",cfg$r,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,".Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,"_",cfg$r,"_proc.Rdata"))
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

  # Add new column "Category" and fill with name according to scenario-to-Category-mapping in "scens" - adjust the name here for your project-specific scenario names file
  scens <- fread("settings/scen_categ_cdlinks.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]

  #reduce size of the data frame - adjust this csv file if you want to delete or add variables to be maintained from the snapshot, for further analysis
  vars <- fread("settings/variables.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$r]


  #############################################################
  ####################### Process data ########################
  #############################################################
  cat("Processing data\n")

  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  ###### Manual changes before addition of calculated variables  - adjust this file for project specific fixing of data, where needed
  if(b.adjustrep){source("adjust_reporting.R")}
  
  #### add variables - add required indicators to this function (in folder 'functions', data_processing.R)
  all <- add_variables(all,scens)
  
  #set scope to "national" for national models
  all[all$model %in% cfg$model_nat,]$Scope <- "national"
  #change model name for national models, so that they appear first
  if(!substr(cfg$model_nat[1],1,1)=="*"){
    all[all$model %in% cfg$model_nat,]$model <- paste0("*",all[all$model %in% cfg$model_nat,]$model)
    cfg$model_nat <- paste0("*",cfg$model_nat)
  }
#get rid of Historical duplicates
all <- all[Category!="Historical"]
  #save country specific file with processed data
  save("all",file = paste0("data/",cfg$infile,"_",cfg$r,"_proc.Rdata"))
}# end if-else: process data


#############################################################
################## Produce fact sheet #######################
#############################################################

theme_set(theme_bw())

cat("Producing graphs in graphs folder and pdf in main folder\n")
 render("markdown/factsheet.rmd",output_file=paste0("Factsheet_",cfg$r,".pdf"))

# render("INDC_sheet_world.rmd",output_file=paste0("INDC_sheet_world.pdf"))
#}