#############################################################
########## Main file that loads and processes data ##########
########## for x-Cut analysis    ##########
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

#source configuration file for region-specific data
source("settings/config_xCut.R")

#Insert database snapshot name here
cfg$infile <- "cdlinks_compare_20171127-154822"

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for overwriting a dataframe for a subset of variables
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if _proc.rdata file is available
# set to true if you always want data re-processed
b.procdata = T
# flag to adjust reporting, set to T if you want to call the script with manual corrections of (missing) data, set to F if you don't want to use this functionality
b.adjustrep = F
# flag to create an overview of which models have submitted which scenarios and variables
b.submoverview = T
# flag to create fact sheets with data as submitted by national models (before processing)
b.natmods=F

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load stock-taking data ##############
#############################################################

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
if (file.exists(paste0("data/",cfg$infile,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("data/",cfg$infile,"_proc.Rdata"),"\n",
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

  
  #reduce size of the data frame - adjust this csv file if you want to delete or add variables to be maintained from the snapshot, for further analysis
  vars <- fread("settings/variables.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$regions]


  #####################################
  ####### Process stock-taking data ###
  #####################################
  cat("Processing stocktaking data\n")

  # Add information for new column "Category" - adjust the name here for your project-specific scenario names file
  scens <- fread("settings/scen_categ_cdlinks.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]

  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  if(b.submoverview){
    #   print out summary of models-scenarios and variables
    source("functions/SubmOverview.R")}
  
  if(b.natmods){
    #produce pdf with analysis of data as-submitted
    for (reg in c("JPN","BRA","CHN","IND","EU","RUS")){
        cat("Producing graphs in graphs folder and INDC_national_subm_xxx.pdf in main folder\n")
    render("national_scenarios.rmd",output_file=paste0("INDC_national_subm_",reg,".pdf"))
    }}
  
  # model specific adjustments - adjust this file for project specific fixing of data, where needed
  if(b.adjustrep){source("adjust_reporting.R")}

  #### add variables - add required indicators to this function (in folder 'functions', data_processing.R)
  all <- add_variables(all,scens)
  
  #correct scope for added variables
  all[all$model %in% cfg$models_nat,]$Scope <- "national"

  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  #get rid of Historical duplicates
  all <- all[Category!="Historical"]
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))

}# end if-else: load and process stocktaking data

#############################################################
######### create fact sheet for national scenarios ##########
#############################################################

source("functions/plot_functions.R")

for (reg in c("JPN","BRA","CHN","IND","EU","RUS")){
  cat("Producing graphs in graphs folder and factsheet_national_adj_xxx.pdf in main folder\n")
  render("markdown/national_scenarios.rmd",output_file=paste0("factsheet_national_adj_",reg,".pdf"))
}

#############################################################
################## Do plots for cross-cut analysis ##########
#############################################################
source("functions/plot_functions.R")
source("cross_cut.R")
source("functions/RegionalBudgetsChecks2100.R")

#############################################################
################## create fact sheet for World ##############
#############################################################

source("settings/config_World.R")
# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

render("markdown/factsheet_world.rmd",output_file=paste0("Factsheet_world.pdf"))
