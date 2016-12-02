#############################################################
########## Main file that loads and processes data ##########
########## and produces the national fact sheet    ##########
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
source("settings/config_BRA.R")
#overwrite file to be used for analysis
cfg$infile    <- "cdlinks_compare_20161129-162657_2"

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for rbinding a dataframe with another, removing duplicates
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions.R")

# flag to process data, reprocess even if .._reg_proc.RData file is available (i.e. overwrite existing RData)
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load data ###########################
#############################################################

#input reference budgets for national scenarios:
ref_budgets <- data.frame(region =c(rep("IND",4),rep("BRA",4), rep("JPN",4),rep("RUS",4) ,rep("CHN",4), rep("EUR",4),rep("USA",4)),scenario=rep(c("high","high","low","low"),7),border=rep(c("min","max"),14),
                          value=c(125*1.05,125*.95,90*1.05,90*.95, 42*1.05,42*.95,22*1.05,22*.95,
                                  36.5*1.05,36.5*.95,31.47*1.05,31.47*.95, 50.5*1.05,50.5*.95,45*1.05,45*.95,
                                  400*1.05,400*.95,290*1.05,290*.95, 112*1.05,112*.95,95*1.05,95*.95,
                                  100*1.05,100*.95,100*1.05,100*.95))

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

  # Add new column "Category" and fill with name according to scenario-to-Categroy-mapping in "scens"
  scens <- fread("settings/scen_categ_cdlinks_indc_BRA.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]

  #reduce size of the data frame
  vars <- fread("settings/variables.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$r]


  #############################################################
  ####################### Process data ########################
  #############################################################
  cat("Processing data\n")

  #manual change before addition of scenario categories
  all[SCENARIO=="Baseline"]$SCENARIO <- "NoPOL"
  
  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  ###### Manual changes before addition of calculated variables  
  source("adjust_reporting_indc.R")  
  
  #### add variables
  all <- add_variables(all,scens)
  
#   #### manual changes after addition of variables
#   #Change Category for AMPERE3-scenarios for GEM-E3 for regions where the results should feature (because this model has no LIMITS data)
#   all[scenario == "MILES-AMPERE3-CF450" & model == "GEM-E3_V1" & region %in% c("BRA","EU")]$Category <- "Global 450 / S2-3"
#   all[scenario == "MILES-AMPERE3-Base" & model == "GEM-E3_V1" & region %in% c("BRA","EU")]$Category <- "Baseline / S0-1"
  
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
render("INDC_sheet_BRA.rmd",output_file=paste0("INDC_sheet_",cfg$r,".pdf"))
