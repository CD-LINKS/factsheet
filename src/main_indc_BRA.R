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
library(stringr) #str_replace_all

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")

#source configuration file for region-specific data
source("settings/config_BRA.R")
#overwrite file to be used for analysis (only need to update this file name with latest snapshot)
cfg$infile    <- "cdlinks_compare_20171127-154822"

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

#############################################################
####################### Load data ###########################
#############################################################

#input reference budgets for national scenarios:
bud <- read.csv2("data/ref_budgets.csv",)
bud$high <- as.numeric(as.character(bud$high))
bud$low <- as.numeric(as.character(bud$low))
ref_budgets <- data.frame(region =c(rep("IND",2),rep("BRA",2), rep("JPN",2),rep("RUS",2) ,rep("CHN",2), rep("EU",2),rep("USA",2),rep("World",2)),scen=rep(c("high","low"),8),
                          value=c(bud[bud$country=="IND",]$high,bud[bud$country=="IND",]$low,
                                  bud[bud$country=="BRA",]$high,bud[bud$country=="BRA",]$low,
                                  bud[bud$country=="JPN",]$high,bud[bud$country=="JPN",]$low,
                                  bud[bud$country=="RUS",]$high,bud[bud$country=="RUS",]$low,
                                  bud[bud$country=="CHN",]$high,bud[bud$country=="CHN",]$low,
                                  bud[bud$country=="EUR",]$high,bud[bud$country=="EUR",]$low,
                                  bud[bud$country=="USA",]$high,bud[bud$country=="USA",]$low,
                                  bud[bud$country=="World",]$high,bud[bud$country=="World",]$low))


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

  # Add new column "Category" and fill with name according to scenario-to-Category-mapping in "scens"
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
  #all[SCENARIO=="Baseline"]$SCENARIO <- "NoPOL"
  #all <- all[!(MODEL=="GEM-E3_V1"&SCENARIO=="INDC")]
  
  # Change scenario names for some models to V3 to not mix up old global model results with new ones
  # Needed when snapshot includes older, non-V3 scenarios. And change GEM-E3 scenario names
  all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO,'_V3',sep="")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO,"INDCi_recGenTaxation_V4","INDCi_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO,"INDC2030i_1000_recGenTaxation_V4","INDC2030i_1000_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO,"NPi2020_1000_recGenTaxation_V4","NPi2020_1000_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO,"NPi2020_400_recGenTaxation_V4","NPi2020_400_V3")
  
  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  ###### Manual changes before addition of calculated variables  
  source("adjust_reporting_indc.R")  
  
  #### add variables
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
render("INDC_sheet_BRA.rmd",output_file=paste0("INDC_sheet_",cfg$r,".pdf"))
