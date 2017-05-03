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
cfg$infile <- "cdlinks_compare_20170503-113312"

#source function for factorizing data frames
source("functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("functions/data_processing.R")
#source function overwrite for overwriting a dataframe for a subset of variables
source("functions/overwrite.R")
#source file with plot functions
source("functions/plot_functions_wp2.R")

# flag to process data, reprocess even if _proc.rdata file is available
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load stock-taking data ##############
#############################################################

#input reference budgets for national scenarios:
bud <- read.csv2("data/ref_budgets.csv",)
bud$high <- as.numeric(as.character(bud$high))
bud$low <- as.numeric(as.character(bud$low))
ref_budgets <- data.frame(region =c(rep("IND",4),rep("BRA",4), rep("JPN",4),rep("RUS",4) ,rep("CHN",4), rep("EU",4),rep("USA",4),rep("World",4)),scen=rep(c("high","low"),8),
                          value=c(bud[bud$country=="IND",]$high*0.95,bud[bud$country=="IND",]$low*0.95,bud[bud$country=="IND",]$high*1.05,bud[bud$country=="IND",]$low*1.05,
                                  bud[bud$country=="BRA",]$high*0.95,bud[bud$country=="BRA",]$low*0.95,bud[bud$country=="BRA",]$high*1.05,bud[bud$country=="BRA",]$low*1.05,
                                  bud[bud$country=="JPN",]$high*0.95,bud[bud$country=="JPN",]$low*0.95,bud[bud$country=="JPN",]$high*1.05,bud[bud$country=="JPN",]$low*1.05,
                                  bud[bud$country=="RUS",]$high*0.95,bud[bud$country=="RUS",]$low*0.95,bud[bud$country=="RUS",]$high*1.05,bud[bud$country=="RUS",]$low*1.05,
                                  bud[bud$country=="CHN",]$high*0.95,bud[bud$country=="CHN",]$low*0.95,bud[bud$country=="CHN",]$high*1.05,bud[bud$country=="CHN",]$low*1.05,
                                  bud[bud$country=="EUR",]$high*0.95,bud[bud$country=="EUR",]$low*0.95,bud[bud$country=="EUR",]$high*1.05,bud[bud$country=="EUR",]$low*1.05,
                                  bud[bud$country=="USA",]$high*0.95,bud[bud$country=="USA",]$low*0.95,bud[bud$country=="USA",]$high*1.05,bud[bud$country=="USA",]$low*1.05,
                                  bud[bud$country=="World",]$high*0.95,bud[bud$country=="World",]$low*0.95,bud[bud$country=="World",]$high*1.05,bud[bud$country=="World",]$low*1.05))


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

  # fix wrong scenario name for COPPE (extra space at the end) already here
  all[SCENARIO == "INDC2030_low ",]$SCENARIO = "INDC2030_low"
  
  # Change scenario names for some models to V2 to not mix up old global model results with new ones
  all[MODEL %in% c("AIM/Enduse 3.0","AIM/Enduse[Japan]","COPPE-COFFEE 1.0","China TIMES","DNE21+ V.14","DNE21+ V.14 (national)","GEM-E3_V1",
                   "IPAC-AIM/technology V1.0","India MARKAL","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO <- paste(all[MODEL %in% c("AIM/Enduse 3.0","AIM/Enduse[Japan]","COPPE-COFFEE 1.0","China TIMES","DNE21+ V.14","DNE21+ V.14 (national)","GEM-E3_V1",
                                                                                                                             "IPAC-AIM/technology V1.0","India MARKAL","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO,'_V2',sep="")
  
  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
  # #   print out summary of models-scenarios and variables
  #   source("functions/SubmOverview.R")
  #   
  #   #produce pdf with analysis of data as-submitted
  #   for (reg in c("JPN","BRA","CHN","IND","EU","RUS")){
  #       cat("Producing graphs in graphs folder and INDC_national_subm_xxx.pdf in main folder\n")
  #   render("national_scenarios.rmd",output_file=paste0("INDC_national_subm_",reg,".pdf"))
  #   }
  
  
  # model specific adjustments
  source("adjust_reporting_indc.R")

  #### add variables
  all <- add_variables(all,scens)
  
  #correct scope for added variables
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case DNE21+ V.14: only national protocol scenarios for JPN are "national", so the rest is global
  # all[all$model == "DNE21+ V.14" & all$scenario %in% c("NoPolicy_V2","INDCi_V2","INDC2030i_1000_V2","INDC2030i_1600_V2",
  #                                                      "INDC2030i_400_V2","NPi2020_1000_V2","NPi2020_1600_V2","NPi2020_400_V2")]$Scope <- "global"
  # all[all$model == "DNE21+ V.14" & all$scenario %in% c("NPi_V2") & all$region!="JPN"] <- "global"
  
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

# source("functions/plot_functions.R")
# 
# for (reg in c("JPN","BRA","CHN","IND","EU","RUS")){
#   cat("Producing graphs in graphs folder and INDC_national_adj_xxx.pdf in main folder\n")
#   render("national_scenarios.rmd",output_file=paste0("INDC_national_adj_",reg,".pdf"))
# }

#############################################################
################## Do plots for cross-cut analysis ##########
#############################################################
source("functions/plot_functions_wp2.R")
source("cross_cut_wp2.R")

#############################################################
################## create fact sheet for World ##############
#############################################################

source("settings/config_World.R")
# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

render("INDC_sheet_world.rmd",output_file=paste0("INDC_sheet_world.pdf"))

# for (reg in c("JPN","BRA","CHN","IND","EUR","RUS")){
# #source configuration file for region-specific data
# source(paste0("settings/config_",reg,".R"))