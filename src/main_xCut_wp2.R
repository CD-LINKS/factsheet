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
ref_budgets <- data.frame(region =c(rep("IND",4),rep("BRA",4), rep("JPN",4),rep("RUS",4) ,rep("CHN",4), rep("EUR",4),rep("USA",4)),scenario=rep(c("high","high","low","low"),7),border=rep(c("min","max"),14),
                          value=c(125*1.05,125*.95,90*1.05,90*.95, 42*1.05,42*.95,22*1.05,22*.95,
                                  36.5*1.05,36.5*.95,31.47*1.05,31.47*.95, 50.5*1.05,50.5*.95,45*1.05,45*.95,
                                  400*1.05,400*.95,290*1.05,290*.95, 112*1.05,112*.95,95*1.05,95*.95,
                                  100*1.05,100*.95,100*1.05,100*.95))

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
  source("adjust_reporting_indc.R")

  #### add variables
  all <- add_variables(all,scens)
  
  #correct scope for added variables
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case DNE21+ V.14: only national protocol scenarios for JPN are "national", so the rest is global
  all[all$model == "DNE21+ V.14" & all$scenario %in% c("NoPolicy","INDCi","INDC2030i_1000","INDC2030i_1600",
                                                       "INDC2030i_400","NPi2020_1000","NPi2020_1600","NPi2020_400")]$Scope <- "global"
  all[all$model == "DNE21+ V.14" & all$scenario %in% c("NPi") & all$region!="JPN"] <- "global"
  
  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))

}# end if-else: load and process stocktaking data

#############################################################
################## Do plots for cross-cut analysis ##########
#############################################################

source("cross_cut_wp2.R")

#############################################################
################## create fact sheet for World ##############
#############################################################

source("settings/config_World.R")
# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#source("functions/plot_functions_wp2.R")
render("INDC_sheet_world.rmd",output_file=paste0("INDC_sheet_world.pdf"))
