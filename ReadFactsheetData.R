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

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")

#source configuration file for region-specific data
source("factsheet/functions/config_xCut.R")
cfg$infile <- "cdlinks_compare_20171031-120856"
cfg$outdir    <- "factsheet/data/factsheet"
#source function for factorizing data frames
source("factsheet/functions/factor.data.frame.R")
# source functions process_data() and add_variables()
source("factsheet/functions/data_processing.R")
#source function overwrite for overwriting a dataframe for a subset of variables
source("factsheet/functions/overwrite.R")
#source file with plot functions
#source("functions/plot_functions.R")

# flag to process data, reprocess even if _proc.rdata file is available
# set to true if you always want data re-processed
b.procdata = T

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#input reference budgets for national scenarios:
#bud <- read.csv2("data/ref_budgets.csv")
#bud$high <- as.numeric(as.character(bud$high))
#bud$low <- as.numeric(as.character(bud$low))
#ref_budgets <- data.frame(region =c(rep("IND",2),rep("BRA",2), rep("JPN",2),rep("RUS",2) ,rep("CHN",2), rep("EU",2),rep("USA",2),rep("World",2)),scen=rep(c("high","low"),8),
#                          value=c(bud[bud$country=="IND",]$high,bud[bud$country=="IND",]$low,
#                                  bud[bud$country=="BRA",]$high,bud[bud$country=="BRA",]$low,
#                                  bud[bud$country=="JPN",]$high,bud[bud$country=="JPN",]$low,
#                                  bud[bud$country=="RUS",]$high,bud[bud$country=="RUS",]$low,
#                                  bud[bud$country=="CHN",]$high,bud[bud$country=="CHN",]$low,
#                                  bud[bud$country=="EUR",]$high,bud[bud$country=="EUR",]$low,
#                                  bud[bud$country=="USA",]$high,bud[bud$country=="USA",]$low,
#                                  bud[bud$country=="World",]$high,bud[bud$country=="World",]$low))

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
if (file.exists(paste0("factsheet/data/",cfg$infile,"_proc.Rdata")) & !b.procdata) {
  cat("Loading processed data from file", paste0("factsheet/data/",cfg$infile,"_proc.Rdata"),"\n",
      "set b.procdata flag and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,"_proc.Rdata"))
  Sys.sleep(2)#give everybody the chance to read the above message
} else {
  
  if (file.exists(paste0("factsheet/data/",cfg$infile,".Rdata"))) {
    cat("Loading file", paste0("factsheet/data/",cfg$infile,".Rdata"),"\n")
    load(paste0("factsheet/data/",cfg$infile,".Rdata"))
  } else {
    cat("Reading data from file",paste0("factsheet/data/",cfg$infile,".csv"),"\n")
    all <- invisible(fread(paste0("factsheet/data/",cfg$infile,".csv"),header=TRUE))
    save("all",file = paste0("factsheet/data/",cfg$infile,".Rdata"))
  }
  all_original <- all
  #reduce size of the data frame
  vars <- fread("factsheet/functions/variables_xCut.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
  all  <- all[VARIABLE %in% vars$variable & REGION %in% cfg$regions]
} 
