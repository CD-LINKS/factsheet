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
library(stringr) #str_replace_all

#set working directory for R right if it is not by default (it is the right one by default if you open Rstudio by clicking on this main.R file)
#setwd("D:/location-of-srcfolder-on-your-system")

#source configuration file for region-specific data
source("settings/config_xCut.R")
cfg$infile <- "cdlinks_compare_20190123-155652"

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
ref_budgets <- data.frame(region =c(rep("IND",2),rep("BRA",2), rep("JPN",2),rep("RUS",2) ,rep("CHN",2), rep("EU",2),rep("USA",2),rep("World",2)),scen=rep(c("high","low"),8),
                          value=c(bud[bud$country=="IND",]$high,bud[bud$country=="IND",]$low,
                                  bud[bud$country=="BRA",]$high,bud[bud$country=="BRA",]$low,
                                  bud[bud$country=="JPN",]$high,bud[bud$country=="JPN",]$low,
                                  bud[bud$country=="RUS",]$high,bud[bud$country=="RUS",]$low,
                                  bud[bud$country=="CHN",]$high,bud[bud$country=="CHN",]$low,
                                  bud[bud$country=="EUR",]$high,bud[bud$country=="EUR",]$low,
                                  bud[bud$country=="USA",]$high,bud[bud$country=="USA",]$low,
                                  bud[bud$country=="World",]$high,bud[bud$country=="World",]$low))


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

  # Add information for new column "Catego ry"
  scens <- fread("settings/scen_categ_cdlinks_indc_bycountry.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]

  #all <- all[!(MODEL=="GEM-E3_V1"&SCENARIO=="INDC")]

  # Change scenario names for some models to V4 to not mix up old global model results with new ones, while using latest version of other models (V3)
  #Special case for RU_TIMES: no V3/V4 at all
  all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("RU-TIMES 3.2")]$SCENARIO,'_V4',sep="")
  #Special case for GEM-E3: select the recGenTaxation scenarios for standard analysis
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDCi_recGenTaxation_V4")]$SCENARIO,"INDCi_recGenTaxation_V4","INDCi_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDC2030i_1000_recGenTaxation_V4")]$SCENARIO,"INDC2030i_1000_recGenTaxation_V4","INDC2030i_1000_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_1000_recGenTaxation_V4")]$SCENARIO,"NPi2020_1000_recGenTaxation_V4","NPi2020_1000_V4")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_400_recGenTaxation_V4")]$SCENARIO,"NPi2020_400_recGenTaxation_V4","NPi2020_400_V4")
  #Rest: use latest version V3 for national (and some global) models, rename to V4
  all[MODEL %in% c("AIM/Enduse 3.0","AIM/Enduse[Japan]","China TIMES","COPPE-COFFEE 1.0","COPPE-MSB_v2.0","DNE21+ V.14 (national)","GCAM-USA_CDLINKS","India MARKAL","IPAC-AIM/technology V1.0","PRIMES_V1","REMIND-MAgPIE 1.7-3.0")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("AIM/Enduse 3.0","AIM/Enduse[Japan]","China TIMES","COPPE-COFFEE 1.0","COPPE-MSB_v2.0","DNE21+ V.14 (national)","GCAM-USA_CDLINKS","India MARKAL","IPAC-AIM/technology V1.0","PRIMES_V1","REMIND-MAgPIE 1.7-3.0")]$SCENARIO,"_V3","_V4")
  # Exclude Globiom and Magpie (used for food security analysis only), and AIM/CGE (newest scenarios are under AIM V2.1)
  all <- all[!MODEL%in%c("MAgPIE 3.0","GLOBIOM 1.0","AIM/CGE")]
  
  #### from raw wide format to long format with additional columns
  all <- process_data(all,scens)
  
  #re-factorize all character and numeric columns
  all <- factor.data.frame(all)
  
#   #add scope (global or national)
#   all[all$model %in% cfg$models_nat,]$Scope <- "national"
#   #special case DNE21+ V.14: only national protocol scenarios for JPN are "national", so the rest is global
#   all[all$model == "DNE21+ V.14" & all$scenario %in% c("NoPolicy","INDCi","INDC2030i_1000","INDC2030i_1600",
#                                                        "INDC2030i_400","NPi2020_1000","NPi2020_1600","NPi2020_400")]$Scope <- "global"
#   all[all$model == "DNE21+ V.14" & all$scenario %in% c("NPi") & all$region!="JPN"] <- "global"
#   
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
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"
    #special case DNE21+ V.14: only national protocol scenarios for JPN are "national", so the rest is global
  # all[all$model == "DNE21+ V.14" & all$scenario %in% c("NoPolicy","INDCi","INDC2030i_1000","INDC2030i_1600",
  #                                                      "INDC2030i_400","NPi2020_1000","NPi2020_1600","NPi2020_400")]$Scope <- "global"
  # all[all$model == "DNE21+ V.14" & all$scenario %in% c("NPi") & all$region!="JPN"] <- "global"
  # 
  
  #### manual changes after addition of variables
  
  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  #get rid of Historical duplicates
  #all <- all[Category!="Historical"]


# for (reg in c("JPN","BRA","CHN","IND","EU","USA")){ #"RUS",
#   cat("Producing graphs in graphs folder and INDC_national_adj_xxx.pdf in main folder\n")
#   render("national_scenarios.rmd",output_file=paste0("INDC_national_adj_",reg,".pdf"))
# }  
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))

}# end if-else: load and process stocktaking data

#############################################################
####################### Load diagnostics data  ##############
#############################################################

# #if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, set b.procdata = TRUE)
# if (file.exists(paste0("data/",cfg$diag_infile,"_proc.Rdata")) & !b.procdata){
#   cat("Loading processed data from file", paste0("data/",cfg$diag_infile,".Rdata"),"\n",
#       "set b.procdata flag and re-run if you want to do the data processing again", "\n")
#   load(paste0("data/",cfg$diag_infile,"_proc.Rdata"))
#   Sys.sleep(2)#give everybody the chance to read the above message
# } else {
#   
#   if (file.exists(paste0("data/",cfg$diag_infile,".Rdata"))) {
#     cat("Loading file", paste0("data/",cfg$diag_infile,".Rdata"),"\n")
#     load(paste0("data/",cfg$diag_infile,".Rdata"))
#   } else {
#     cat("Reading data from file",paste0("data/",cfg$diag_infile,".csv"),"\n")
#     diag <- invisible(fread(paste0("data/",cfg$diag_infile,".csv"),header=TRUE))
#     #re-factorize all character and numeric columns
#     diag <- factor.data.frame(diag)
#     save("diag",file = paste0("data/",cfg$diag_infile,".Rdata"))
#   }
#   
#   #reduce size of the data frame
#   vars <- fread("settings/variables_xCut.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
#   diag  <- diag[VARIABLE %in% vars$variable & REGION %in% cfg$regions]
#   #get rid of unnecessary year columns
#   source("functions/reduce_diag_t.R")
#   diag <- reduce_diag_t(diag)
#   
#   ######################################
#   ##### Process diagnostics data #######
#   ######################################
#   cat("Processing diagnostics data\n")
#   
# 
# 
#   
#   # FIXME: use Emissions|CO2 as a proxy for Emission|CO2|FFI
#  
#   # Add information for new column "Category"
#   diag_scens <- fread("settings/scen_categ_diag.csv", header=TRUE)
#   #### from raw wide format to long format with additional columns
#  
#   diag <- process_data(diag,diag_scens)
#  
#   #re-factorize all character and numeric columns
#   diag <- factor.data.frame(diag)
#   
#   
#   tmp1 <- diag %>% filter( model == "COPPE-MSB_v1.3.2", variable == "Emissions|CO2")
#   tmp1$variable ="Emissions|CO2|Energy and Industrial Processes"
#   
#   diag <-  overwrite(tmp1, diag)
#   
#   diag <- add_variables(diag,diag_scens)
#   
#   
#   d_nat_models <- c("COPPE-MSB_v1.3.2")  #,"China TIMES","IPAC-AIM/technology V1.0","PRIMES_V1",
#   #"AIM/Enduse[Japan]","DNE21+ V.14","GCAM4_MILES")
#   
#   #mark all national models with asterisk
#   diag[diag$model %in% d_nat_models,]$model <- paste0("*",diag[diag$model %in% d_nat_models,]$model)
#   d_nat_models <- paste0("*",d_nat_models)
#   diag[diag$model %in% d_nat_models,]$Scope <- "national"
#   
#   save("diag",file = paste0("data/",cfg$diag_infile,"_proc.Rdata"))
# } # end if-else: load and process diagnostics data


#############################################################
################## Do plots for cross-cut analysis ##########
#############################################################

### main cross cut analysis
 #source("cross_cut.R")
source("cross_cut_synthesis.R")

### regional budget analysis and base year emission consistency check
 #source("RegionalBudgetsChecks.R")

