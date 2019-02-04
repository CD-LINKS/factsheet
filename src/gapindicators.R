
# Load data ---------------------------------------------------------------
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

  
  # model specific adjustments
  source("adjust_reporting_indc.R")
  
  #### add variables
  all <- add_variables(all,scens)
  
  
  #correct scope for added variables
  all[all$model %in% cfg$models_nat,]$Scope <- "national"
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"

  #### manual changes after addition of variables
  
  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))
  
}# end if-else: load and process stocktaking data



# Select data for gap indicators ------------------------------------------


# 1. GHG/CO2 emissions ----------------------------------------------------
emis=all[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2","Emissions|N2O","Emissions|CH4","Emissions|F-Gases","Emissions|CO2|AFOLU","Emissions|Non-CO2")&
           Category%in%c("NoPOL","NPi","INDC","2020_verylow","2020_low","2030_low")]
emis=spread(emis,period,value)
emis$scenario<-NULL
emis$Baseline<-NULL
emis$Scope<-NULL
setcolorder(emis,c("variable","Category","region","unit","model"))
setnames(emis,"Category","scenario")
emis$scenario<-str_replace_all(emis$scenario,"NoPOL","No policy")
emis$scenario<-str_replace_all(emis$scenario,"NPi","Current policies")
emis$scenario<-str_replace_all(emis$scenario,"INDC","NDC")
emis$scenario<-str_replace_all(emis$scenario,"2020_low","2C")
emis$scenario<-str_replace_all(emis$scenario,"2030_low","2C delay")
emis$scenario<-str_replace_all(emis$scenario,"2020_verylow","1.5C")

write.csv(emis,"GHGemissions.csv")

emisrange=data.table(gather(emis,period,value,c(`2005`,`2010`,`2015`,`2020`,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`,`2055`,`2060`,`2065`,`2070`,`2075`,`2080`,`2085`,`2090`,`2095`,`2100`)))
emisrange=emisrange[,list(min=min(value,na.rm=T),tenp=quantile(value,probs=c(0.1),na.rm=T),mean=mean(value,na.rm=T),median=median(value,na.rm=T),
                          ninetyp=quantile(value,probs=c(0.9),na.rm=T),max=max(value,na.rm=T)),
               by=c("variable","scenario","region","unit","period")]
emisrange=gather(emisrange,statistic,value,c(min,tenp,mean,median,ninetyp,max))
emisrange=na.omit(emisrange)
emisrange=spread(emisrange,period,value)
write.csv(emisrange,"GHGemissionsrange.csv")

# 2. Carbon intensity --------------------------------------------------------
#TO DO: calculate %/year with [[C/GDP(t)-C/GDP(t-5)]/5]/(C/GDP)(2010), t=2015,2020,2025,2030 etc.

intens=all[variable%in%c("Carbon Intensity of GDP|MER","GHG Intensity of GDP|MER")&
           Category%in%c("NoPOL","NPi","INDC","2020_verylow","2020_low","2030_low")]
intens=spread(intens,period,value)
intens$scenario<-NULL
intens$Baseline<-NULL
intens$Scope<-NULL
setcolorder(intens,c("variable","Category","region","unit","model"))
setnames(intens,"Category","scenario")
intens$scenario<-str_replace_all(intens$scenario,"NoPOL","No policy")
intens$scenario<-str_replace_all(intens$scenario,"NPi","Current policies")
intens$scenario<-str_replace_all(intens$scenario,"INDC","NDC")
intens$scenario<-str_replace_all(intens$scenario,"2020_low","2C")
intens$scenario<-str_replace_all(intens$scenario,"2030_low","2C delay")
intens$scenario<-str_replace_all(intens$scenario,"2020_verylow","1.5C")

write.csv(intens,"Cintensity.csv")

intensrange=data.table(gather(intens,period,value,c(`2005`,`2010`,`2015`,`2020`,`2025`,`2030`,`2035`,`2040`,`2045`,`2050`,`2055`,`2060`,`2065`,`2070`,`2075`,`2080`,`2085`,`2090`,`2095`,`2100`)))
intensrange=intensrange[,list(min=min(value,na.rm=T),tenp=quantile(value,probs=c(0.1),na.rm=T),mean=mean(value,na.rm=T),median=median(value,na.rm=T),
                              ninetyp=quantile(value,probs=c(0.9),na.rm=T),max=max(value,na.rm=T)),
                    by=c("variable","scenario","region","unit","period")]
intensrange=gather(intensrange,statistic,value,c(min,tenp,mean,median,ninetyp,max))
intensrange=na.omit(intensrange)
intensrange=spread(intensrange,period,value)
write.csv(intensrange,"Cintensityrange.csv")


rintens=all[variable%in%c("Rate of Change| Carbon Intensity of GDP|MER","Rate of Change| GHG Intensity of GDP|MER")&
             Category%in%c("NoPOL","NPi","INDC","2020_verylow","2020_low","2030_low")]
rintens=spread(rintens,period,value)
rintens$scenario<-NULL
rintens$Baseline<-NULL
rintens$Scope<-NULL
setcolorder(rintens,c("variable","Category","region","unit","model"))
setnames(rintens,"Category","scenario")
rintens$scenario<-str_replace_all(rintens$scenario,"NoPOL","No policy")
rintens$scenario<-str_replace_all(rintens$scenario,"NPi","Current policies")
rintens$scenario<-str_replace_all(rintens$scenario,"INDC","NDC")
rintens$scenario<-str_replace_all(rintens$scenario,"2020_low","2C")
rintens$scenario<-str_replace_all(rintens$scenario,"2030_low","2C delay")
rintens$scenario<-str_replace_all(rintens$scenario,"2020_verylow","1.5C")

write.csv(rintens,"Cintensity_rateofchange.csv")

rintensrange=data.table(gather(rintens,period,value,c(`2020-2050`,`2030-2050`,`2050-2100`)))
rintensrange=rintensrange[,list(min=min(value,na.rm=T),tenp=quantile(value,probs=c(0.1),na.rm=T),mean=mean(value,na.rm=T),median=median(value,na.rm=T),
                                ninetyp=quantile(value,probs=c(0.9),na.rm=T),max=max(value,na.rm=T)),
                        by=c("variable","scenario","region","unit","period")]
rintensrange=gather(rintensrange,statistic,value,c(min,tenp,mean,median,ninetyp,max))
rintensrange=na.omit(rintensrange)
rintensrange=spread(rintensrange,period,value)
write.csv(rintensrange,"Cintensity_rateofchange_range.csv")

# 3. Carbon budget --------------------------------------------------------

budget=all[variable%in%c("Carbon budget","Carbon budget|Energy and Industry")&
           Category%in%c("NoPOL","NPi","INDC","2020_verylow","2020_low","2030_low")]
budget=spread(budget,period,value)
budget$scenario<-NULL
budget$Baseline<-NULL
budget$Scope<-NULL
setcolorder(budget,c("variable","Category","region","unit","model"))
setnames(budget,"Category","scenario")
budget$scenario<-str_replace_all(budget$scenario,"NoPOL","No policy")
budget$scenario<-str_replace_all(budget$scenario,"NPi","Current policies")
budget$scenario<-str_replace_all(budget$scenario,"INDC","NDC")
budget$scenario<-str_replace_all(budget$scenario,"2020_low","2C")
budget$scenario<-str_replace_all(budget$scenario,"2030_low","2C delay")
budget$scenario<-str_replace_all(budget$scenario,"2020_verylow","1.5C")
setnames(budget,"2030","2011-2030")
setnames(budget,"2050","2011-2050")
setnames(budget,"2100","2011-2100")
setnames(budget,"2105","2051-2100")

write.csv(budget,"Cbudget.csv")

budgetrange=data.table(gather(budget,period,value,c(`2011-2030`,`2011-2050`,`2011-2100`,`2051-2100`)))
budgetrange=budgetrange[,list(min=min(value,na.rm=T),tenp=quantile(value,probs=c(0.1),na.rm=T),mean=mean(value,na.rm=T),median=median(value,na.rm=T),
                              ninetyp=quantile(value,probs=c(0.9),na.rm=T),max=max(value,na.rm=T)),
                          by=c("variable","scenario","region","unit","period")]
budgetrange=gather(budgetrange,statistic,value,c(min,tenp,mean,median,ninetyp,max))
budgetrange=na.omit(budgetrange)
budgetrange=spread(budgetrange,period,value)
write.csv(budgetrange,"Cbudgetrange.csv")
