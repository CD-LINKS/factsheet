#############################################################
########## Main file that loads and processes data ##########
########## and produces the national fact sheet    ##########
#############################################################

library(reshape2)   # melt
library(data.table) # setnames, nice view option
library(dplyr)      # %>%
library(tidyr)      # spread
library(ggplot2)    # ggplot
library(ggtern)     # ternary plots
library(rmarkdown)  # render pdf
library(directlabels) # year labels for scatter plots

#source configuration file for region-specific data
source("settings/config_CHN.R")
#source file with plot functions
source("functions/plot_functions.R")

# Create plot directory
if(!file.exists(cfg$outdir)) {
  dir.create(cfg$outdir, recursive = TRUE)
}

#############################################################
####################### Load data ###########################
#############################################################

#if processed data is already available, just load it. To redo processing (e.g. after adding new calculated variable, please delete the ..._proc.Rdata file)
if (file.exists(paste0("data/",cfg$infile,"_proc.Rdata"))){
  cat("Loading processed data from file", paste0("data/",cfg$infile,".Rdata"),"\n",
      "delete this file and re-run if you want to do the data processing again", "\n")
  load(paste0("data/",cfg$infile,"_proc.Rdata")) 
  Sys.sleep(3)#give everybody the chance to read the above message
} else {
  
if (file.exists(paste0("data/",cfg$infile,".Rdata"))) {
    cat("Loading file", paste0("data/",cfg$infile,".Rdata"),"\n")
    load(paste0("data/",cfg$infile,".Rdata")) 
  } else {
    cat("Reading data from file",paste0("data/",cfg$infile,".csv"),"\n")
    all <- invisible(fread(paste0("data/",cfg$infile,".csv"),header=TRUE))
    save("all",file = paste0("data/",cfg$infile,".Rdata"))  
}

#############################################################
####################### Process data ########################
#############################################################
cat("Processing data\n")

# move years from rows to a new column
all <- invisible(melt(all,measure.vars=names(all)[grep("[0-9]+",names(all))],variable.name = "Year",variable.factor=FALSE))
all$Year <- as.numeric(all$Year)
# Rename columns
setnames(all, "MODEL", "Model")
setnames(all, "SCENARIO", "Scenario")
setnames(all, "REGION", "Region")
setnames(all, "VARIABLE", "Variable")
setnames(all, "UNIT", "Unit")

# Add new column "Category" and fill with name according to Scenario-to-Categroy-mapping in "scens"
scens <- fread("settings/scen_categ_cdlinks.csv", header=TRUE)
#get rid of duplicated scenarios
scens <- scens[!duplicated(scens$Scenario)]
all   <- merge(scens, all, by=c("Scenario"), all=TRUE)
all   <- all %>% filter (Category!="Limited")
#for the moment, remove SSP scenarios, to reduce spread
all <- all %>% filter(Model!="MESSAGE-GLOBIOM_1.0")


# Understanding R: Equivalents:
#a <- subset(all, subset=!Category=="Limited")
#b <- filter(all,Category!="Limited")
#c <- all %>% filter (Category!="Limited")

# Select variables defined in variables.csv
vars <- fread("settings/variables.csv",header=TRUE,stringsAsFactors=FALSE,sep='\n')
all  <- all[Variable %in% vars$Variable]
all  <- na.omit(all) # remove rows containing NAs

#add column for sorting into national and global models
all$Scope <- factor("global",levels = c("global","national"))
#make variables a factor, so that the order in facets can be manipulated easily
all$Variable <- factor(all$Variable)
#to be able to not loop over "Baselines of baselines"
all[all$Baseline=="-",]$Baseline <- NA

# For EU keep data only that has CO2 emissions in 2010 in right range
mods <- unique(all[Year=="2010" & Variable=="Emissions|CO2" & Region=="EU" & value>1000, Model, with=TRUE])
dat2 <- subset(all, Model %in% mods & Region=="EU")
all  <- rbind(subset(all, !Region=="EU"),dat2)

# Multiplying Brazil GDP for COPPE by 1000 because reported differently (factor 1000 different from global models)
all[Model=="COPPE-MSB_v1.3.2"&Variable=="GDP|MER"&Region=="BRA"]$value=all[Model=="COPPE-MSB_v1.3.2"&Variable=="GDP|MER"&Region=="BRA"]$value*1000

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2"
tmp1 <- all[Model %in% setdiff(unique(all[Variable=="Emissions|CO2"]$Model),unique(all[Variable=="Emissions|CO2|Energy and Industrial Processes"]$Model)) & 
              Variable == "Emissions|CO2"]
tmp1$Variable <- "Emissions|CO2|Energy and Industrial Processes"

all <- rbind(all,tmp1)

#Change Category for AMPERE3-Scenarios for GEM-E3 for regions where the results should feature (because this model has no LIMITS data)
all[Scenario == "MILES-AMPERE3-CF450" & Model == "GEM-E3_V1" & Region %in% c("BRA","EU")]$Category <- "Global 450 / S3"
all[Scenario == "MILES-AMPERE3-Base" & Model == "GEM-E3_V1" & Region %in% c("BRA","EU")]$Category <- "Baseline"

####Additional variables
#source functions for creation of additional variables
source("functions/calcVariable.R")
source("functions/calcRel2Base.R")
all <- calcVariable(all,'`Emissions|CO2|FFI` ~ `Emissions|CO2|Energy and Industrial Processes` ' , newUnit='Mt CO2/yr')
all <- calcVariable(all,'`Emissions Intensity of GDP|MER` ~ `Emissions|CO2|FFI`/`GDP|MER` ' , newUnit='kg CO2/$US 2005')
all <- calcVariable(all,'`Emissions Intensity of GDP|PPP` ~ `Emissions|CO2|FFI`/`GDP|PPP` ' , newUnit='kg CO2/$US 2005')
all <- calcVariable(all,'`Emissions per capita` ~ `Emissions|CO2|FFI`/`Population` ' , newUnit='t CO2/cap')
all <- calcVariable(all,'`Carbon Intensity of FE` ~ `Emissions|CO2|FFI`/`Final Energy` ' , newUnit='kg CO2/GJ')
all <- calcVariable(all,'`Energy Intensity of GDP|MER` ~ `Final Energy`/`GDP|MER` ' , newUnit='GJ/$2005')
all <- calcVariable(all,'`Energy Intensity of GDP|PPP` ~ `Final Energy`/`GDP|PPP` ' , newUnit='GJ/$2005')
all <- calcVariable(all,'`GDP per capita|MER` ~ `GDP|MER`/`Population` ' , newUnit='1000 $US 2005/cap')
all <- calcVariable(all,'`GDP per capita|PPP` ~ `GDP|PPP`/`Population` ' , newUnit='1000 $US 2005/cap')
all <- calcRel2Base(all,var="Emissions|CO2|FFI",baseEq1=F,"relative Abatement|CO2")
all <- calcRel2Base(all,var="Carbon Intensity of FE",baseEq1=T,"Carbon intensity reduction rel. to Base")
all <- calcRel2Base(all,var="Energy Intensity of GDP|MER",baseEq1=T,"Energy intensity reduction rel. to Base")

save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))  

}
#this has to be executed every time
#set scope to "national" for national models
all[all$Model %in% cfg$model_nat,]$Scope <- "national"
#chnage Model name for national models, so that they appear first
if(!substr(cfg$model_nat[1],1,1)=="*"){
all[all$Model %in% cfg$model_nat,]$Model <- paste0("*",all[all$Model %in% cfg$model_nat,]$Model)
cfg$model_nat <- paste0("*",cfg$model_nat)

}

#############################################################
################## Produce fact sheet #######################
#############################################################


cat("Producing graphs in graphs folder and pdf in main folder\n")
render("fact_sheet.rmd",output_file=paste0("Fact_sheet_",cfg$r,".pdf"))
