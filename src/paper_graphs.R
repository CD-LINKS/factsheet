
# Data processing ---------------------------------------------------------
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
cfg$infile <- "cdlinks_compare_20170720-114454"
cfg$outdir    <- "paper graphs"

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
bud <- read.csv2("data/ref_budgets.csv")
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
  
  cat("Processing stocktaking data\n")
  
  # Add information for new column "Catego ry"
  scens <- fread("settings/scen_categ_cdlinks_indc_bycountry.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]
  
  # fix wrong scenario name for COPPE (extra space at the end) already here
  all[SCENARIO == "INDC2030_low ",]$SCENARIO = "INDC2030_low"
  all <- all[!(MODEL=="GEM-E3_V1"&SCENARIO=="INDC")]
  
  # Change scenario names for some models to V2 to not mix up old global model results with new ones
  all[MODEL %in% c("AIM/Enduse 3.0","DNE21+ V.14 (national)","GEM-E3_V1","IPAC-AIM/technology V1.0","India MARKAL","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("AIM/Enduse 3.0","DNE21+ V.14 (national)","GEM-E3_V1","IPAC-AIM/technology V1.0","India MARKAL","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO,'_V3',sep="")
  
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

  #### manual changes after addition of variables
  
  # categorize national models
  all[all$Scope=="national",]$model <- paste0("*",all[all$Scope=="national",]$model)
  nat_models <- paste0("*",cfg$models_nat)
  
  #get rid of Historical duplicates
  #all <- all[Category!="Historical"]
  
  save("all",file = paste0("data/",cfg$infile,"_proc.Rdata"))
  
}# end if-else: load and process stocktaking data

# Figure 1a - time series -------------------------------------------------
source("functions/plot_functions.R")
cats <- c("NoPOL","NPi","INDC")
a<-plot_funnel2(reg="World",dt=all,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto greenhouse gas emissions",
             file_pre="1a_GHG_funnel",glob_lines=T,xlim=c(2005,2031),ylim=c(20000,75000),range=T,median=T)

# Figure 1b - Regional emissions ------------------------------------------
source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","EU","IND","JPN","RUS","USA")
cats <- c("NoPOL","NPi","INDC")
b<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),ylabel="GHG emissions (MtCO2eq/year)",
                                  file_pre="1b_GHG_reg_2030", var.labels = c("Global GHG emissions (2030)"),b.multiyear = F,globpoints = T) 


# Figure 1c - GHG sources -------------------------------------------------
regs <- c("World")
cats <- c("NoPOL","NPi","INDC")
c<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|CH4","Emissions|N2O","Emissions|CO2","Emissions|F-Gases"),cats = cats, 
                                  years=c(2030),file_pre="1c_GHG_sources_2030", 
                                  var.labels = c("Global CH4 emissions (Mt CH4/yr)","Global N2O emissions (kt N2O/yr)","Global CO2 emissions (Mt CO2/yr)","Global F-gas emissions (MtCO2eq/yr)"),
                                  b.multiyear = F, b.multivar=T,globpoints=T)


# Figure 1def - Key regions -----------------------------------------------
regs <- c("BRA")
cats <- c("Historical","NoPOL","NPi","INDC")
d<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1d_GHG_BRA_2030", 
                                  b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions Brazil (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
                                  globpoints=T)

regs <- c("CHN")
e<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1e_GHG_CHN_2030", 
                                  b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions China (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
                                  globpoints=T)

regs <- c("USA")
f<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1f_GHG_USA_2030", 
                                  b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions USA (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
                                  globpoints=T)


# Figure 1 together -------------------------------------------------------
library(gridExtra)
g=arrangeGrob(a,b,c,d,e,f,ncol=3)
ggsave(file=paste(cfg$outdir,"/Fig1.png",sep=""),g,width=20,height=10,dpi=300)

# Figure 2 - carbon budgets ------------------------------------------------
#specify plot scope
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "World")
mods <- unique(all$model)
vars <- "Emissions|CO2"
cats <- c("NoPOL","NPi","INDC", "2030_high", "2030_low", "2020_high", "2020_low", "2020_verylow")
scens2deg <- c("INDC", "2030_high", "2030_low", "2020_verylow")

#calculate emissions and 2050 budgets
v_emireg <- all %>%
  filter (variable %in% vars & region %in% regs & !is.na(value) & Category %in% cats & model %in% mods) %>%
  #mutate(value = value / 1000, unit = "GtCO2/yr") %>%
  factor.data.frame()
v_emireg <- as.data.table(v_emireg)
v_emireg$period <- as.numeric(as.character(v_emireg$period))
v_budgreg <- calcBudget(data = v_emireg,var = vars,new_var = paste0("Budget|",vars))

tmp1 <- filter(v_budgreg, variable=="Budget|Emissions|CO2",period == 2050)
tmp2 <- filter(v_emireg, period == 2010)
tmp3 <- filter(v_budgreg, variable=="Budget|Emissions|CO2",period == 2100)

### budgets expressed as multiples of 2010 to get rid of baseyear differences
v_emi_cumrel <- rbind(tmp1, tmp2) %>%
  select(-period,-unit) %>%
  spread(key = variable, value = value) %>%
  mutate( CO2rel2010 = 1000* `Budget|Emissions|CO2` / `Emissions|CO2` ) %>%
  select(model, scenario, Category, Scope,region,Baseline, `Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` ) %>%
  arrange(region, scenario, Category, Baseline, Scope, model) %>% gather(variable,value,`Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` )
v_emi_cumrel$unit<-"MtCO2"
v_emi_cumrel$period<-2050
v_emi_cumrel=data.table(v_emi_cumrel)

v_emi_cumrel2 <- rbind(tmp3, tmp2) %>%
  select(-period,-unit) %>%
  spread(key = variable, value = value) %>%
  mutate( CO2rel2010 = 1000* `Budget|Emissions|CO2` / `Emissions|CO2` ) %>%
  select(model, scenario, Category, Scope,region,Baseline, `Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` ) %>%
  arrange(region, scenario, Category, Baseline, Scope, model) %>% gather(variable,value,`Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` )
v_emi_cumrel2$unit<-"MtCO2"
v_emi_cumrel2$period<-2100
v_emi_cumrel2=data.table(v_emi_cumrel2)

#plotting
source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS")
two<-plot_pointrange_multiScen_glob(regs=regs,dt=v_emi_cumrel,vars=c("Budget|Emissions|CO2","CO2rel2010"),cats = cats, years=c(2050),ylabel="CO2 budget 2011-2050",
                                  file_pre="2_budget_reg_2050", var.labels = c("CO2 budget (MtCO2)","Emission years (yr)"),b.multicat=T,globpoints = T) #,b.multicat = T, b.multivar=T,
two<-plot_pointrange_multiScen_glob(regs=regs,dt=v_emi_cumrel,vars=c("Budget|Emissions|CO2","CO2rel2010"),cats = cats, years=c(2050),ylabel="CO2 budget 2011-2050",
                                    file_pre="2_budget_reg_2050_2", var.labels = c("CO2 budget (MtCO2)","Emission years (yr)"),b.multivar=T,globpoints=T) #,b.multicat = T, b.multivar=T,

two<-plot_pointrange_multiScen_glob(regs=regs,dt=v_emi_cumrel2,vars=c("Budget|Emissions|CO2","CO2rel2010"),cats = cats, years=c(2100),ylabel="CO2 budget 2011-2100",
                                    file_pre="2_budget_reg_2100", var.labels = c("CO2 budget (MtCO2)","Emission years (yr)"),b.multicat=T,globpoints=T) #,b.multicat = T, b.multivar=T,
two<-plot_pointrange_multiScen_glob(regs=regs,dt=v_emi_cumrel2,vars=c("Budget|Emissions|CO2","CO2rel2010"),cats = cats, years=c(2100),ylabel="CO2 budget 2011-2100",
                                    file_pre="2_budget_reg_2100_2", var.labels = c("CO2 budget (MtCO2)","Emission years (yr)"),b.multivar=T,globpoints = T) #,b.multicat = T, b.multivar=T,

# Figure 3 - implementation -----------------------------------------------
source("functions/plot_functions.R")
vars <- c("GDP per capita|MER","Energy Intensity of GDP|MER","Carbon Intensity of FE","Emissions per capita")
cats <- c("NPi","INDC","2020_verylow","2020_low","2020_high","2030_low","2030_high")
#ta1<-plot_bar_facet(reg="World",dt=all[period==2050],vars=vars,cats=cats,lab="Kaya factors; 2050)",file_pre="3_kaya_2050_bar")   
ta2<-plot_bar_facet2(reg="World",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - GHG; 2030",file_pre="kaya_GHG_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2030_high","2020_high","2030_low","2020_low","2020_verylow")) 
ta3<-plot_bar_facet2(reg="World",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - GHG; 2050",file_pre="kaya_GHG_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2030_high","2020_high","2030_low","2020_low","2020_verylow")) 

library(gridExtra)
g=arrangeGrob(ta2,ta3,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig3.png",sep=""),g,width=20,height=10,dpi=300)

source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","EU","IND","JPN","RUS","USA","World")
cats <- c("2020_high","2020_low","2030_high","2030_low")
tb1<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars="Mitigation Costs",cats=cats,years=2100,file_pre="MitiCosts_2100_mitigscens",ylabel="Mitigation costs as % of GDP (2100)",b.multicat=T,globpoints = T)
tb2<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars="Mitigation Costs",cats=cats,years=2050,file_pre="MitiCosts_2050_mitigscens",ylabel="Mitigation costs as % of GDP (2050)",b.multicat=T,globpoints=T)
g=arrangeGrob(tb2,tb1,ncol=1)
ggsave(file=paste(cfg$outdir,"/Fig3b.png",sep=""),g,width=16,height=12,dpi=300)
