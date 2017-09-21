
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
source("settings/config_xCut.R")
cfg$infile <- "cdlinks_compare_20170908-154923"
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
  
  # Add information for new column "Category"
  scens <- fread("settings/scen_categ_cdlinks_indc_bycountry.csv", header=TRUE)
  #get rid of duplicated scenarios
  scens <- scens[!duplicated(scens$scenario)]
  
  #all <- all[!(MODEL=="GEM-E3_V1"&SCENARIO=="INDC")]
  
  # Change scenario names for some models to V3 to not mix up old global model results with new ones
  # Needed when snapshot includes older, non-V3 scenarios
  all[MODEL %in% c("DNE21+ V.14 (national)","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO <- 
    paste(all[MODEL %in% c("DNE21+ V.14 (national)","PRIMES_V1","RU-TIMES 3.2")]$SCENARIO,'_V3',sep="")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("INDCi_recSocialSecurity_V3")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("INDCi_recSocialSecurity_V3")]$SCENARIO,"INDCi_recSocialSecurity_V3","INDCi_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_1000_recSocialSecurity_V3")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_1000_recSocialSecurity_V3")]$SCENARIO,"NPi2020_1000_recSocialSecurity_V3","NPi2020_1000_V3")
  all[MODEL %in% c("GEM-E3")&SCENARIO%in%c("NPi2020_400_recSocialSecurity_V3")]$SCENARIO <- str_replace_all(
    all[MODEL %in% c("GEM-E3")& SCENARIO%in%c("NPi2020_400_recSocialSecurity_V3")]$SCENARIO,"NPi2020_400_recSocialSecurity_V3","NPi2020_400_V3")
  
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
  #special case GEM-E3: national model for EU, global for other regions
  all[model=="GEM-E3"&region!="EU"]$Scope<-"global"
  
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
cats <- c("NoPOL","NPi","INDC","2020_low","2020_verylow")
a<-plot_funnel2(reg="World",dt=all,vars=c("Emissions|Kyoto Gases"),cats=cats,title="Kyoto greenhouse gas emissions",
             file_pre="1a_GHG_funnel",glob_lines=T,xlim=c(2010,2031),ylim=c(20000,75000),range=T,median=T)

# Figure 1c - GHG sources -------------------------------------------------
source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "RoW","World")
cats <- c("Historical","NoPOL","NPi","INDC")
c<-plot_stackbar_regions(regs=regs,dt=all,vars=c("Emissions|CO2|Energy"),cats = cats,per=c(2030),file_pre="1c_CO2energy_2030"
                                   ,lab = "Global energy CO2 emissions (Mt CO2/yr)",hist=T)


# Figure 1def (old) - Key regions -----------------------------------------------
# regs <- c("BRA")
# cats <- c("Historical","NoPOL","NPi","INDC")
# d<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1d_GHG_BRA_2030", 
#                                   b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions Brazil (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
#                                   globpoints=T)
# 
# regs <- c("CHN")
# e<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1e_GHG_CHN_2030", 
#                                   b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions China (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
#                                   globpoints=T)
# 
# regs <- c("USA")
# f<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),file_pre="1f_GHG_USA_2030", 
#                                   b.multiyear = F,nonreg=T,hist=T,var.labels=c("GHG emissions USA (2030)"),ylabel="GHG emissions (MtCO2eq/year)",
#                                   globpoints=T)



# Figure 1def (new) - GHG sources ----------------------------------------
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "RoW","World")
cats <- c("Historical","NoPOL","NPi","INDC")
d<-plot_stackbar_regions(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU"),cats = cats,per=c(2030),file_pre="1d_CO2land_2030"
                         ,lab = "Global land CO2 emissions (Mt CO2/yr)",hist=T)

e<-plot_stackbar_regions(regs=regs,dt=all,vars=c("Emissions|CH4"),cats = cats,per=c(2030),file_pre="1e_CH4_2030"
                         ,lab = "Global CH4 emissions (Mt CH4/yr)",hist=T)

f<-plot_stackbar_regions(regs=regs,dt=all,vars=c("Emissions|N2O"),cats = cats,per=c(2030),file_pre="1f_N2O_2030"
                         ,lab = "Global N2O emissions (kt N2O/yr)",hist=T)

# Figure 1 together -------------------------------------------------------
library(gridExtra)
g=arrangeGrob(a,c,d,e,f,ncol=3)
ggsave(file=paste(cfg$outdir,"/Fig1.png",sep=""),g,width=20,height=10,dpi=300)

library(grid)
lay<-rbind(c(1,1,2,3),c(1,1,4,5))
h=grid.arrange(a,c,d,e,f,layout_matrix=lay)#ncol=3
ggsave(file=paste(cfg$outdir,"/Fig1_arrange.png",sep=""),h,width=20,height=12,dpi=300)

# Figure 2 - regions -----------------------------------------------------
source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","EU","IND","JPN","RUS","USA")
cats <- c("Historical","NoPOL","NPi","INDC")
a2<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases"),cats = cats, years=c(2030),ylabel="GHG emissions (MtCO2eq/year)",
                                  file_pre="1b_GHG_reg_2030", var.labels = c("GHG emissions (2030)"),b.multiyear = F,globpoints = T,hist=T,
                                  modnames=T,mod.labels=c("AIM","COFFEE","DNE","GEM-E3","IMAGE","MESSAGE","POLES","REMIND","WITCH")) 
a_excl<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Emissions|Kyoto Gases|Excl. AFOLU CO2"),cats = cats, years=c(2030),ylabel="GHG emissions excl. AFOLU CO2 (MtCO2eq/year)",
                                   file_pre="1b_GHG_reg_2030", var.labels = c("GHG emissions (2030)"),b.multiyear = F,globpoints = T,hist=T,
                                   modnames=T,mod.labels=c("AIM","COFFEE","DNE","GEM-E3","IMAGE","MESSAGE","POLES","REMIND","WITCH")) 

# stacked bar per region
regs <- c("BRA")
cats <- c("Historical","NoPOL","NPi","INDC")
b2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2b_BRA_2030",lab = "Brazil GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                 "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                 "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                 "Emissions|N2O"="N2O"))

regs <- c("CHN")
c2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2c_CHN_2030",lab = "China GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                           "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                           "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                           "Emissions|N2O"="N2O"))

regs <- c("IND")
d2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2d_IND_2030",lab = "India GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                           "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                           "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                           "Emissions|N2O"="N2O"))

regs <- c("JPN")
e2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2e_JPN_2030",lab = "Japan GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                           "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                           "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                           "Emissions|N2O"="N2O"))

regs <- c("USA")
f2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2f_USA_2030",lab = "USA GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                         "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                         "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                         "Emissions|N2O"="N2O"))

regs <- c("EU")
g2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2g_EU_2030",lab = "EU GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                       "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                       "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                       "Emissions|N2O"="N2O"))

regs <- c("RUS")
h2<-plot_stackbar_ghg(regs=regs,dt=all,vars=c("Emissions|CO2|AFOLU","Emissions|CO2|Energy","Emissions|CH4","Emissions|N2O","Emissions|Kyoto Gases"),cats = cats,
                      per=c(2030),file_pre="2h_RUS_2030",lab = "Russia GHG emissions (Mt CO2eq/yr)",hist=T,labels=T,var.labels=c("Emissions|CH4"="CH4",
                                                                                                                                            "Emissions|CO2|AFOLU"="CO2 AFOLU",
                                                                                                                                            "Emissions|CO2|Energy"="CO2 Energy",
                                                                                                                                            "Emissions|N2O"="N2O"))

# Together
g=arrangeGrob(a2,b2,c2,d2,e2,f2,g2,h2,ncol=4)
ggsave(file=paste(cfg$outdir,"/Fig2.png",sep=""),g,width=22,height=12,dpi=300)

g=arrangeGrob(a2,b2,c2,d2,e2,f2,g2,h2,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig2_vertical.png",sep=""),g,width=20,height=22,dpi=300)

tmp<-ggplot_gtable(ggplot_build(b2))
leg<-which(sapply(tmp$grobs,function(x) x$name) =="guide-box")
legend<-tmp$grobs[[leg]]
b2=b2+theme(legend.position = "none")
c2=c2+theme(legend.position = "none")
d2=d2+theme(legend.position = "none")
e2=e2+theme(legend.position = "none")
f2=f2+theme(legend.position = "none")
g2=g2+theme(legend.position = "none")
h2=h2+theme(legend.position = "none")
lay<-rbind(c(1,1,1,2,3,4,5),c(1,1,1,6,7,8,9))
h=grid.arrange(a2,b2,c2,d2,e2,f2,g2,h2,legend,layout_matrix=lay)
ggsave(file=paste(cfg$outdir,"/Fig2_arrange.png",sep=""),h,width=20,height=12,dpi=300)

# Figure 3 - carbon budgets ------------------------------------------------
#specify plot scope
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS","RoW","World")
mods <- unique(all$model)
vars <- "Emissions|CO2"
#cats <- c("NoPOL","NPi","INDC", "2030_high", "2030_low", "2020_high", "2020_low", "2020_verylow")
cats <- c("NoPOL","NPi","INDC", "2030_low", "2020_low", "2020_verylow")
scens2deg <- c("INDC", "2030_high", "2030_low", "2020_verylow")

#calculate emissions and 2050 budgets
v_emireg <- all %>%
  filter (variable %in% vars & region %in% regs & !is.na(value) & Category %in% cats & model %in% mods) %>%
  #mutate(value = value / 1000, unit = "GtCO2/yr") %>%
  factor.data.frame()
v_emireg=data.table(v_emireg)
national=v_emireg[Scope=="national"]
v_emireg =  spread(v_emireg[Scope=="global"],key = region, value = value,fill=0) 
v_emireg = v_emireg%>%mutate (RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` )%>%
  gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`)%>%
  filter(!value==0)
setcolorder(v_emireg,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
v_emireg=rbind(v_emireg,national)
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

write.csv(v_emi_cumrel, file = "EmissionBudgets.csv", row.names = F,
          col.names = c("MODEL", "SCENARIO", "REGION", "CO2 Energy&Ind 2010",  "CO2 E&I 2010-2050", "Emission Years E&I",
                        "CO2 total 2010",  "CO2 total 2010-2050", "Emission Years CO2 total"))
# library(openxlsx)
# write.xlsx(v_emi_cumrel, file = "EmissionBudgets.xlsx")


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

# PDF-style 
theme_set(ggplot2::theme_bw(base_size = 15))

#2050
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS","RoW","World")
v_plot <-  filter(v_emi_cumrel, Category %in% cats) 
v_plot$Category =  factor(v_plot$Category, levels = cats, ordered = T)
v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)
v_plot=data.table(v_plot)

a=ggplot() +
  geom_boxplot(data=v_plot[Scope=="global"&variable=="Budget|Emissions|CO2"],aes(x=Category,y=value, fill = Category), outlier.size = 0) +
  geom_point(data=v_plot[variable=="Budget|Emissions|CO2"],aes(x=Category,y=value,shape=model,color=model,size=model)) +
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0(" Cumulative CO2 (incl. AFOLU) 2011-2050")) + ylab("Gt CO2") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
  theme(axis.text.x  = element_blank())
ggsave(file=paste0(cfg$outdir,"/","CO2tot_budget_2050","_multiregbox.pdf"),a,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")
ggsave(file=paste0(cfg$outdir,"/","CO2tot_budget_2050","_multiregbox.png"),a,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")

b=ggplot() +
  geom_boxplot(data=v_plot[Scope=="global"&variable=="CO2rel2010"],aes(x=Category,y=value, fill = Category), outlier.size = 0) +
  geom_point(data=v_plot[variable=="CO2rel2010"],aes(x=Category,y=value,shape=model,color=model,size=model)) +
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0("CO2 total (2011-2050 rel. to 2010)")) + ylab("Emission Years") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
  theme(axis.text.x  = element_blank() )
ggsave(file=paste0(cfg$outdir,"/","CO2tot_EmissionYears_2050","_multiregbox.pdf"),b,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")
ggsave(file=paste0(cfg$outdir,"/","CO2tot_EmissionYears_2050","_multiregbox.png"),b,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")

#2100
v_plot <-  filter(v_emi_cumrel2, Category %in% cats) 
v_plot$Category =  factor(v_plot$Category, levels = cats, ordered = T)
v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)
v_plot=data.table(v_plot)

c=ggplot() +
  geom_boxplot(data=v_plot[Scope=="global"&variable=="Budget|Emissions|CO2"],aes(x=Category,y=value, fill = Category), outlier.size = 0) +
  geom_point(data=v_plot[variable=="Budget|Emissions|CO2"],aes(x=Category,y=value,shape=model,color=model,size=model)) +
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0(" Cumulative CO2 (incl. AFOLU) 2011-2100")) + ylab("Gt CO2") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
  theme(axis.text.x  = element_blank())
ggsave(file=paste0(cfg$outdir,"/","CO2tot_budget_2100","_multiregbox.pdf"),c,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")
ggsave(file=paste0(cfg$outdir,"/","CO2tot_budget_2100","_multiregbox.png"),c,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")

d=ggplot() +
  geom_boxplot(data=v_plot[Scope=="global"&variable=="CO2rel2010"],aes(x=Category,y=value, fill = Category), outlier.size = 0) +
  geom_point(data=v_plot[variable=="CO2rel2010"],aes(x=Category,y=value,shape=model,color=model,size=model)) +
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0("CO2 total (2011-2100 rel. to 2010)")) + ylab("Emission Years") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
  theme(axis.text.x  = element_blank() )
ggsave(file=paste0(cfg$outdir,"/","CO2tot_EmissionYears_2100","_multiregbox.pdf"),d,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")
ggsave(file=paste0(cfg$outdir,"/","CO2tot_EmissionYears_2100","_multiregbox.png"),d,
       width=24, height=22, unit="cm", dpi=300, bg = "transparent")

# Figure 4 - implementation -----------------------------------------------
# Kaya
source("functions/plot_functions.R")
vars <- c("GDP per capita|MER","Energy Intensity of GDP|MER","Carbon Intensity of FE","Emissions per capita")
cats <- c("NPi","INDC","2020_verylow","2020_low","2020_high","2030_low","2030_high")
#ta1<-plot_bar_facet(reg="World",dt=all[period==2050],vars=vars,cats=cats,lab="Kaya factors; 2050)",file_pre="3_kaya_2050_bar")   
ta2<-plot_bar_facet2(reg="World",dt=all,year=2030,vars=vars,cats=cats,lab="Kaya factors - GHG; 2030",file_pre="kaya_GHG_2030_bar2", b.legend=T,legendorder=c("NPi","INDC","2030_high","2020_high","2030_low","2020_low","2020_verylow")) 
ta3<-plot_bar_facet2(reg="World",dt=all,year=2050,vars=vars,cats=cats,lab="Kaya factors - GHG; 2050",file_pre="kaya_GHG_2050_bar2", b.legend=T,legendorder=c("NPi","INDC","2030_high","2020_high","2030_low","2020_low","2020_verylow")) 

library(gridExtra)
g=arrangeGrob(ta2,ta3,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig3_old.png",sep=""),g,width=20,height=10,dpi=300)

#CI vs. EI
vars <- c(x="Energy intensity improvement rel. to Base",y="Carbon intensity improvement rel. to Base")
cats <- c("NoPOL","NPi","2030_low","2020_verylow")
tc<-plot_scatter(reg="World",dt=all[period<=2050],vars_to_spread=vars,cats=cats,title="Carbon Intensity vs. Energy Intensity",
                 yearlabglob=T,file_pre="ci_ei_scatter")   

#CI vs. EI relative to 2010
vars <- c(x="Energy Intensity of GDP|MER|rel2010",y="Carbon Intensity of FE|rel2010")
cats <- c("NoPOL","NPi","2030_low","2020_verylow")
td<-plot_scatter(reg="World",dt=all[period<=2050],vars_to_spread=vars,cats=cats,title="Carbon Intensity vs. Energy Intensity",
                 yearlabglob=T,file_pre="ci_ei_scatter_baseyear")   

vars <- c(x="Energy Intensity of GDP|MER|rel2010",y="Total CO2 Intensity of FE|rel2010")
cats <- c("NoPOL","NPi","2030_low","2020_verylow")
te<-plot_scatter(reg="World",dt=all[period<=2050],vars_to_spread=vars,cats=cats,title="Carbon Intensity vs. Energy Intensity",
                 yearlabglob=T,file_pre="ci_ei_scatter_baseyear_totalCO2")   

source("functions/plot_functions_xcut.R")
#CI vs. EI split out and for regions
regs <- c("BRA","CHN","EU","IND","JPN","RUS","USA","World")
cats <- c("NoPOL","NPi","2030_low","2020_verylow")
tf<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars=c("Carbon Intensity of Fuel|rel2010","Carbon Intensity of Electricity|rel2010","Energy Intensity of GDP|MER|rel2010"),
                                   cats=cats,years=2050,file_pre="CI_EI_2050_regions",ylabel="Carbon and energy intensity relative to 2010",
                                   b.multicat=F,globpoints = T,b.multivar=T,var.labels=c("CI fuel","CI electricity","EI"),ylim=c(-0.5,1.5))

#EI vs. CI split out - ternary diagram - TODO?
#library(ggtern)

# Mitigation costs
source("functions/plot_functions_xcut.R")
regs <- c("BRA","CHN","EU","IND","JPN","RUS","USA","World")
cats <- c("NPi","2030_low","2020_verylow") #"2020_high","2020_low","2030_high","2030_low"
tb1<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars="Mitigation Costs",cats=cats,years=2100,file_pre="MitiCosts_2100_mitigscens",ylabel="Mitigation costs as % of GDP (2100)",b.multicat=T,globpoints = T)
tb2<-plot_pointrange_multiScen_glob(regs=regs,dt=all,vars="Mitigation Costs",cats=cats,years=2050,file_pre="MitiCosts_2050_mitigscens",ylabel="Mitigation costs as % of GDP (2050)",b.multicat=T,globpoints=T)
g=arrangeGrob(tb2,tb1,ncol=1)
ggsave(file=paste(cfg$outdir,"/Fig4c.png",sep=""),g,width=16,height=12,dpi=300)

# Combined - figure 4
g=arrangeGrob(tc,tb2,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig4_new.png",sep=""),g,width=24,height=12,dpi=300)

g=arrangeGrob(td,tb2,ncol=2)
ggsave(file=paste(cfg$outdir,"/Fig4_newer.png",sep=""),g,width=24,height=12,dpi=300)

g=arrangeGrob(td,tf,tb2,ncol=1)
ggsave(file=paste(cfg$outdir,"/Fig4.png",sep=""),g,width=22,height=18,dpi=300)
