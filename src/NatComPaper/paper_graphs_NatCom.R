# TO DO
#- update data to V4
# - check with AIM/CGE why coal/gas w/ CCS is not in database --> can we assume zero?
# - See Final Energy.xlsx --> Subtracing biomass from electricity and transport from total biomass leads to both negative and positive numbers, 
#   check with models (but first update)

# read in 'all scenarios and historical data
keep_original=FALSE
source('NatComPaper/Data Natcom paper.R')
# read in graphical functions
source("functions/plot_functions_NatCom.R")
source("functions/plot_functions_xcut_NatCom.R")
source('../../TIMER_output/functions/pbl_colors.R')

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
library(grid)
library(scales)
library(RColorBrewer)
library(maptools)

# general settings
regs_paper<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
GWP_CH4 <- 25
GWP_N2O <- 298


# FIGURE 1

# choose selection of secenarios and variables
#cats_stack_fig1 <- c("2010", "2015","No policy","National policies","NDC")
#cats_stack_fig1 <- c("2010", "2010_model", "2015", "2015_model", "No policy","National policies","NDC")
cats_stack_fig1 <- c("2010_model", "2015_model", "No policy","National policies","NDC")
catsnat_fig1 <- c("No policy","National policies","NDC")
#vars_fig1b_h=c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|AFOLU", "Emissions|CH4","Emissions|N2O","Emissions|F-Gases")
#var.labels_fig1b_h=c("Emissions|CO2|Energy and Industrial Processes"="CO2 energy/industry","Emissions|CO2|AFOLU"="CO2 AFOLU","Emissions|CH4"="CH4","Emissions|N2O"="N2O","Emissions|F-Gases"="F-gases")
vars_fig1b_h <- c("Emissions|CO2|Energy|Supply",
                  "Emissions|CO2|Energy|Demand|Transportation",
                  "Emissions|CO2|Energy|Demand|Residential and Commercial",
                  "Emissions|CO2|Industry",
                  "Emissions|CO2|AFOLU",
                  "Emissions|Non-CO2")
var.labels_fig1b_h <- c("Emissions|CO2|Energy|Supply"="CO2 energy supply",
                        "Emissions|CO2|Energy|Demand|Transportation"="CO2 transport",
                        "Emissions|CO2|Energy|Demand|Residential and Commercial"="CO2 buildings",
                        "Emissions|CO2|Industry"="CO2 industry",
                        "Emissions|CO2|AFOLU"="AFOLU CO2",
                        "Emissions|Non-CO2"="Non-CO2")
cats_a1 <- c("NDC","National policies","No policy")
TotalEmis_var_fig1 = "Emissions|Kyoto Gases"
legend_name_fig1 <- "Greenhouse gas"
lab_fig1b_h <- bquote("Mt"~CO[2]~eq)

# select data from 'all' for figure 1
all_fig1a <- filter(all_paper, variable=="Emissions|Kyoto Gases", Category%in%cats_a1)
all_fig1a$value <- all_fig1a$value/1000 # in GtCO2eq
all_fig1a <- as.data.table(all_fig1a)
all_hist_fig1a <- filter(all_hist_paper,variable=="Emissions|Kyoto Gases")
all_hist_fig1a$value <- all_hist_fig1a$value/1000
all_hist_fig1a <- as.data.table(all_hist_fig1a)

all_fig1a <- rbind(all_fig1a, all_hist_fig1a)

# All GHG emissions are in GtCO2eq
all_fig1b_h <- all_paper[variable%in%c(vars_fig1b_h,TotalEmis_var_fig1)]
all_fig1b_h=data.table(all_fig1b_h)
all_fig1b_h[variable=="Emissions|CH4"]$value<-all_fig1b_h[variable=="Emissions|CH4"]$value*GWP_CH4
all_fig1b_h[variable=="Emissions|CH4"]$unit<-"GtCO2eq/yr"
all_fig1b_h[variable=="Emissions|N2O"]$value<-all_fig1b_h[variable=="Emissions|N2O"]$value*GWP_N2O/1000
all_fig1b_h[variable=="Emissions|N2O"]$unit<-"GtCO2eq/yr"

all_fig1b_h_hist <- all_hist_paper[variable%in%vars_fig1b_h & period%in%c(2010, 2015)]
all_fig1b_h_hist[variable=="Emissions|CH4"]$value<-all_fig1b_h_hist[variable=="Emissions|CH4"]$value*GWP_CH4
all_fig1b_h_hist[variable=="Emissions|CH4"]$unit<-"GtCO2eq/yr"
all_fig1b_h_hist[variable=="Emissions|N2O"]$value<-all_fig1b_h_hist[variable=="Emissions|N2O"]$value*(44/28)*GWP_N2O
all_fig1b_h_hist[variable=="Emissions|N2O"]$unit<-"GtCO2eq/yr"
all_fig1b_h_hist[period==2010]$Category <- "2010"
all_fig1b_h_hist[period==2015]$Category <- "2015"
all_fig1b_h_hist[period==2010]$Scope <- "global"
all_fig1b_h_hist[period==2015]$Scope <- "global"
all_fig1b_h <- rbind(all_fig1b_h, all_fig1b_h_hist)

source("functions/plot_functions_xcut_NatCom.R")
# Total GHG emissions broken up into CO2 (energy/industry), CO2 (land use), CH4, N2O, F-gases for seven large countries for NoPolicy, NPi and INDCi
# Left panel (a) global total ghg emissions
# Righ panel (b-h) GHG emissions per country, split up per greenhouse gas
a1<-plot_funnel2(reg="World",dt=all_fig1a,vars=c("Emissions|Kyoto Gases"),cats=cats_a1,start_scen=2010, title="Kyoto greenhouse gas emissions",
                file_pre="1a_GHG_funnel",glob_lines=T,xlim=c(1990,2032),ylim=c(20,71),range=T,median=T,linetypemanual=F,
                dt_hist=all_hist_fig1a, hist=T)
regs_fig1 <- c("BRA")
b1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h, var_line="", cats=cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_BRA_2030", lab = "", title=T, Title="Brazil", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

regs_fig1 <- c("CHN")
c1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_CHN_2030", lab = lab_fig1b_h, title=T, Title="China", 
                      linegraph=F, hist=T,labels=T,x_labels=T, var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T,
                      add_legend="\u25CF\u25B2 national models")

regs_fig1 <- c("IND")
d1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_IND_2030", lab = "", title=T, Title="India", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

regs_fig1 <- c("JPN")
e1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_JPN_2030", lab = "", title=T, Title="Japan", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

regs_fig1 <- c("USA")
f1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_USA_2030", lab = "", title=T, Title="USA", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

regs_fig1 <- c("EU")
g1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_EUE_2030", lab = "", title=T, Title="EU", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

regs_fig1 <- c("RUS")
h1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_RUS_2030", lab = "", title=T, Title="Russia", 
                      linegraph=F, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)

# Figure 1 together -------------------------------------------------------
tmp1_1<-ggplot_gtable(ggplot_build(a1))
leg1_1<-which(sapply(tmp1_1$grobs,function(x) x$name) =="guide-box")
legend1_1<-tmp1_1$grobs[[leg1_1]]

tmp1_2<-ggplot_gtable(ggplot_build(c1))
leg1_2<-which(sapply(tmp1_2$grobs,function(x) x$name) =="guide-box")
legend1_2<-tmp1_2$grobs[[leg1_2]]

#a1=a1+theme(legend.position = "none")
b1=b1+theme(legend.position = "none")
c1=c1+theme(legend.position = "none")
d1=d1+theme(legend.position = "none")
e1=e1+theme(legend.position = "none")
f1=f1+theme(legend.position = "none")
g1=g1+theme(legend.position = "none")
h1=h1+theme(legend.position = "none")

lay_fig1<-rbind(c(1,1,2,3,4,5),c(1,1,6,7,8,9))
h_fig1=grid.arrange(a1,c1,f1,g1,d1,b1,e1,h1, legend1_2, layout_matrix=lay_fig1)
ggsave(file=paste("NatComPaper/graphs","/Figure1_NatCom.jpg",sep=""),h_fig1,width=20,height=12,dpi=200)

# FIGURE 2
# define variables and scenarios to show in graph
vars_stack_fig2 <- c("Final Energy|Transportation", "Final Energy|Residential and Commercial","Final Energy|Industry")
vars_line_fig2 <- c("Final Energy|Non-fossil share")
#cats_stack_fig2 <- c("2005", "2010", "Historical","No policy","National policies","NDC")
cats_stack_fig2 <- c("2015", "2015_model", "No policy","National policies","NDC")
catsnat_fig2 <- c("No policy","National policies","NDC")
var.labels_fig2 <- c("Transport", "Buildings", "Industry")
legend_name_fig2 <- "Final energy"
TotalEmis_var_fig2 <- c("Final Energy")

# select data from 'all' based on above selection
#all_fig2 <- filter(all_paper, Scope=="global", variable %in% c(vars_stack_fig2, vars_line_fig2,TotalEmis_var_fig2), Category %in% cats_stack_fig2, 
#                   period==2030, region %in% regs_paper, !is.na(value))
all_fig2 <- filter(all_paper, variable %in% c(vars_stack_fig2, vars_line_fig2,TotalEmis_var_fig2), Category %in% cats_stack_fig2, 
                   region %in% regs_paper, !is.na(value))
all_fig2 <- as.data.table(all_fig2)
all_fig2[variable==vars_line_fig2]$value <- all_fig2[variable==vars_line_fig2]$value
all_fig2_tmp<-all_fig2
all_fig2_tmp$unit  <- NULL
#all_fig2_overview <- spread(all_fig2_tmp, key=variable, value=value)
#all_fig2_overview_stat <- group_by(all_fig2_overview, Category, region) %>% 
#                          summarise(median_NFShare=median(`Final Energy|Non-fossil share`), min_NFShare=min(`Final Energy|Non-fossil share`), max_NFShare=max(`Final Energy|Non-fossil share`), 
#                                    median_FE=median(`Final Energy`), min_FE=min(`Final Energy`), max_FE=max(`Final Energy`))

# add historical data
all_fig2_hist <- all_hist_paper[variable%in%c(vars_stack_fig2,vars_line_fig2) & period%in%c(2010, 2015)]
all_fig2_hist[period==2010]$Category <- "2010"
all_fig2_hist[period==2015]$Category <- "2015"
all_fig2_hist[period==2010]$Scope <- "global"
all_fig2_hist[period==2015]$Scope <- "global"
all_fig2 <- rbind(all_fig2, all_fig2_hist)
all_fig2_stat <- group_by(all_fig2, Category, region, period, variable) %>% summarise(median=median(value, na.rm=T))

palette_fig2 <- "Blues"
source("functions/plot_functions_xcut_NatCom.R")
regs_fig2 <- c("World")
a2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_WLD_2030", lab = "", lab_line="", title=T, Title="World", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

regs_fig2 <- c("BRA")
b2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_BRA_2030", lab = "", lab_line="", title=T, Title="Brazil", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)
regs_fig2 <- c("CHN")
c2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_CHN_2030", lab = "EJ/yr", lab_line="", title=T, Title="China", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2,
                      add_legend="\u25CF\u25B2 national models\n%=low carbon share")

regs_fig2 <- c("IND")
d2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_IND_2030", lab = "EJ/yr", title=T, Title="India", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

regs_fig2 <- c("JPN")
e2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_JPN_2030", lab = "", title=T, Title="Japan", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

regs_fig2 <- c("USA")
f2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_USA2030", lab = "", title=T, Title="USA", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

regs_fig2 <- c("EU")
g2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_EUE_2030", lab = "", title=T, Title="European Union", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

regs_fig2 <- c("RUS")
h2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_RUS_2030", lab = "", title=T, Title="Russia", 
                      linegraph=T, hist=T,labels=T,x_labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

# Figure 2 together -------------------------------------------------------
tmp2_1<-ggplot_gtable(ggplot_build(c2))
leg2_1<-which(sapply(tmp2_1$grobs,function(x) x$name) =="guide-box")
legend2_1<-tmp2_1$grobs[[leg2_1]]

a2=a2+theme(legend.position = "none")
b2=b2+theme(legend.position = "none")
c2=c2+theme(legend.position = "none")
d2=d2+theme(legend.position = "none")
e2=e2+theme(legend.position = "none")
f2=f2+theme(legend.position = "none")
g2=g2+theme(legend.position = "none")
h2=h2+theme(legend.position = "none")

lay_fig2<-rbind(c(1,2,3,4,5),c(6,7,8,9,10))
h_fig2=grid.arrange(a2,c2,f2,g2,d2,b2,e2,h2,legend2_1, layout_matrix=lay_fig2) #ncol=3
ggsave(file=paste("NatComPaper/graphs","/Figure2_NatCom.jpg",sep=""),h_fig2,width=20,height=12,dpi=200)

source("functions/plot_functions_xcut_NatCom.R")
source("functions/plot_functions_NatCom.R")
# Combine figure 1 and 2
a1<-plot_funnel2(reg="World",dt=all_fig1a,vars=c("Emissions|Kyoto Gases"),cats=cats_a1,start_scen=2010, title="Kyoto greenhouse gas emissions",
                 file_pre="1a_GHG_funnel",glob_lines=T,xlim=c(1990,2032),ylim=c(20,71),range=T,median=T,linetypemanual=F,
                 dt_hist=all_hist_fig1a, hist=T)
regs_fig1 <- c("CHN")
c1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_CHN_2030", lab = lab_fig1b_h, title=T, Title="China", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T,
                      add_legend="\u25CF\u25B2 national models")
regs_fig1 <- c("IND")
d1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_IND_2030", lab = "", title=T, Title="India", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)
regs_fig1 <- c("USA")
f1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_USA_2030", lab = "", title=T, Title="USA", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)
regs_fig1 <- c("EU")
g1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars_stack=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_EUE_2030", lab = "", title=T, Title="EU", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=T)
regs_fig2 <- c("CHN")
c2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_CHN_2030", lab = "EJ/yr", lab_line="", title=T, Title="", 
                      linegraph=T, hist=T,labels=T,x_labels=T, var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2,
                      add_legend="%=low carbon share")

regs_fig2 <- c("IND")
d2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_IND_2030", lab = "", title=T, Title="", 
                      linegraph=T, hist=T,labels=T,x_labels=T, var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)
regs_fig2 <- c("USA")
f2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_USA2030", lab = "", title=T, Title="", 
                      linegraph=T, hist=T,labels=T,x_labels=T, var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)
regs_fig2 <- c("EU")
g2<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_EUE_2030", lab = "", title=T, Title="", 
                      linegraph=T, hist=T,labels=T, x_labels=T, var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=T, error_bar=T,quantiles=T, color_brewer_palette = palette_fig2)

tmp1_1<-ggplot_gtable(ggplot_build(a1))
leg1_1<-which(sapply(tmp1_1$grobs,function(x) x$name) =="guide-box")
legend1_1<-tmp1_1$grobs[[leg1_1]]

tmp1_2<-ggplot_gtable(ggplot_build(c1))
leg1_2<-which(sapply(tmp1_2$grobs,function(x) x$name) =="guide-box")
legend1_2<-tmp1_2$grobs[[leg1_2]]

tmp2_1<-ggplot_gtable(ggplot_build(c2))
leg2_1<-which(sapply(tmp2_1$grobs,function(x) x$name) =="guide-box")
legend2_1<-tmp2_1$grobs[[leg2_1]]

#a1=a1+theme(legend.position = "none")
c1=c1+theme(legend.position = "none")
d1=d1+theme(legend.position = "none")
f1=f1+theme(legend.position = "none")
g1=g1+theme(legend.position = "none")
c2=c2+theme(legend.position = "none")
d2=d2+theme(legend.position = "none")
f2=f2+theme(legend.position = "none")
g2=g2+theme(legend.position = "none")

lay_fig12<-rbind(c(1,1,2,3,4,5,6),c(1,1,7,8,9,10,11))
h_fig12=grid.arrange(a1,c1,d1,f1,g1,legend1_2, c2,d2,f2, g2, legend2_1, layout_matrix=lay_fig12)
ggsave(file=paste("NatComPaper/graphs","/Figure1_alt_GHG_FE_NatCom.jpg",sep=""),h_fig12,width=20,height=12,dpi=200)

# FIGURE 3
# - Emissions gap
# - Non-fossil gap
# - CO2-intensity gap

# ADD PERCENTAGE REDUCTION NEXT TO ARROW

cats_fig3 <- c('National policies', 'Carbon budget 1000', 'Carbon budget 400')
regs_fig3 <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
#regs_fig3 <- c("World", "CHN", "USA", "EU",  "IND")
vars_fig3 <- c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy intensity of GDP")

gaps <- c("2C", "1.5C")

data_fig3 <- filter(all_paper, Scope=="global", Category %in% cats_fig3, region %in% regs_fig3, period >= 2010, period<=2030, variable %in% vars_fig3, !is.na(value))
data_fig3$variable <- factor(data_fig3$variable, levels=vars_fig3)
data_fig3$region <- factor(data_fig3$region, levels=regs_fig3)
data_fig3_stat <-  group_by(data_fig3, Category, region, period, variable) %>%
                  summarise(median=median(value), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T))

d1 <- filter(data_fig3, Category %in% c("National policies"), period==2030) %>% select(Category, model, region, value, variable)
d2 <- filter(data_fig3, Category %in% c("Carbon budget 1000"), period==2030) %>% select(Category, model, region, value, variable)
d3 <- filter(data_fig3, Category %in% c("Carbon budget 400"), period==2030) %>% select(Category, model, region, value, variable)
TwoC_gap <- inner_join(d1, d2, by=c('model', 'region', 'variable')) %>%
            mutate(start=value.x) %>%
            mutate(gap=value.x-value.y) %>%
            select(model, region, variable, start, gap)
TwoC_gap_stat <- group_by(TwoC_gap, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))
OnePointFiveC_gap <- inner_join(d1, d3, by=c('model', 'region', 'variable')) %>%
                     mutate(start=value.x) %>%
                     mutate(gap=value.x-value.y) %>%
                     select(model, region, variable, start, gap)
OnePointFiveC_gap_stat <- group_by(OnePointFiveC_gap, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))

Gap_stat <- rbind(mutate(TwoC_gap_stat, gap="2C"), mutate(OnePointFiveC_gap_stat, gap="1.5C"))
Gap_stat$gap <- factor(Gap_stat$gap, levels=gaps)

reg_labels <- c("World"="World", "BRA"="Brazil",  "CHN"="China", "EU"="European Union",  "IND"="India", "JPN"="Japan", "RUS"="Russia", "USA"="USA")
var_labels <- c("Emissions|Kyoto Gases"="GHG emissions (MtCO2eq))", "Final Energy|Non-fossil share"="Low carbon share (%)", 
                "Energy intensity of GDP"="Energy intensity (TJ/US$2010)")
colours_fig3 <- brewer.pal(5,"Accent")
names(colours_fig3) <- levels(cats_fig3)
data_fig3_stat <- mutate(data_fig3_stat, ymin=0)
data_fig3_stat <- mutate(data_fig3_stat, ymax=ifelse(variable=="Emissions|Kyoto Gases", NA, ifelse(variable=="Final Energy|Non-fossil share", 50, 25)))

fig3 <- ggplot(data=data_fig3_stat) + geom_line(aes(period, median, colour=Category), size=2) + 
                   geom_ribbon(aes(x=period,ymin=perc_10,ymax=perc_90,fill=Category),alpha=.15, show.legend = F) +
                   geom_segment(data=filter(Gap_stat, gap=="1.5C"), mapping=aes(x=2032, y=start_median, xend=2032, yend=start_median-gap_median, linetype=gap), 
                                arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
                   geom_segment(data=filter(Gap_stat, gap=="2C"), mapping=aes(x=2031, y=start_median, xend=2031, yend=start_median-gap_median, linetype=gap), 
                                arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
                   facet_wrap(variable~region, scales = "free_y", nrow=3, labeller=labeller(variable = var_labels, region=reg_labels)) +
                   #facet_grid(variable~region, scales = "free_y", labeller=labeller(variable = var_labels, region=reg_labels)) +
                   #ylim(0,NA) +
                   #https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y/42590452
                   geom_blank(aes(y = data_fig3_stat$ymin))+
                   geom_blank(aes(y = data_fig3_stat$ymax))+
                   xlab("year") + ylab("") +
                   scale_linetype_discrete(name="Median gap with", breaks=gaps, labels=c("2° C", "1.5° C")) +                 
                   scale_colour_manual(name= "Scenario", values=colours_fig3, labels=c("National policies"="National policies", "Carbon budget 1000"="2° C", "Carbon budget 400"="1.5° C"),
                                       guide = guide_legend(reverse=TRUE)) +
                   scale_fill_manual(values=colours_fig3) +
  
                   guides(color = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
                   theme_bw() +
                   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                   theme(strip.text.x = element_text(size=8, face="bold"),
                         strip.text.y = element_text(size=8, face="bold"),
                         strip.background = element_rect(colour="black", fill="white"))
ggsave(file=paste("NatComPaper/graphs","/Figure3_NatCom.jpg",sep=""),fig3,width=20,height=12,dpi=200)

# FIGURE 3a and 3b (ALTERNATIVE)

# ALTERNATIVE Figure 3a
cats_fig3 <- c('National policies', 'Carbon budget 1000', 'Carbon budget 400')
regs_fig3 <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
#regs_fig3 <- c("World", "CHN", "USA", "EU",  "IND")
vars_fig3a <- c("Emissions|Kyoto Gases", 
                #"Emissions|CO2", 
                "Emissions|CO2|Energy", 
                "Emissions|CO2|Industrial Processes", 
                "Emissions|CO2|AFOLU",
                "Emissions|Non-CO2")
gaps <- c("2C", "1.5C")

data_fig3a <- filter(all_paper, Scope=="global", Category %in% cats_fig3, region %in% regs_fig3, period >= 2010, period<=2030, variable %in% vars_fig3a, !is.na(value))
data_fig3a$variable <- factor(data_fig3a$variable, levels=vars_fig3a)
data_fig3a$region <- factor(data_fig3a$region, levels=regs_fig3)
data_fig3a_stat <-  group_by(data_fig3a, Category, region, period, variable) %>%
                    summarise(median=median(value), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T))

data_fig3a_hist <- filter(all_hist_paper, region %in% regs_fig3, variable %in% vars_fig3a, period >= 2005, !is.na(value))

# determine emissions gap between national policies and 2C/1.5C
d1a <- filter(data_fig3a, Category %in% c("National policies"), period==2030) %>% select(Category, model, region, value, variable)
d2a <- filter(data_fig3a, Category %in% c("Carbon budget 1000"), period==2030) %>% select(Category, model, region, value, variable)
d3a <- filter(data_fig3a, Category %in% c("Carbon budget 400"), period==2030) %>% select(Category, model, region, value, variable)
TwoC_gap_a <- inner_join(d1a, d2a, by=c('model', 'region', 'variable')) %>%
  mutate(start=value.x) %>%
  mutate(gap=value.x-value.y) %>%
  select(model, region, variable, start, gap)
TwoC_gap_a_stat <- group_by(TwoC_gap_a, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))
OnePointFiveC_gap_a <- inner_join(d1a, d3a, by=c('model', 'region', 'variable')) %>%
  mutate(start=value.x) %>%
  mutate(gap=value.x-value.y) %>%
  select(model, region, variable, start, gap)
OnePointFiveC_gap_a_stat <- group_by(OnePointFiveC_gap_a, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))

# determine percentage decrease between national policies and 2C/1.5C
d4a <- filter(data_fig3a, Category %in% c("National policies", "Carbon budget 1000"), period==2030) %>% 
  select(Category, model, region, value, variable) %>%
  spread(key=Category, value=value) %>% 
  #mutate(gap = -1*(`Carbon budget 1000`/`National policies`-1)) %>%
  mutate(gap = (-1/1000)*(`Carbon budget 1000`-`National policies`)) %>%
  mutate(y_pos = `National policies`) %>%
  mutate(Category="Carbon budget 1000") %>%
  select(Category, model, region, gap, y_pos, variable)
d5a <- filter(data_fig3a, Category %in% c("National policies", "Carbon budget 400"), period==2030) %>% 
  select(Category, model, region, value, variable) %>%
  spread(key=Category, value=value) %>% 
  #mutate(gap = -1*(`Carbon budget 400`/`National policies`-1)) %>%
  mutate(gap = (-1/1000)*(`Carbon budget 400`-`National policies`)) %>%
  mutate(y_pos = `National policies`) %>%
  mutate(Category="Carbon budget 400") %>%
  select(Category, model, region, gap, y_pos, variable)

TwoC_gap2_a_stat <- group_by(d4a, region, variable) %>% summarise(gap_median = median(gap, na.rm=TRUE), y_pos_median=max(y_pos, na.rm=TRUE))
OnePointFiveC_gap2_a_stat <- group_by(d5a, region, variable) %>% summarise(gap_median = median(gap, na.rm=TRUE), y_pos_median=max(y_pos, na.rm=TRUE))

Gap_a_stat <- rbind(mutate(TwoC_gap_a_stat, gap="2C"), mutate(OnePointFiveC_gap_a_stat, gap="1.5C"))
Gap_a_stat$gap <- factor(Gap_a_stat$gap, levels=gaps)

reg_labels <- c("World"="World", "BRA"="Brazil",  "CHN"="China", "EU"="European Union",  "IND"="India", "JPN"="Japan", "RUS"="Russia", "USA"="USA")
var_labels_a <- c("Emissions|Kyoto Gases"="Total GHG", 
                  #"Emissions|CO2"="CO2", 
                  "Emissions|CO2|Energy"="CO2 energy", 
                  "Emissions|CO2|Industrial Processes"="CO2 industrial processes", 
                  "Emissions|CO2|AFOLU"="CO2 AFOLU", 
                  "Emissions|Non-CO2"="Non-CO2")
colours_fig3 <- brewer.pal(5,"Accent")
names(colours_fig3) <- levels(cats_fig3)
data_fig3a_stat <- mutate(data_fig3a_stat, ymin=0.0)
data_fig3a_stat <- mutate(data_fig3a_stat, ymax=NA)
data_fig3a_stat$ymin <- as.double(data_fig3a_stat$ymin)
data_fig3a_stat$ymax <- as.double(data_fig3a_stat$ymax)

data_fig3a_stat$variable <- factor(data_fig3a_stat$variable, levels=vars_fig3a)
data_fig3a_hist$variable <- factor(data_fig3a_hist$variable, levels=vars_fig3a)
#Gap_a_stat$variable <- factor(Gap_a_stat$variable, levels=vars_fig3a)
#TwoC_gap_perc_a_stat$variable <- factor(TwoC_gap_perc_a_stat, levels=vars_fig3a)
#OnePointFiveC_gap_perc_a_stat$variable <- factor(OnePointFiveC_gap_perc_a_stat$variable, levels=vars_fig3a)

fig3a <- ggplot(data=data_fig3a_stat) + geom_line(aes(period, median, colour=Category), size=2) + 
  geom_ribbon(aes(x=period,ymin=perc_10,ymax=perc_90,fill=Category),alpha=.15, show.legend = F) +
  geom_line(data=data_fig3a_hist, aes(x=period,y=value, colour="History")) +
  geom_segment(data=filter(Gap_a_stat, gap=="2C"), mapping=aes(x=2031, y=start_median, xend=2031, yend=start_median-gap_median, linetype=gap), 
               arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
  geom_segment(data=filter(Gap_a_stat, gap=="1.5C"), mapping=aes(x=2035, y=start_median, xend=2035, yend=start_median-gap_median, linetype=gap), 
               arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
  #geom_text(data=TwoC_gap_perc_a_stat, aes(x=2031, y=y_pos_median, label=paste0(round(100*gap_median,0), "%")), size=2.5) +
  #geom_text(data=OnePointFiveC_gap_perc_a_stat, aes(x=2035, y=y_pos_median, label=paste0(round(100*gap_median,0), "%")), size=2.5) +
  geom_text(data=TwoC_gap2_a_stat, aes(x=2031, y=y_pos_median, label=paste0(round(gap_median,1), "")), size=2.5) +
  geom_text(data=OnePointFiveC_gap2_a_stat, aes(x=2035, y=0.9*y_pos_median, label=paste0(round(gap_median,1), "")), size=2.5) +
  facet_wrap(variable~region, scales = "free_y", nrow=5, labeller=labeller(variable = var_labels_a, region=reg_labels)) +
  #https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y/42590452
  geom_blank(aes(y = data_fig3a_stat$ymin))+
  geom_blank(aes(y = data_fig3a_stat$ymax))+
  xlab("year") + ylab("") +
  scale_linetype_discrete(name="Median gap with", breaks=gaps, labels=c("2° C", "1.5° C")) +                 
  scale_colour_manual(name= "Scenario", values=colours_fig3, labels=c("History", "National policies"="National policies", "Carbon budget 1000"="2° C", "Carbon budget 400"="1.5° C"),
                      guide = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=colours_fig3) +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        strip.background = element_rect(colour="black", fill="white"))
ggsave(file=paste("NatComPaper/graphs","/Figure3a_NatCom.jpg",sep=""),fig3a,width=20,height=12,dpi=200)

# ALTERNATIVE FIGURE 3B
kaya_co2 <- FALSE
if (kaya_co2)
{ vars_fig3b <- c("Energy intensity of GDP", 
                 "Energy utilisation rate",
                 "Final Energy|Non-fossil share", 
                 "Carbon intensity of fossil-fuel use")
  var_labels_b <- c("Energy intensity of GDP"="TPES/GDP", 
                    "Energy utilisation rate"="FE/TPES",
                    "Final Energy|Non-fossil share"="Low carbon share", 
                    "Carbon intensity of fossil-fuel use"="CO2/FE_ff")
} else {vars_fig3b <- c("Final Energy", 
               "Final Energy|Non-fossil share", 
                "Fossil fuel utilisation rate",
                "Carbon intensity of fossil-fuel use")
var_labels_b <- c("Final Energy"="FE", 
                  "Final Energy|Non-fossil share"="FE_REN/FE", 
                  "Fossil fuel utilisation rate"="TPES_ff/FF_ff",
                 "Carbon intensity of fossil-fuel use"="CO2/FE_ff")
}              
reg_labels <- c("World"="World", "BRA"="Brazil",  "CHN"="China", "EU"="European Union",  "IND"="India", "JPN"="Japan", "RUS"="Russia", "USA"="USA")

data_fig3b <- filter(all_paper, Scope=="global", Category %in% cats_fig3, region %in% regs_fig3, period >= 2010, period<=2030, variable %in% vars_fig3b, !is.na(value))
data_fig3b$variable <- factor(data_fig3b$variable, levels=vars_fig3b)
data_fig3b$region <- factor(data_fig3b$region, levels=regs_fig3)
data_fig3b_stat <-  group_by(data_fig3b, Category, region, period, variable) %>%
  summarise(median=median(value), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T))

data_fig3b_hist <- filter(all_hist_paper, region %in% regs_fig3, variable %in% vars_fig3b, period >= 2005, !is.na(value))

d1b <- filter(data_fig3b, Category %in% c("National policies"), period==2030) %>% select(Category, model, region, value, variable)
d2b <- filter(data_fig3b, Category %in% c("Carbon budget 1000"), period==2030) %>% select(Category, model, region, value, variable)
d3b <- filter(data_fig3b, Category %in% c("Carbon budget 400"), period==2030) %>% select(Category, model, region, value, variable)
TwoC_gap_b <- inner_join(d1b, d2b, by=c('model', 'region', 'variable')) %>%
  mutate(start=value.x) %>%
  mutate(gap=value.x-value.y) %>%
  select(model, region, variable, start, gap)
TwoC_gap_b_stat <- group_by(TwoC_gap_b, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))
OnePointFiveC_gap_b <- inner_join(d1b, d3b, by=c('model', 'region', 'variable')) %>%
  mutate(start=value.x) %>%
  mutate(gap=value.x-value.y) %>%
  select(model, region, variable, start, gap)
OnePointFiveC_gap_b_stat <- group_by(OnePointFiveC_gap_b, region, variable) %>% summarise(start_median = median(start), gap_median = median(gap))

Gap_b_stat <- rbind(mutate(TwoC_gap_b_stat, gap="2C"), mutate(OnePointFiveC_gap_b_stat, gap="1.5C"))
Gap_b_stat$gap <- factor(Gap_b_stat$gap, levels=gaps)

# determine percentage decrease between national policies and 2C/1.5C
d4b <- filter(data_fig3b, Category %in% c("National policies", "Carbon budget 1000"), period==2030) %>% 
  select(Category, model, region, value, variable) %>%
  spread(key=Category, value=value) %>% 
  #mutate(gap = -1*(`Carbon budget 1000`/`National policies`-1)) %>%
  mutate(gap = -1*(`Carbon budget 1000`-`National policies`)) %>%
  mutate(y_pos = pmax(`National policies`, `Carbon budget 1000`)) %>%
  mutate(Category="Carbon budget 1000") %>%
  select(Category, model, region, gap, y_pos, variable)
d5b <- filter(data_fig3b, Category %in% c("National policies", "Carbon budget 400"), period==2030) %>% 
  select(Category, model, region, value, variable) %>%
  spread(key=Category, value=value) %>% 
  #mutate(gap = -1*(`Carbon budget 400`/`National policies`-1)) %>%
  mutate(gap = -1*(`Carbon budget 400`-`National policies`)) %>%
  mutate(y_pos = pmax(`National policies`,`Carbon budget 400`)) %>%
  mutate(Category="Carbon budget 400") %>%
  select(Category, model, region, gap, y_pos, variable)

max_EI = 25
max_CE = 1.2
max_UR = 1.25
max_NF = 40
max_CF = 200

TwoC_gap2_b_stat <- group_by(d4b, region, variable) %>% summarise(gap_median = median(gap, na.rm=TRUE), 
                                                                      y_pos_median=max(y_pos, na.rm=TRUE))

OnePointFiveC_gap2_b_stat <- group_by(d5b, region, variable) %>% summarise(gap_median = median(gap, na.rm=TRUE), y_pos_median=max(y_pos, na.rm=TRUE))

colours_fig3 <- brewer.pal(6,"Accent")
names(colours_fig3) <- levels(c("History", cats_fig3))
data_fig3b_stat <- mutate(data_fig3b_stat, ymin=0.0)
data_fig3b_stat <- mutate(data_fig3b_stat, ymax=ifelse(variable=="Energy intensity of GDP", max_EI,
                                                ifelse(variable=="Conversion efficiency", max_CE, 
                                                ifelse(variable=="Fossil fuel utilisation rate", max_UR,
                                                ifelse(variable=="Energy utilisation rate", max_UR, 
                                                ifelse(variable=="Final Energy|Non-fossil share", max_NF, 
                                                ifelse(variable=="Carbon intensity of fossil-fuel use", max_CF, NA)))))))
data_fig3b_stat$ymin <- as.double(data_fig3b_stat$ymin)
data_fig3b_stat$ymax <- as.double(data_fig3b_stat$ymax)
data_fig3b_stat$variable <- factor(data_fig3b_stat$variable, levels=vars_fig3b)
data_fig3b_hist$variable <- factor(data_fig3b_hist$variable, levels=vars_fig3b)

if (kaya_co2) k<-"CO2" else k <- "FE"
fig3b <- ggplot(data=data_fig3b_stat) + geom_line(aes(period, median, colour=Category), size=2) + 
  geom_ribbon(aes(x=period,ymin=perc_10,ymax=perc_90,fill=Category),alpha=.15, show.legend = F) +
  geom_line(data=data_fig3b_hist, aes(x=period,y=value, colour="History")) +
  geom_segment(data=filter(Gap_b_stat, gap=="1.5C"), mapping=aes(x=2032, y=start_median, xend=2032, yend=start_median-gap_median, linetype=gap), 
               arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
  geom_segment(data=filter(Gap_b_stat, gap=="2C"), mapping=aes(x=2031, y=start_median, xend=2031, yend=start_median-gap_median, linetype=gap), 
               arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
  #geom_text(data=TwoC_gap2_b_stat, aes(x=2031, y=y_pos_median, label=paste0(round(100*gap_median,0), "%")), size=2.5) +
  #geom_text(data=OnePointFiveC_gap2_b_stat, aes(x=2035, y=y_pos_median, label=paste0(round(100*gap_median,0), "%")), size=2.5) +
  geom_text(data=TwoC_gap2_b_stat, aes(x=2031, y=y_pos_median, label=paste0(round(gap_median,1), "")), size=2.5) +
  geom_text(data=OnePointFiveC_gap2_b_stat, aes(x=2035, y=y_pos_median, label=paste0(round(gap_median,1), "")), size=2.5) +
  facet_wrap(variable~region, scales = "free_y", nrow=4, labeller=labeller(variable = var_labels_b, region=reg_labels)) +
  #https://stackoverflow.com/questions/42588238/setting-individual-y-axis-limits-with-facet-wrap-not-with-scales-free-y/42590452
  geom_blank(aes(y = data_fig3b_stat$ymin))+
  geom_blank(aes(y = data_fig3b_stat$ymax))+
  xlab("year") + ylab("") +
  scale_linetype_discrete(name="Median gap with", breaks=gaps, labels=c("2° C", "1.5° C")) +                 
  scale_colour_manual(name= "Scenario", values=colours_fig3, labels=c("History"="History", "National policies"="National policies", "Carbon budget 1000"="2° C", "Carbon budget 400"="1.5° C"),
                      guide = guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=colours_fig3) +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.text.x = element_text(size=8, face="bold"),
        strip.text.y = element_text(size=8, face="bold"),
        strip.background = element_rect(colour="black", fill="white"))
ggsave(file=paste("NatComPaper/graphs","/Figure3b_", k, "_NatCom.jpg",sep=""),fig3b,width=20,height=12,dpi=200)

# FIGURE 4
# Carbon budgets ------------------------------------------------

#specify plot scope
regs_fig3 <- c("BRA","CHN","IND","EU","JPN","USA","RUS","RoW","World")
#regs_fig3 <- c("CHN","USA", "EU", "IND", "World")
mods_fig3 <- unique(all$model)
vars_fig3 <- "Emissions|CO2"
cats_fig3 <- c("No policy","National policies","NDC","Carbon budget 1000","Carbon budget 400")
cats_fig3_nat <- c("No policy", "National policies", "NDC")

# remove outliers due to different region definition or limited policy implementaiton
dt_all <- all_paper
dt_all[model == "COPPE-COFFEE 1.0" & region == "EU", "value"] <- NA
dt_all[model == "DNE21+ V.14" & (region == "CHN" | region == "IND"), "value"] <- NA
dt_all[model == "MESSAGE" & region == "EU", "value"] <- NA

#calculate emissions and 2050 budgets
v_emireg <- dt_all %>%
  filter (variable %in% vars_fig3 & region %in% regs_fig3 & !is.na(value) & Category %in% cats_fig3 & model %in% mods_fig3) %>%
  #mutate(value = value / 1000, unit = "GtCO2/yr") %>%
  factor.data.frame()
v_emireg=data.table(v_emireg)
national=v_emireg[Scope=="national"]
v_emireg =  spread(v_emireg[Scope=="global"],key = region, value = value,fill=0) 
v_emireg = v_emireg%>%mutate (RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` )%>%
  gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`)%>%
  filter(!value==0)
#v_emireg = v_emireg%>%mutate (RoW = `World` - `CHN` - `IND` - `EU` - `USA`)%>%
#  gather(region,value,`RoW`, `World`, `CHN`, `IND`, `EU`, `USA`)%>%
#  filter(!value==0)
setcolorder(v_emireg,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
v_emireg=rbind(v_emireg,national)
v_emireg <- as.data.table(v_emireg)
v_emireg$period <- as.numeric(as.character(v_emireg$period))
v_budgreg <- calcBudget(data = v_emireg,var = vars_fig3,new_var = paste0("Budget|",vars_fig3))

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

write.table(v_emi_cumrel, file = "NatComPaper/EmissionBudgets.csv", sep=";", row.names = F)

# PDF-style 
theme_set(ggplot2::theme_bw(base_size = 15))

#2050
v_plot <-  filter(v_emi_cumrel, Category %in% cats_fig3) 
v_plot$Category =  factor(v_plot$Category, levels = cats_fig3, ordered = T)
v_plot$region =  factor(v_plot$region, levels = regs_fig, ordered = T)
v_plot=data.table(v_plot)

#Leave out GEM-E3 as national point for EU - upon request model team
v_plot=v_plot[!c(model=="*GEM-E3"&region=="EU")]

#do selection of only catsnat for national models here, as | doesn't work
v_plot=rbind(v_plot[Scope=="national"&Category%in%cats_fig3_nat],v_plot[Scope=="global"])

b=ggplot() +
  geom_boxplot(data=v_plot[Scope=="global"&variable=="CO2rel2010"],aes(x=Category,y=value, fill = Category), outlier.size = 0) +
  geom_point(data=v_plot[variable=="CO2rel2010"],aes(x=Category,y=value,shape=model,color=model,size=model)) + # coord_cartesian(ylim = c(0, 100)) + & (Scope=="global" | Scope=="national" & Category%in%cats_fig3_nat)
  # facet_wrap(~region, scales = "fixed") +
  facet_wrap(~region, scales = "free_y") +
  #ggtitle(expression(paste("Cumulative CO"[2], " emissions (2011-2050) relative to 2010"))) + 
  labs(title=expression(paste("Cumulative CO"[2], " emissions in period 2011-2050")), subtitle="relative to 2010")+
  xlab("") + ylab("")  +
  scale_color_manual(values = c(rep("black",9),rep("orangered3",11)),name="Model")+
  scale_shape_manual(values = rep(seq(1,11),2),name="Model") + #c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9)
  scale_size_manual(values = c(rep(1,11),rep(3,9)),name="Model") +
  scale_fill_manual(values=plotstyle(cats_fig3), name="Scenario")+
  theme(axis.text.x  = element_blank() )
ggsave(file=paste0("NatComPaper/graphs","/","Figure4_NatCom.jpg"),b,
       width=24, height=22, unit="cm", dpi=200, bg = "transparent")

