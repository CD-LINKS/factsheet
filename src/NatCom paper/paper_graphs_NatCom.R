# TO DO
#- update data to V4
# - check with AIM/CGE why coal/gas w/ CCS is not in database --> can we assume zero?
# - See Final Energy.xlsx --> Subtracing biomass from electricity and transport from total biomass leads to both negative and positive numbers, 
#   check with models (but first update)

# set working dir
#setwd("/mnt/scratch/roelfsemam/MarkR/CD_LINKS/6_R/factsheet_oct2017/src")
setwd("~/disks/y/Kennisbasis/image/model/Timer/ontwapps_Timer/Users/Mathijs/Projects/CD-LINKS/CD-LINKS/6_R/factsheet_oct2017/src")
# read in 'all
source('main_WP2_3_indicators.R')
source("functions/CreateHistoricalData.R")
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
regs<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
GWP_CH4 <- 25
GWP_N2O <- 298

# create historical data
source("functions/CreateHistoricalData.R")

# read in graphical functions
source("functions/plot_functions_NatCom.R")
source("functions/plot_functions_xcut_NatCom.R")


# Figure 1

# Figure 1a - time series -------------------------------------------------
# Total GHG emissions on a global level for NoPolicy, NPi and INDCi
source("functions/plot_functions_NatCom.R")
source("functions/plot_functions_xcut_NatCom.R")

# choose selection of secenarios and variables
cats_stack_fig1 <- c("2005", "2010","No policy","National policies","NDC")
catsnat_fig1 <- c("No policy","National policies","NDC")
vars_fig1b_h=c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|AFOLU", "Emissions|CH4","Emissions|N2O","Emissions|F-Gases")
var.labels_fig1b_h=c("Emissions|CO2|Energy and Industrial Processes"="CO2 Energy & Industry","Emissions|CO2|AFOLU"="CO2 AFOLU","Emissions|CH4"="CH4","Emissions|N2O"="N2O","Emissions|F-Gases"="F-gases")
legend_name_fig1 <- "Greenhouse gas"
cats_a1 <- c("NDC","National policies","No policy")
TotalEmis_var_fig1 = "Emissions|Kyoto Gases"

# select data from 'all' for figure 2
all_fig1 <- filter(all, variable %in% c(vars_fig1b_h, "Emissions|Kyoto Gases"))
all_fig1$value <- all_fig1$value/1000 # in GtCO2eq

dt_hist_fig1 <- all_hist[variable=="Emissions|Kyoto Gases"]
dt_hist_fig1$value <- dt_hist_fig1$value/1000
dt_fig1a <- all[variable=="Emissions|Kyoto Gases" & Category%in%cats_a1]
dt_fig1a$value <- dt_fig1a$value/1000

# All GHG emissions are in GtCO2eq
all_fig1b_h <- all[variable%in%c(vars_fig1b_h,TotalEmis_var_fig1)]
all_fig1b_h=data.table(all_fig1b_h)
all_fig1b_h[variable=="Emissions|CH4"]$value<-all_fig1b_h[variable=="Emissions|CH4"]$value*GWP_CH4
all_fig1b_h[variable=="Emissions|CH4"]$unit<-"GtCO2eq/yr"
all_fig1b_h[variable=="Emissions|N2O"]$value<-all_fig1b_h[variable=="Emissions|N2O"]$value*GWP_N2O/1000
all_fig1b_h[variable=="Emissions|N2O"]$unit<-"GtCO2eq/yr"

all_fig1b_h_hist <- all_hist[variable%in%vars_fig1b_h & period%in%c(2005, 2010)]
all_fig1b_h_hist[variable=="Emissions|CH4"]$value<-all_fig1b_h_hist[variable=="Emissions|CH4"]$value*GWP_CH4
all_fig1b_h_hist[variable=="Emissions|CH4"]$unit<-"GtCO2eq/yr"
all_fig1b_h_hist[variable=="Emissions|N2O"]$value<-all_fig1b_h_hist[variable=="Emissions|N2O"]$value*(44/28)*GWP_N2O
all_fig1b_h_hist[variable=="Emissions|N2O"]$unit<-"GtCO2eq/yr"
all_fig1b_h_hist[period==2005]$Category <- "2005"
all_fig1b_h_hist[period==2010]$Category <- "2010"
all_fig1b_h_hist[period==2005]$Scope <- "global"
all_fig1b_h_hist[period==2010]$Scope <- "global"
all_fig1b_h <- rbind(all_fig1b_h, all_fig1b_h_hist)

# Total GHG emissions broken up into CO2 (energy/industry), CO2 (land use), CH4, N2O, F-gases for seven large countries for NoPolicy, NPi and INDCi
# Left panel (a) global total ghg emissions
# Righ panel (b-h) GHG emissions per country, split up per greenhouse gas
a1<-plot_funnel2(reg="World",dt=dt_fig1a,vars=c("Emissions|Kyoto Gases"),cats=cats_a1,start_scen=2010, title="Kyoto greenhouse gas emissions",
                file_pre="1a_GHG_funnel",glob_lines=T,xlim=c(1990,2032),ylim=c(20,71),range=T,median=T,linetypemanual=F,
                dt_hist=dt_hist_fig1, hist=T)
source("functions/plot_functions_xcut_NatCom.R")
regs_fig1 <- c("BRA")
#b1<-plot_stackbar_ghg(regs=regs_fig1,dt=all_fig1b_h,vars_stack=vars_fig1b_h,var_line="",
#                      cats = cats_stack_fig1, per=c(2030),file_pre="1b_BRA_2030",lab = "Mt CO2eq/yr",title=T,Title="Brazil",
#                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h,legend_name=legend_name_fig1,TotalEmis_var=TotalEmis_var_fig1,
#                      natpoints=T,catsnat_fig1, error_bar=T,quantiles=F)

b1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_BRA_2030", lab = "Mt CO2eq/yr", title=T, Title="Brazil", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("CHN")
c1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_CHN_2030", lab = "Mt CO2eq/yr", title=T, Title="China", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("IND")
d1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_IND_2030", lab = "Mt CO2eq/yr", title=T, Title="India", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("JPN")
e1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_JPN_2030", lab = "Mt CO2eq/yr", title=T, Title="Japan", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("USA")
f1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_USA_2030", lab = "Mt CO2eq/yr", title=T, Title="USA", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("EU")
g1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_EUE_2030", lab = "Mt CO2eq/yr", title=T, Title="EU", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

regs_fig1 <- c("RUS")
h1<-plot_stackbar_ghg(regs=regs_fig1, dt=all_fig1b_h, vars=vars_fig1b_h,var_line="", cats = cats_stack_fig1, catsnat=catsnat_fig1, 
                      per=c(2030), file_pre="1b_RUS_2030", lab = "Mt CO2eq/yr", title=T, Title="Russia", 
                      linegraph=F, hist=T,labels=T,var.labels=var.labels_fig1b_h, legend_name=legend_name_fig1, TotalEmis_var=TotalEmis_var_fig1,
                      natpoints=T, error_bar=T,quantiles=F)

# Figure 1 together -------------------------------------------------------
tmp1_1<-ggplot_gtable(ggplot_build(a1))
leg1_1<-which(sapply(tmp1_1$grobs,function(x) x$name) =="guide-box")
legend1_1<-tmp1_1$grobs[[leg1_1]]

tmp1_2<-ggplot_gtable(ggplot_build(b1))
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
h_fig1=grid.arrange(a1,b1,c1,d1,e1,f1,g1, h1, legend1_2, layout_matrix=lay_fig1)
ggsave(file=paste(cfg$outdir,"/Figure1_NatCom.png",sep=""),h_fig1,width=20,height=12,dpi=200)

# Figure 2
# define variables and scenarios to show in graph
vars_stack_fig2 <- c("Final Energy|Transportation", "Final Energy|Residential and Commercial","Final Energy|Industry")
vars_line_fig2 <- c("Final Energy|Non-fossil share")
cats_stack_fig2 <- c("2005", "2010", "Historical","No policy","National policies","NDC")
catsnat_fig2 <- c("No policy","National policies","NDC")
var.labels_fig2 <- c("Final energy transport", "Final energy buildings", "Final energy industry")
legend_name_fig2 <- "Final energy"
TotalEmis_var_fig2 <- c("Final Energy")

# select data from 'all' based on above selection
all_fig2 <- filter(all, Scope=="global", variable %in% c(vars_stack_fig2, vars_line_fig2,TotalEmis_var_fig2), Category %in% cats_stack_fig2, 
                   period==2030, region %in% regs, !is.na(value))
all_fig2_tmp<-all_fig2
all_fig2_tmp$unit  <- NULL
all_fig2_overview <- spread(all_fig2_tmp, key=variable, value=value)
all_fig2_overview_stat <- group_by(all_fig2_overview, Category, region) %>% 
                          summarise(median_NFShare=median(`Final Energy|Non-fossil share`), min_NFShare=min(`Final Energy|Non-fossil share`), max_NFShare=max(`Final Energy|Non-fossil share`), 
                                    median_FE=median(`Final Energy`), min_FE=min(`Final Energy`), max_FE=max(`Final Energy`))

source("functions/plot_functions_xcut_NatCom.R")
regs_fig2 <- c("BRA")
b1<-plot_stackbar_ghg(regs=regs_fig2, dt=all_fig2, vars=vars_stack_fig2,var_line=vars_line_fig2, cats = cats_stack_fig2, catsnat=catsnat_fig2, 
                      per=c(2030), file_pre="2b_BRA_2030", lab = "EJ/yr", title=T, Title="Brazil", 
                      linegraph=T, hist=T,labels=T,var.labels=var.labels_fig2, legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2,
                      natpoints=F, error_bar=T,quantiles=F, color_brewer_palette = "BrBG")

regs_fig2 <- c("BRA")
a2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_BRA_2030",lab = "EJ/yr",title=T,Title="Brazil",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2, TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("CHN")
b2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_CHN_2030",lab = "EJ/yr",title=T,Title="China",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("IND")
c2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_IND_2030",lab = "EJ/yr",title=T,Title="India",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("JPN")
d2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_JPN_2030",lab = "EJ/yr",title=T,Title="Japan",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("USA")
e2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_USA_2030",lab = "EJ/yr",title=T,Title="USA",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("EU")
f2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_EU_2030",lab = "EJ/yr",title=T,Title="European Union",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")
regs_fig2 <- c("RUS")
g2<-plot_stackbar_with_line_ghg(regs=regs_fig2,dt=all_fig2,vars_stack=vars_stack_fig2,var_line=vars_line_fig2,
                      cats = cats_stack_fig2, per=c(2030),file_pre="2b_RUS_2030",lab = "EJ/yr",title=T,Title="Russia",
                      hist=F,labels=T,var.labels=var.labels_fig2,legend_name=legend_name_fig2,TotalEmis_var=TotalEmis_var_fig2, natpoints=F,catsnat_fig2, 
                      error_bar=T,quantiles=F, color_brewer_palette = "BrBG")

# Figure 2 together -------------------------------------------------------
tmp2_1<-ggplot_gtable(ggplot_build(a2))
leg2_1<-which(sapply(tmp2_1$grobs,function(x) x$name) =="guide-box")
legend2_1<-tmp2_1$grobs[[leg2_1]]

a2=a2+theme(legend.position = "none")
b2=b2+theme(legend.position = "none")
c2=c2+theme(legend.position = "none")
d2=d2+theme(legend.position = "none")
e2=e2+theme(legend.position = "none")
f2=f2+theme(legend.position = "none")
g2=g2+theme(legend.position = "none")

lay_fig2<-rbind(c(1,2,3,4),c(5,6,7,8))
h_fig2=grid.arrange(a2,b2,c2,d2,e2,f2,g2, legend2_1, layout_matrix=lay_fig2) #ncol=3
ggsave(file=paste(cfg$outdir,"/Figure2_NatCom.png",sep=""),h_fig2,width=20,height=12,dpi=200)

# Figure 3
# Emissions gap
# Non-fossil gap
# CO2-intensity gap

# ADD PERCENTAGE REDUCTION NEXT TO ARROW

cats_fig3 <- c('National policies', 'Carbon budget 1000', 'Carbon budget 400')
regs_fig3 <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
vars_fig3 <- c("Emissions|Kyoto Gases", "Emissions Intensity of GDP|MER", "Final Energy|Non-fossil share")
gaps <- c("2C", "1.5C")

#ghg_emissions <- filter(all, Category %in% cats_fig3, region %in% regs_fig3, period<=2030, variable=="Emissions|Kyoto Gases", !is.na(value)) %>%
#                 group_by(Category, region, period) %>%
#                 summarise(value=median(value))
#nf_share <- filter(all, Category %in% cats_fig3, region %in% regs_fig3, period<=2030, variable=="Final Energy|Non-fossil share", !is.na(value)) %>%
#            group_by(Category, region, period) %>%
#            summarise(value=median(value))
#co2_intens <- filter(all, Category %in% cats_fig3, region %in% regs_fig3, period<=2030, variable=="Emissions Intensity of GDP|MER", !is.na(value)) %>%
#              group_by(Category, region, period) %>%
#              summarise(value=median(value))

data_fig3 <- filter(all, Scope=="global", Category %in% cats_fig3, region %in% regs_fig3, period<=2030, variable %in% vars_fig3, !is.na(value))
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
var_labels <- c("Emissions|Kyoto Gases"="Total GHG emissions", "Emissions Intensity of GDP|MER"="CO2 intensity", "Final Energy|Non-fossil share"="% non-fossil energy")
colours_fig3 <- brewer.pal(5,"Accent")
names(colours_fig3) <- levels(cats_fig3)
fig3 <- ggplot(data=data_fig3_stat) + geom_line(aes(period, median, colour=Category), size=2) + 
                   #scale_colour_brewer(palette="Dark2") +
                   geom_ribbon(aes(x=period,ymin=perc_10,ymax=perc_90,fill=Category),alpha=.15, show.legend = F) +
                   geom_segment(data=filter(Gap_stat, gap=="1.5C"), mapping=aes(x=2032, y=start_median, xend=2032, yend=start_median-gap_median, linetype=gap), 
                                arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
                   geom_segment(data=filter(Gap_stat, gap=="2C"), mapping=aes(x=2031, y=start_median, xend=2031, yend=start_median-gap_median, linetype=gap), 
                                arrow=arrow(length = unit(0.25, "cm")), size=1)+#, color="dark blue") +
                   facet_wrap(variable~region, scales = "free_y", nrow=3, labeller=labeller(variable = var_labels, region=reg_labels)) +
                   ylim(0,NA) +
                   xlab("year") + ylab("") +
                   #scale_y_discrete(limits=c(0,NA), labels=scales::comma_format()) +
                   scale_linetype_discrete(name="Median gap with", breaks=gaps, labels=c("2° C", "1.5° C")) +                 
                   scale_colour_manual(name= "Scenario", values=colours_fig3, labels=c("National policies"="National policies", "Carbon budget 1000"="2° C", "Carbon budget 400"="1.5° C"),
                                       guide = guide_legend(reverse=TRUE)) +
                   scale_fill_manual(values=colours_fig3) +
  
                   guides(color = guide_legend(order = 1), linetype = guide_legend(order = 0)) +
                   theme_bw() +
                   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                   theme(strip.text.x = element_text(size=12, face="bold"),
                         strip.text.y = element_text(size=8, face="bold"),
                         strip.background = element_rect(colour="black", fill="white"))
ggsave(file=paste(cfg$outdir,"/Figure3_NatCom.png",sep=""),fig3,width=20,height=12,dpi=200)