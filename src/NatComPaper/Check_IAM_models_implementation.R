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
library(xtable)
library(grid)
library(scales)
library(readxl)

# create CD-LINKS data
# set variables

# retrieve CD-LINKS data from csv files and historical PRIMAP/IEA data
keep_original = TRUE
source('NatComPaper/Data Natcom paper.R')

# adjust data for check IAM models
all_check <- all_paper
all_check_before_adj <- all_paper_before_adj
all_check$period <- as.integer(all_check$period)
all_check_before_adj$period <- as.integer(all_check_before_adj$period)
all_check_hist <- all_hist_paper
all_check_hist$period <- as.integer(all_check_hist$period)

# convert CH4 and N2O emissions to CO2eq
GWP_CH4 <- 25
GWP_N2O <- 298
# CH4
tmp1 <- filter(all_check, grepl("Emissions\\|CH4", variable))
tmp1$value<-tmp1$value*GWP_CH4
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check, !(grepl("Emissions\\|CH4", variable)))
all_check <- rbind(tmp2, tmp1)
tmp1 <- filter(all_check_before_adj, grepl("Emissions\\|CH4", variable))
tmp1$value<-tmp1$value*GWP_CH4
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check_before_adj, !(grepl("Emissions\\|CH4", variable)))
all_check_before_adj <- rbind(tmp2, tmp1)
tmp1 <- filter(all_check_hist, grepl("Emissions\\|CH4", variable))
tmp1$value<-tmp1$value*GWP_CH4
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check_hist, !(grepl("Emissions\\|CH4", variable)))
all_check_hist <- rbind(tmp2, tmp1)
#N2O
tmp1 <- filter(all_check_before_adj, grepl("Emissions\\|N2O", variable))
tmp1$value<-tmp1$value*GWP_N2O/1000
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check_before_adj, !(grepl("Emissions\\|N2O", variable)))
all_check_before_adj <- rbind(tmp2, tmp1)

tmp1 <- filter(all_check, grepl("Emissions\\|N2O", variable))
tmp1$value<-tmp1$value*GWP_N2O/1000
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check, !(grepl("Emissions\\|N2O", variable)))
all_check <- rbind(tmp2, tmp1)

tmp1 <- filter(all_check_hist, grepl("Emissions\\|N2O", variable))
tmp1$value<-tmp1$value*GWP_N2O
tmp1$unit <- "MtCO2eq"
tmp2 <- filter(all_check_hist, !(grepl("Emissions\\|N2O", variable)))
all_check_hist <- rbind(tmp2, tmp1)

# read in NDC growth rates between 2010 and 2030
NDCgrowth <- read.table("data/INDC growth rates protocol.csv", sep=";", header=TRUE)

# settings
regs_check<- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World")
cats_check <- c("No policy", "National policies","NDC")
models_global <- filter(all_check, Scope=="global", model != "AIM/CGE") %>% select(model) %>% unique() %>% as.matrix() %>% as.vector()
models_national <- filter(all_check, Scope=="national") %>% select(model) %>% unique()
vars_GHG <- c("Emissions|Kyoto Gases", "Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|AFOLU", 
              "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")
vars_FE <- c("Final Energy", "Final Energy|Other",  
             "Secondary Energy|Electricity", 
             "Final Energy|Residential and Commercial", 
             "Final Energy|Transportation",  
             "Final Energy|Industry")

# I. Check model gaps, data export to Excel
write.table(all_hist, "NatComPaper/data/all_hist.csv", sep=";", row.names=F)
all_check_before_adj_emissions <- filter(all_check_before_adj, grepl("Emissions", variable), Scope=="global", region %in% regs_check)
write.table(all_check_before_adj_emissions, "NatComPaper/data/all_before_emissions.csv", sep=";", row.names = F)

all_check_before_adj_final_energy <- filter(all_check_before_adj, grepl("Final Energy", variable), Scope=="global", region %in% regs_check)
all_check_before_adj_secondary_energy <- filter(all_check_before_adj, grepl("Secondary Energy", variable), Scope=="global", region %in% regs_check)
all_check_before_adj_energy <- rbind(all_check_before_adj_final_energy, all_check_before_adj_secondary_energy)
all_check_before_adj_energy <- select(all_check_before_adj_energy, scenario,	Category,	Baseline,	model, region, period, unit, value, Scope, variable)
write.table(all_check_before_adj_energy, "NatComPaper/data/all_before_energy.csv", sep=";", row.names = F)

all_check_before_adj_primary_energy <- filter(all_check_before_adj, grepl("Primary Energy", variable), Scope=="global", region %in% regs_check)
all_check_before_adj_gdp <- filter(all_check_before_adj, grepl("GDP|MER", variable), Scope=="global", region %in% regs_check)
all_check_before_adj_select <- rbind(all_check_before_adj_emissions, all_check_before_adj_final_energy) %>% rbind(all_check_before_adj_secondary_energy) %>% rbind(all_check_before_adj_primary_energy) %>% rbind(all_check_before_adj_gdp)
write.table(all_check_before_adj_select, "NatComPaper/data/all_before_select.csv", sep=";", row.names = F)

all_check_emissions <- filter(all_check, grepl("Emissions", variable), Scope=="global", region %in% regs_check, Category%in%scens)
write.table(all_check_emissions, "NatComPaper/data/all_emissions.csv", sep=";", row.names = F)

all_check_final_energy <- filter(all_paper, grepl("Final Energy", variable), Scope=="global", region %in% regs_check, Category%in%cats_check)
all_check_secondary_energy <- filter(all_paper, grepl("Secondary Energy", variable), Scope=="global", region %in% regs_check, Category%in%cats_check)
all_hist_final_energy <- filter(all_hist_paper, grepl("Final Energy", variable),region %in% regs_check)
all_hist_secondary_energy <- filter(all_hist_paper, grepl("Secondary Energy", variable), region %in% regs_check)
all_check_energy <- rbind(all_check_final_energy, all_check_secondary_energy)
all_hist_energy <- rbind(all_hist_final_energy, all_hist_secondary_energy)
all_energy <- rbind(all_hist_energy, all_check_energy)
all_energy <- select(all_energy, scenario,	Category,	Baseline,	model, region, period, unit, value, Scope, variable)
write.table(all_check_energy, "NatComPaper/data/all_check_energy.csv", sep=";", row.names = F)
write.table(all_energy, "NatComPaper/data/all_energy.csv", sep=";", row.names = F)

all_check_primary_energy <- filter(all_check, grepl("Primary Energy", variable), Scope=="global", region %in% regs_check)
all_check_gdp <- filter(all_check, grepl("GDP|MER", variable), Scope=="global", region %in% regs_check, region %in% regs_check)
all_check_select <- rbind(all_check_emissions, all_check_final_energy) %>% rbind(all_check_secondary_energy) %>% rbind(all_check_primary_energy) %>% rbind(all_check_gdp)
write.table(all_check_select, "NatComPaper/data/all_select.csv", sep=";", row.names = F)

all_check_final_energy_transport <- filter(all_check, grepl("Final Energy\\|Transportation", variable), Scope=="global", region %in% regs_check, region %in% regs_check)
all_check_final_energy_industry <- filter(all_check, grepl("Final Energy\\|Industry", variable), Scope=="global", region %in% regs_check, region %in% regs_check)
all_check_final_energy_buildings <- filter(all_check, grepl("Final Energy\\|Residential and Commerical", variable), Scope=="global", region %in% regs_check, region %in% regs_check)
all_check_final_biomass <- filter(all_check, grepl("Biomass", variable), Scope=="global", region %in% regs_check, region %in% regs_check)

p = ggplot() + 
       geom_line(data=filter(all_energy, variable=="Final Energy|Non-fossil share", period>=2010, period<=2030, Category%in%c("National policies")), aes(x=period, y=value, colour=model)) +
       geom_line(data=filter(all_energy, variable=="Final Energy|Non-fossil share", period>=2010, period<=2030, Category%in%c("Historical")), aes(x=period, y=value, linetype=model), size=2) +
       facet_wrap(~region) +
       theme_bw() +
       ggtitle("Low carbon share of final energy") + 
       xlab("period")+
       ylab("%") +
       ylim(0,NA) +
       scale_linetype(name="source")
plot(p)
ggsave(file=paste("NatComPaper/graphs/review","/LowCarbonShare.png",sep=""),p,width=20,height=12,dpi=200) 

# II. Compare CD-LINKS NPi and INDCi with PBL factsheet report

# IIa. PBL CLIMA factsheet data (1. NDC, 2. NPi)
# 1. NDC
# INDCs incl LULUCF
INDC_incl_PBLCLIMA <- read.csv("data/2018_ndcs_incl.csv", header=TRUE, sep=";")
colnames(INDC_incl_PBLCLIMA) = gsub("X", "", colnames(INDC_incl_PBLCLIMA))
INDC_incl_PBLCLIMA <- gather(INDC_incl_PBLCLIMA, 6:ncol(INDC_incl_PBLCLIMA), key="year", value=value)
INDC_incl_PBLCLIMA <- filter(INDC_incl_PBLCLIMA, !is.na(value))
INDC_incl_PBLCLIMA <- mutate(INDC_incl_PBLCLIMA, NDC="incl")
# INDCs excl LULUCF
INDC_excl_PBLCLIMA <- read.csv("data/2018_ndcs_excl.csv", header=TRUE, sep=";")
colnames(INDC_excl_PBLCLIMA) = gsub("X", "", colnames(INDC_excl_PBLCLIMA))
INDC_excl_PBLCLIMA <- gather(INDC_excl_PBLCLIMA, 6:ncol(INDC_excl_PBLCLIMA), key="year", value=value)
INDC_excl_PBLCLIMA <- filter(INDC_excl_PBLCLIMA, !is.na(value))
INDC_excl_PBLCLIMA <- mutate(INDC_excl_PBLCLIMA, NDC="excl")
# merge two NDC tables
INDC_PBLCLIMA <- rbind(INDC_excl_PBLCLIMA,INDC_incl_PBLCLIMA)
colnames(INDC_PBLCLIMA)[colnames(INDC_PBLCLIMA)=="Country"] <- "region"
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"Brazil","BRA")
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"China","CHN")
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"India","IND")
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"Russia","RUS")
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"USA","USA")
INDC_PBLCLIMA$region=str_replace_all(INDC_PBLCLIMA$region,"Japan","JPN")
# select regions
INDC_PBLCLIMA <- filter(INDC_PBLCLIMA, region %in% regs_check)
INDC_PBLCLIMA <- arrange(INDC_PBLCLIMA, region, year)
# determine whether NDC is incl/excl LULUCF CO2
INDC_type <- select(INDC_PBLCLIMA, region, NDC)
INDC_type <- distinct(INDC_type, region, NDC)
INDC_type_world <- c("World", "incl")
INDC_type <- rbind(INDC_type, INDC_type_world)
# make table with only NDC value for 2025 and 2030 and determine min/max
INDC_PBLCLIMA$region <- factor(INDC_PBLCLIMA$region, levels=regs_check)
INDC_PBLCLIMA$NDC <- factor(INDC_PBLCLIMA$NDC, levels=c('incl', 'excl'))
INDC_PBLCLIMA$year <- as.numeric(INDC_PBLCLIMA$year)
INDC_PBLCLIMA <- group_by(INDC_PBLCLIMA, region, year, NDC)
INDC_PBLCLIMA <- filter(INDC_PBLCLIMA, year %in% c(2025, 2030))
minmax_NDC <- summarize(INDC_PBLCLIMA, min=min(value), max=max(value))
# 2. NPi
NPi_PBLCLIMA <- read.csv("data/Output Marian 2018.csv", header=TRUE, sep=";")
NPi_PBLCLIMA <- select(NPi_PBLCLIMA, -X)
colnames(NPi_PBLCLIMA) = gsub("X", "", colnames(NPi_PBLCLIMA))
NPi_PBLCLIMA <- gather(NPi_PBLCLIMA, 3:ncol(NPi_PBLCLIMA), key="year", value=value)
NPi_PBLCLIMA <- filter(NPi_PBLCLIMA, !is.na(value))
# rename and select regions for years 2025, 2030
colnames(NPi_PBLCLIMA)[colnames(NPi_PBLCLIMA)=="Country"] <- "region"
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"Brazil","BRA")
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"China","CHN")
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"India","IND")
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"Russia","RUS")
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"USA","USA")
NPi_PBLCLIMA$region=str_replace_all(NPi_PBLCLIMA$region,"Japan","JPN")
NPi_PBLCLIMA <- filter(NPi_PBLCLIMA, region %in% regs_check, year %in% c(2025, 2030))
# create min/max range
NPi_PBLCLIMA <- right_join(NPi_PBLCLIMA, INDC_type, by=c('region'))
NPi_PBLCLIMA_a <- filter(NPi_PBLCLIMA, Type=='Min Current Policy Excluding', NDC=="excl")
NPi_PBLCLIMA_b <- filter(NPi_PBLCLIMA, Type=='Max Current Policy Excluding', NDC=="excl")
NPi_PBLCLIMA_c <- filter(NPi_PBLCLIMA, Type=='Min Current Policy Including', NDC=="incl")
NPi_PBLCLIMA_d <- filter(NPi_PBLCLIMA, Type=='Max Current Policy Including', NDC=="incl")
NPi_PBLCLIMA <- bind_rows(NPi_PBLCLIMA_a, NPi_PBLCLIMA_b) %>% bind_rows(NPi_PBLCLIMA_c) %>% bind_rows(NPi_PBLCLIMA_d)
NPi_PBLCLIMA <- select(NPi_PBLCLIMA, -Type)
NPi_PBLCLIMA_1 <- filter(NPi_PBLCLIMA, region %in% c('USA', 'BRA'), year==2025)
NPi_PBLCLIMA_2 <- filter(NPi_PBLCLIMA, !(region %in% c('USA')), year==2030)
NPi_PBLCLIMA <- bind_rows(NPi_PBLCLIMA_1, NPi_PBLCLIMA_2)
NPi_PBLCLIMA <- arrange(NPi_PBLCLIMA, region, year)
NPi_PBLCLIMA$region <- factor(NPi_PBLCLIMA$region, levels=regs_check)
NPi_PBLCLIMA$NDC <- factor(NPi_PBLCLIMA$NDC, levels=c('incl', 'excl'))
NPi_PBLCLIMA$year <- as.numeric(NPi_PBLCLIMA$year)
NPi_PBLCLIMA <- group_by(NPi_PBLCLIMA, region, year, NDC)
minmax_NPi <- summarize(NPi_PBLCLIMA, min=min(value), max=max(value))

# Add global numbers from UNEP
minmax_NPi_World = data.frame(region=factor("World", levels=regs_check), year=2030, NDC=factor('incl', levels=c('incl', 'excl')), min=57600.0, max=60700.0)
minmax_NPi <- bind_rows(minmax_NPi, minmax_NPi_World)
minmax_NDC_World = data.frame(region="World", year=2030, NDC='incl', min=49500.0, max=54200.0)
minmax_NDC <- bind_rows(minmax_NDC, minmax_NDC_World)

# Filter model data for scope, scenarios, regions and variables
cats_check_NDC <- c("National policies","NDC")
Model_global_data <- filter(all_check, Scope=="global", Category %in% cats_check_NDC, region %in% regs_check, variable%in%c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|Excl. AFOLU CO2"))

all_hist_graph <- filter(all_hist, variable %in% c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|Excl. AFOLU CO2")) %>% select(region, period, value, variable)

# also add NDC and CPS from PBL CLIMA factsheet report
for (r in regs_check) { 
  #r="World"
  cat(r, "\n")
  if (INDC_type[INDC_type$region==r,]$NDC=="incl") var="Emissions|Kyoto Gases" else var="Emissions|Kyoto Gases|Excl. AFOLU CO2"
  g <- ggplot(data=filter(Model_global_data, region==r, period<=2030, variable==var)) + geom_line(aes(x=period,y=value, colour=Category)) +
  geom_line(data=filter(all_hist_graph, region %in% r, variable==var), aes(x=period, y=value), linetype="dashed") +
  # add min/max NDCs from factsheet
  geom_point(data=minmax_NDC[minmax_NDC$region==r,], aes(x=year+0.5, y=min), colour="green", size=4) +
  geom_point(data=minmax_NDC[minmax_NDC$region==r,], aes(x=year+0.5, y=max), colour="green", size=4) +
  geom_segment(data=minmax_NDC[minmax_NDC$region==r,], stat="identity", aes(x=year+0.5, xend=year+0.5, y=min, yend=max, size=1.5), show.legend=FALSE, colour="green") + 
  # add minmax national policies from factsheet
  geom_point(data=minmax_NPi[minmax_NPi$region==r,], aes(x=year+1.5, y=min), colour="blue", size=3) +
  geom_point(data=minmax_NPi[minmax_NPi$region==r,], aes(x=year+1.5, y=max), colour="blue", size=3) +
  geom_segment(data=minmax_NPi[minmax_NPi$region==r,], stat="identity", aes(x=year+1.5, xend=year+1.5, y=min, yend=max, size=1.5), show.legend=FALSE, colour="blue") +
  geom_text(data=minmax_NPi[minmax_NPi$region==r,], aes(x=2030, y=1.1*max, label="Compare"), show.legend=FALSE) + 
  scale_colour_manual(values=c("blue", "green")) +
  facet_wrap(~model) +
  ylim(0,NA) +
  theme_bw() +
  ggtitle(paste(r, " Total GHG ", INDC_type[INDC_type$region==r,]$NDC, " AFOLU CO[2]", sep=""))
  gg <- plot(g)
  ggsave(file=paste("NatComPaper/graphs/review/NDC/check_", r, ".png", sep=""),gg, height=10, width=15)
}


# II.b make table with NDC growth rates
NDCgrowth_models_incl <- filter(all_check, variable == "Emissions|Kyoto Gases", model %in% models_global, Category=="NDC", (period==2010 | period==2030)) %>% 
  spread(key=period, value=value) %>%
  mutate(EM_rel_2010=`2030`/`2010`) %>%
  mutate(NDC="incl") %>%
  select(model, region, NDC, EM_rel_2010)
NDCgrowth_models_excl <- filter(all_check, variable == "Emissions|Kyoto Gases|Excl. AFOLU CO2", model %in% models_global, Category=="NDC", (period==2010 | period==2030)) %>% 
  spread(key=period, value=value) %>%
  mutate(EM_rel_2010=`2030`/`2010`) %>%
  mutate(NDC="excl") %>%
  select(model, region, NDC, EM_rel_2010)
NDCgrowth_models <- rbind(NDCgrowth_models_incl, NDCgrowth_models_excl)
NDCgrowth_models <- right_join(NDCgrowth_models, INDC_type, by=c('region')) %>%
  filter(NDC.x==NDC.y) %>%
  select(-NDC.y) %>%
  rename(NDC="NDC.x")
NDCgrowth_models <- spread(NDCgrowth_models, key="model", value=EM_rel_2010)
NDCgrowth_models <- cbind(NDCgrowth_models, NDCgrowth$EM_rel_2010)
colnames(NDCgrowth_models)[length(NDCgrowth_models)] <- "NDC_growth"
colnames(NDCgrowth_models)[which(names(NDCgrowth_models) == "NDC")] <- "LULUCF"
NDCgrowth_models[3:ncol(NDCgrowth_models)] <-  round(NDCgrowth_models[3:ncol(NDCgrowth_models)], 2)

# png table
png("NatComPaper/graphs/NDC_table.png", height = 25*nrow(NDCgrowth_models), width = 125*ncol(NDCgrowth_models))
grid.table(NDCgrowth_models)
dev.off()

# html table
##n <- data.frame(x = c(1,1,1,1,1), y = c(0,1,0,1,0))
## the html header  
## here I am using a link to mystyle.css 
html.head <- paste("<head>" ,
                   '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                   "</head>",sep='\n')
html.table <- paste(print(xtable(NDCgrowth_models),type='html','NatComPaper/graphs/NDCgrowth.html'), 
                    collapse = "\n", caption="NDC emission growth (incl/excl LULCUF) between 2010 and 2030 for CD-LINKS models and pledged NDC (Emissions gap report for world)")
html.body <- paste("<body>", html.table,"</body>")
write(paste(html.head,html.body,sep='\n'),"NatComPaper/graphs/NDCgrowth.html")


# IIIa. Compare CD-LINKS NPi and INDCi with historical PRIMAP data
# before and after adjustments (adjust_reporting_indc_Mark)
for (i in 1:2) {
  if (i==1){
    all_check_graph <- all_check_before_adj
    check<-"before"
  }
  else{
    all_check_graph <- all_check
    check<-"after"
    }
  for (v in vars_GHG) { 
    cat(paste0(v, "-", check, "\n"))
    for (r in regs_check) { 
      #v="Emissions|Kyoto Gases"
      #r="World"
      cat(paste0("- ", r, "\n"))
      d1 <- filter(all_check_graph, model %in% models_global, period>=2005, period<=2015, Category=="National policies", region==r, variable==v)
      d2 <- filter(all_check_hist, model=="PRIMAP", period>=2005, period<=2015, region==r, variable==v) %>% select(period, value)
      d2$period  <- as.integer(d2$period)
      d <- left_join(d1, d2, by=c('period'))
      d <- mutate(d, diff=round(value.x/value.y, digits=2))
      #d <- rbind(d1, d2)
      g_hist <- ggplot(data=d) + 
                geom_point(aes(x=period, y=value.x, colour="model"), show.legend = TRUE) +
                geom_point(aes(x=period, y=value.y, colour="PRIMAP"), show.legend = TRUE) +
                geom_line(aes(x=period, y=value.x, colour="model")) +
                geom_line(aes(x=period, y=value.y, colour="PRIMAP"),linetype=2) +
                geom_text(aes(x=period, y=diff, label=diff)) +
                facet_wrap(~model) +
                theme_bw() +
                ylim(min(0,min(d1$value)),NA) + 
                xlab("Year") +
                ylab(d1$unit) +
                scale_x_continuous(breaks=seq(2005, 2015, 5)) +
                scale_colour_manual(name="Source", values=c(model="cornflowerblue", PRIMAP="darkgrey")) +
                ggtitle(paste(r, "-", v, sep=""))
      gg_hist <- plot(g_hist)
      ggsave(file=paste("NatComPaper/graphs/review/hist/check_hist_GHG_", check, "_", v, "_", r, ".png", sep=""),gg_hist, height=10, width=15)
      d_plot <-rbind(mutate(d2,model="PRIMAP"), select(d1,period, model, value)) %>% filter(period==2010)
      d_plot$value<-as.double(d_plot$value)
      #d_plot$model<-factor(d_plot$model, levels=c("PRIMAP", unique(d1$model)))
      p = ggplot(data=d_plot) + 
          geom_bar(aes(x=model, y=value, fill=model), stat="identity")       +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1,size=18)) +
          ggtitle(paste0("2010 - ",v))
      ggsave(file=paste("NatComPaper/graphs/review/hist/compare_GHG_", check, "_", v, "_", r, ".png", sep=""),p, height=10, width=15)
    }
  }
}

# IIIb. Compare CD-LINKS NPi and INDCi with historical IEA data
# before and after adjustments (adjust_reporting_indc_Mark)
for (i in 1:2) {
  if (i==1){
    all_check_graph <- all_check
    check<-"after"
  }
  else{
    all_check_graph <- all_check_before_adj
    check<-"before"
  }
  for (v in vars_FE) { 
    cat(paste0(v, "-", check, "\n"))
    for (r in regs_check) { 
      #v="Emissions|Kyoto Gases"
      #r="World"
      cat(paste0("- ", r, "\n"))
      d1 <- filter(all_check_graph, model %in% models_global, period>=2005, period<=2015, Category=="National policies", region==r, variable==v)
      d2 <- filter(all_check_hist, model=="IEA", period>=2005, period<=2015, region==r, variable==v) %>% select(period, value)
      d2$period  <- as.integer(d2$period)
      d <- left_join(d1, d2, by=c('period'))
      d <- mutate(d, diff=round(value.x/value.y, digits=2))
      #d <- rbind(d1, d2)
      g_hist <- ggplot(data=d) + 
        geom_point(aes(x=period, y=value.x, colour="model"), show.legend = TRUE) +
        geom_point(aes(x=period, y=value.y, colour="IEA"), show.legend = TRUE) +
        geom_line(aes(x=period, y=value.x, colour="model")) +
        geom_line(aes(x=period, y=value.y, colour="IEA"),linetype=2) +
        geom_text(aes(x=period, y=diff, label=diff)) +
        facet_wrap(~model) +
        theme_bw() +
        ylim(min(0,min(d1$value)),NA) + 
        xlab("Year") +
        ylab("EJ/yr") +
        scale_x_continuous(breaks=seq(2005, 2015, 5)) +
        scale_colour_manual(name="Source", values=c(model="brown3", IEA="darkgoldenrod3")) +
        ggtitle(paste(r, "-", v, sep=""))
      gg_hist <- plot(g_hist)
      ggsave(file=paste("NatComPaper/graphs/review/hist/check_hist_FE_", check, "_", v, "_", r, ".png", sep=""),gg_hist, height=10, width=15)
    }
  }
}

# Compare bunkers
b <- mutate(EDGAR_bunkers, region=="World", model="EDGAR")
d1 <- filter(all_check, variable=="Emissions|Kyoto Gases", Category=="National policies", region=="Bunkers", model%in%models_global, period>=2010, period<=2030)
d2 <- filter(b, period>=2010, period<=2030)
p = ggplot()+
  geom_line(data=d1, aes(x=period, y=value, colour=model, linetype=model))+
  geom_point(data=d2, aes(x=period, y=value, size=model))+
  #facet_wrap(~region, scales = "free")+
  scale_size_discrete(name="source")
plot(p)
ggsave(file=paste("NatComPaper/graphs/Bunkers.png", sep=""),gg_hist, height=10, width=15)


# Compare GDP
d1 <- filter(all_check, variable=="GDP|MER", Category=="National policies", region%in%regs_check, model%in%models_global, period>=2010, period<=2030)
d2 <- filter(all_check_hist, variable=="GDP|MER", region%in%regs_check, period>=2010, period<=2030)
d <- rbind(d1,d2)
p = ggplot()+
    geom_line(data=d1, aes(x=period, y=value, colour=model, linetype=model))+
    geom_point(data=d2, aes(x=period, y=value, size=model))+
    facet_wrap(~region, scales = "free")+
    scale_size_discrete(name="source")
plot(p)
ggsave(file=paste("NatComPaper/graphs/GDP.png", sep=""),gg_hist, height=10, width=15)

# check final energy
vars_FE <- c("Final Energy", "Final Energy|Other", "Final Energy|Residential and Commercial|Electricity", "Final Energy|Transportation|Electricity", "Final Energy|Industry|Electricity", 
             "Secondary Energy|Electricity", "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Coal|w/ CCS", "Secondary Energy|Electricity|Fossil|w/ CCS", 
             "Secondary Energy|Electricity|Gas|w/ CCS", "Secondary Energy|Electricity|Nuclear", "Final Energy|Residential and Commercial", 
             "Final Energy|Residential and Commercial|Electricity", "Final Energy|Transportation", "Final Energy|Transportation|Electricity", 
             "Final Energy|Transportation|Liquids|Biomass", "Final Energy|Industry", "Final Energy|Industry|Electricity", "Final Energy|Solids|Biomass",
             "Final Energy|Solids|Biomass|Traditional", "Secondary Energy|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Biomass|w/o CCS", 
             "Secondary Energy|Electricity|Non-Biomass Renewables")
all_FE <- filter(all_check, variable %in% vars_FE, region %in% regs_check)
write.table(all_FE, "data/all_check_FE.csv", sep=";", row.names=F)

vars_FE_overview <- c("Secondary Energy|Electricity|Fossil", "Secondary Energy|Electricity|Non-fossil", "Secondary Energy|Electricity|Non-fossil share",
                      "Final Energy|Residential and Commercial|Non-fossil", "Final Energy|Residential and Commercial|Non-fossil share",
                      "Final Energy|Transportation|Non-fossil", "Final Energy|Transportation|Non-fossil share",
                      "Final Energy|Industry|Non-fossil", "Final Energy|Industry|Non-fossil share",
                      "Final Energy|Non-fossil", "Final Energy", "Final Energy|Non-fossil share"
)
all_check_FE_overview <- filter(all_check, variable %in% vars_FE_overview, region %in% regs_check)
write.table(all_check_FE_overview, "data/all_check_FE_overview.csv", sep=";", row.names=F)

# check nuclear India
d_CO2_IND_NoPolicy <- filter(NoPolicy$ENEMISCO2, region=="INDIA", year>=2010, year<=2030, sector=="Total", energy_carrier=="Total") %>% mutate(scenario="No policy")
d_CO2_IND_NPi <- filter(NPi$ENEMISCO2, region=="INDIA", year>=2010, year<=2030, sector=="Total", energy_carrier=="Total")  %>% mutate(scenario="NPi")
d_CO2_IND_NDC <- filter(INDCi$ENEMISCO2, region=="INDIA", year>=2010, year<=2030, sector=="Total", energy_carrier=="Total")  %>% mutate(scenario="INDCi")
d_CO2_IND_NPi_1000 <- filter(NPi2020_1000$ENEMISCO2, region=="INDIA", year>=2010, year<=2030, sector=="Total", energy_carrier=="Total")  %>% mutate(scenario="NPi2020_1000")
d_CO2_IND_NDC_1000 <- filter(INDC2030i_1000$ENEMISCO2, region=="INDIA", year>=2010, year<=2030, sector=="Total", energy_carrier=="Total")  %>% mutate(scenario="INDC2030i_1000")
d_CO2_IND <- rbind(d_CO2_IND_NoPolicy, d_CO2_IND_NPi) %>% rbind(d_CO2_IND_NDC) %>% rbind(d_CO2_IND_NPi_1000) %>% rbind(d_CO2_IND_NDC_1000)
d_CO2_IND$value <- d_CO2_IND$value*(44/12)*1000
ggplot(data=d_CO2_IND) + geom_line(aes(x=year, y=value, colour=scenario)) +theme_bw() + ylab("Mt CO2")

d_coal_IND_NoPolicy <- filter(NoPolicy$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Conv. Coal") %>% mutate(scenario="No policy")
d_coal_IND_NPi      <- filter(NPi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Conv. Coal")  %>% mutate(scenario="NPi")
d_coal_IND_NDC      <- filter(INDCi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Conv. Coal")  %>% mutate(scenario="INDCi")
d_coal_IND_NPi_1000 <- filter(NPi2020_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Conv. Coal")  %>% mutate(scenario="NPi2020_1000")
d_coal_IND_NDC_1000 <- filter(INDC2030i_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Conv. Coal")  %>% mutate(scenario="INDC2030i_1000")
d_coal_IND <- rbind(d_coal_IND_NoPolicy, d_coal_IND_NPi) %>% rbind(d_coal_IND_NDC) %>% rbind(d_coal_IND_NPi_1000) %>% rbind(d_coal_IND_NDC_1000)
ggplot(data=d_coal_IND) + geom_line(aes(x=year, y=value, colour=scenario)) + theme_bw() + ylab("MW")

d_nuclear_IND_NoPolicy <- filter(NoPolicy$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Nuclear") %>% mutate(scenario="No policy")
d_nuclear_IND_NPi      <- filter(NPi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Nuclear")  %>% mutate(scenario="NPi")
d_nuclear_IND_NDC      <- filter(INDCi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Nuclear")  %>% mutate(scenario="INDCi")
d_nuclear_IND_NPi_1000 <- filter(NPi2020_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Nuclear")  %>% mutate(scenario="NPi2020_1000")
d_nuclear_IND_NDC_1000 <- filter(INDC2030i_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Nuclear")  %>% mutate(scenario="INDC2030i_1000")
d_nuclear_IND <- rbind(d_nuclear_IND_NoPolicy, d_nuclear_IND_NPi) %>% rbind(d_nuclear_IND_NDC) %>% rbind(d_nuclear_IND_NPi_1000) %>% rbind(d_nuclear_IND_NDC_1000)
ggplot(data=d_nuclear_IND) + geom_line(aes(x=year, y=value, colour=scenario)) + theme_bw() + ylab("MW")

d_wind_IND_NoPolicy <- filter(NoPolicy$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Wind Onshore") %>% mutate(scenario="No policy")
d_wind_IND_NPi      <- filter(NPi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Wind Onshore")  %>% mutate(scenario="NPi")
d_wind_IND_NDC      <- filter(INDCi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Wind Onshore")  %>% mutate(scenario="INDCi")
d_wind_IND_NPi_1000 <- filter(NPi2020_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Wind Onshore")  %>% mutate(scenario="NPi2020_1000")
d_wind_IND_NDC_1000 <- filter(INDC2030i_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="Wind Onshore")  %>% mutate(scenario="INDC2030i_1000")
d_wind_IND <- rbind(d_wind_IND_NoPolicy, d_wind_IND_NPi) %>% rbind(d_wind_IND_NDC) %>% rbind(d_wind_IND_NPi_1000) %>% rbind(d_wind_IND_NDC_1000)
ggplot(data=d_wind_IND) + geom_line(aes(x=year, y=value, colour=scenario)) + theme_bw() + ylab("MW")

d_solarPV_IND_NoPolicy <- filter(NoPolicy$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="PV") %>% mutate(scenario="No policy")
d_solarPV_IND_NPi      <- filter(NPi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="PV")  %>% mutate(scenario="NPi")
d_solarPV_IND_NDC      <- filter(INDCi$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="PV")  %>% mutate(scenario="INDCi")
d_solarPV_IND_NPi_1000 <- filter(NPi2020_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="PV")  %>% mutate(scenario="NPi2020_1000")
d_solarPV_IND_NDC_1000 <- filter(INDC2030i_1000$ElecCap, region=="INDIA", year>=2010, year<=2030, energy_technology=="PV")  %>% mutate(scenario="INDC2030i_1000")
d_solarPV_IND <- rbind(d_solarPV_IND_NoPolicy, d_solarPV_IND_NPi) %>% rbind(d_solarPV_IND_NDC) %>% rbind(d_solarPV_IND_NPi_1000) %>% rbind(d_solarPV_IND_NDC_1000)
ggplot(data=d_solarPV_IND) + geom_line(aes(x=year, y=value, colour=scenario)) + theme_bw() + ylab("MW")

# OUTLIER test
source("functions/DeanDixonTest.R")
regs <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
test_DDT <- NA
test_DDT <- data.frame(region=character(), model=character(), location=character(), outlier=logical())
test_DDT$location <- factor(test_DDT$location, levels=c('small', 'large'))
IAM_models <- unique(all_paper$model)
test_DDT$model <- factor(test_DDT$model, levels=IAM_models)
alpha_DDT <- 0.05

for (r in regs){
  cat(r,"\n")
  #r="BRA"
  d_NDC_largest <- filter(all_paper, Category=="NDC", region==r, period==2030, variable=="Emissions|Kyoto Gases") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(desc(value))
  d_NDC_smallest <- filter(all_paper, Category=="NDC", region==r, period==2030, variable=="Emissions|Kyoto Gases") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(value) 
  
  #http://www.sthda.com/english/wiki/ggplot2-qq-plot-quantile-quantile-graph-quick-start-guide-r-software-and-data-visualization
  #p <- qplot(sample = value, data = d_NDC_smallest) + theme_bw()
  p <- qqnorm(d_NDC_smallest$value)
  p <- qqline(d_NDC_smallest$value, col="red")

  p_tmp <- ggplot(data=d_NDC_smallest, aes(sample=value))  +
           stat_qq() +
           stat_qq_line(colour="red") +
           theme_bw()
  d_new <- ggplot_build(p_tmp)$data[[1]]
  d_new <- cbind(d_new, d_NDC_smallest$model) %>% rename(model=`d_NDC_smallest$model`)
  p <- ggplot(data=d_new, aes(theoretical,sample, label=model)) + 
               stat_qq_line(aes(sample=sample),colour="red") +
               geom_text(size=4) +
               theme_bw() +
               ggtitle(paste0("QQ-plot ", r))
  plot(p)
  ggsave(file=paste0("NatComPaper/graphs/review/outliers/outlier_DDT_",r,".jpg",sep=""),p,width=20,height=12,dpi=200)
  
  cat(" - test largest value for outlier\n")
  dl <- as.vector(d_NDC_largest$value)
  dl_outlier <- DeanDixonTest(dl, alpha_DDT,TRUE)
  xl <- data.frame(region=r, model=d_NDC_largest[1,]$model, location="large", outlier=dl_outlier)
  test_DDT <- rbind(test_DDT, xl)
  
  cat(" - test smallest value for outlier\n")
  ds <- as.vector(d_NDC_smallest$value)
  ds_outlier <- DeanDixonTest(ds, alpha_DDT, TRUE)
  xs <- data.frame(region=r, model=d_NDC_smallest[1,]$model, location="small", outlier=ds_outlier)
  test_DDT <- rbind(test_DDT, xs)
}
outlier_IAM <- filter(outlier_IAM, outlier==TRUE) %>% arrange(desc(outlier))
# html table
html.head <- paste("<head>" ,
                   '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                   "</head>",sep='\n')
html.table <- paste(print(xtable(test_DDT),type='html','NatComPaper/graphs/review/outliers/DeanDixonTestResults_table.html'), 
                    collapse = "\n", caption=paste0("Dean Dixon test for outliers, with alpha=",round(alpha_DDT,2)))
html.body <- paste("<body>", html.table,"</body>")
write(paste(html.head,html.body,sep='\n'),"NatComPaper/graphs/review/outliers/DeanDixonTestResults_table.html")

# html table
html.head <- paste("<head>" ,
                   '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                   "</head>",sep='\n')
html.table <- paste(print(xtable(outlier_IAM),type='html','NatComPaper/graphs/review/outliers/outliers_table.html'), 
                    collapse = "\n", caption=paste0("Dean Dixon test for outliers, with alpha=",round(alpha_DDT,2)))
html.body <- paste("<body>", html.table,"</body>")
write(paste(html.head,html.body,sep='\n'),"NatComPaper/graphs/review/outliers/outliers_table.html")
