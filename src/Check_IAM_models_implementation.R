# create CD-LINKS data
# set variables

source('Data Natcom paper.R')
all$period <- as.integer(all$period)
# Change scenario names for paper -----------------------------------------
all$Category=str_replace_all(all$Category,"NoPOL","No policy")
all$Category=str_replace_all(all$Category,"INDC","NDC")
all$Category=str_replace_all(all$Category,"NPip","National policies planned")
all$Category=str_replace_all(all$Category,"NPi","National policies")
all$Category=str_replace_all(all$Category,"2020_low","Carbon budget 1000")
all$Category=str_replace_all(all$Category,"2020_verylow","Carbon budget 400")
all$Category=str_replace_all(all$Category,"2030_low","Carbon budget 1000 (2030)")

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

# settings
regs<- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World")
scens <- c("National policies","NDC")
models_global <- filter(all, Scope=="global", model != "AIM/CGE") %>% select(model) %>% unique() %>% as.matrix() %>% as.vector()
#models_global <- as.vector(as.matrix((models_global))
models_national <- filter(all, Scope=="national") %>% select(model) %>% unique()

all[model=="*GEM-E3"]$model<-"GEM-E3"
all[model=="GEM-E3"]$Scope<-"global"
all_before_adj[model=="*GEM-E3"]$model<-"GEM-E3"
all_before_adj[model=="GEM-E3"]$Scope<-"global"

# I. Check model gaps
# data export to Excel
all_before_adj_emissions <- filter(all_before_adj, grepl("Emissions", variable), Scope=="global")
write.table(all_before_adj_emissions, "data/all_before_adj_emissions.csv", sep=";", row.names = F)
all_before_adj_final_energy <- filter(all_before_adj, grepl("Final Energy", variable), Scope=="global")
all_before_adj_secondary_energy <- filter(all_before_adj, grepl("Secondary Energy", variable), Scope=="global")
all_before_adj_primary_energy <- filter(all_before_adj, grepl("Primary Energy", variable), Scope=="global")
all_before_adj_gdp <- filter(all_before_adj, grepl("GDP|MER", variable), Scope=="global")
all_before_adj_select <- rbind(all_before_adj_emissions, all_before_adj_final_energy) %>% rbind(all_before_adj_secondary_energy) %>% rbind(all_before_adj_primary_energy) %>% rbind(all_before_adj_gdp)
write.table(all_before_adj_select, "data/all_before_adj_select.csv", sep=";", row.names = F)

all_emissions <- filter(all, grepl("Emissions", variable), Scope=="global")
write.table(all_emissions, "data/all_emissions.csv", sep=";", row.names = F)
all_final_energy <- filter(all, grepl("Final Energy", variable), Scope=="global")
all_secondary_energy <- filter(all, grepl("Secondary Energy", variable), Scope=="global")
all_primary_energy <- filter(all, grepl("Primary Energy", variable), Scope=="global")
all_gdp <- filter(all, grepl("GDP|MER", variable), Scope=="global")
all_select <- rbind(all_emissions, all_final_energy) %>% rbind(all_secondary_energy) %>% rbind(all_primary_energy) %>% rbind(all_gdp)
write.table(all_select, "data/all_select.csv", sep=";", row.names = F)

# II. Compare CD-LINKS NPi and INDCi with PBL factsheet report

# PBL CLIMA factsheet data (1. NDC, 2. NPi)
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
INDC_PBLCLIMA <- filter(INDC_PBLCLIMA, region %in% regs)
INDC_PBLCLIMA <- arrange(INDC_PBLCLIMA, region, year)
# determine whether NDC is incl/excl LULUCF CO2
INDC_type <- select(INDC_PBLCLIMA, region, NDC)
INDC_type <- distinct(INDC_type, region, NDC)
INDC_type_world <- c("World", "incl")
INDC_type <- rbind(INDC_type, INDC_type_world)
# make table with only NDC value for 2025 and 2030 and determine min/max
INDC_PBLCLIMA$region <- factor(INDC_PBLCLIMA$region, levels=regs)
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
NPi_PBLCLIMA <- filter(NPi_PBLCLIMA, region %in% regs, year %in% c(2025, 2030))
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
NPi_PBLCLIMA$region <- factor(NPi_PBLCLIMA$region, levels=regs)
NPi_PBLCLIMA$NDC <- factor(NPi_PBLCLIMA$NDC, levels=c('incl', 'excl'))
NPi_PBLCLIMA$year <- as.numeric(NPi_PBLCLIMA$year)
NPi_PBLCLIMA <- group_by(NPi_PBLCLIMA, region, year, NDC)
minmax_NPi <- summarize(NPi_PBLCLIMA, min=min(value), max=max(value))

# Add global numbers from UNEP
minmax_NPi_World = data.frame(region=factor("World", levels=regs), year=2030, NDC=factor('incl', levels=c('incl', 'excl')), min=57600.0, max=60700.0)
minmax_NPi <- bind_rows(minmax_NPi, minmax_NPi_World)
minmax_NDC_World = data.frame(region="World", year=2030, NDC='incl', min=49500.0, max=54200.0)
minmax_NDC <- bind_rows(minmax_NDC, minmax_NDC_World)

# Filter model data for scope, scenarios, regions and variables
Model_global_data <- filter(all, Scope=="global", Category %in% scens, region %in% regs, variable%in%c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|Excl. AFOLU CO2"))

all_hist_graph <- filter(all_hist, variable %in% c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|Excl. AFOLU CO2")) %>% select(region, period, value, variable)

# also add NDC and CPS from PBL CLIMA factsheet report
for (r in regs) { 
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
  ggsave(file=paste("graphs/check_", r, ".png", sep=""),gg, height=10, width=15)
}


vars_GHG <- c("Emissions|Kyoto Gases", "Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|AFOLU", 
              "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")

for (v in vars_GHG) { 
  cat(paste0(v, "\n"))
  for (r in regs) { 
    #v="Emissions|Kyoto Gases"
    #r="World"
    cat(paste0("- ", r, "\n"))
    d1 <- filter(all, model %in% models_global, period>=2005, period<=2015, Category=="NoPOL", region==r, variable==v)
    d2 <- filter(all_hist, model=="PRIMAP", period>=2005, period<=2015, region==r, variable==v) %>% select(period, value)
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
              ylim(0,NA) + 
              xlab("Year") +
              ylab("MtCO2eq") +
              scale_x_continuous(breaks=seq(2005, 2015, 5)) +
              scale_colour_manual(name="Source", values=c(model="cornflowerblue", PRIMAP="darkgrey")) +
              ggtitle(paste(r, "-", v, sep=""))
    gg_hist <- plot(g_hist)
    ggsave(file=paste("graphs/hist/check_hist_", v, "_", r, ".png", sep=""),gg_hist, height=10, width=15)
  }
}

# check final energy
vars_FE <- c("Final Energy", "Final Energy|Other", "Final Energy|Residential and Commercial|Electricity", "Final Energy|Transportation|Electricity", "Final Energy|Industry|Electricity", 
             "Secondary Energy|Electricity", "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Coal|w/ CCS", "Secondary Energy|Electricity|Fossil|w/ CCS", 
             "Secondary Energy|Electricity|Gas|w/ CCS", "Secondary Energy|Electricity|Nuclear", "Final Energy|Residential and Commercial", 
             "Final Energy|Residential and Commercial|Electricity", "Final Energy|Transportation", "Final Energy|Transportation|Electricity", 
             "Final Energy|Transportation|Liquids|Biomass", "Final Energy|Industry", "Final Energy|Industry|Electricity", "Final Energy|Solids|Biomass",
             "Final Energy|Solids|Biomass|Traditional", "Secondary Energy|Electricity|Biomass|w/ CCS", "Secondary Energy|Electricity|Biomass|w/o CCS", 
             "Secondary Energy|Electricity|Non-Biomass Renewables")
all_FE <- filter(all, variable %in% vars_FE, region %in% regs)
write.table(all_FE, "data/all_FE.csv", sep=";", row.names=F)

vars_FE_overview <- c("Secondary Energy|Electricity|Fossil", "Secondary Energy|Electricity|Non-fossil", "Secondary Energy|Electricity|Non-fossil share",
                      "Final Energy|Residential and Commercial|Non-fossil", "Final Energy|Residential and Commercial|Non-fossil share",
                      "Final Energy|Transportation|Non-fossil", "Final Energy|Transportation|Non-fossil share",
                      "Final Energy|Industry|Non-fossil", "Final Energy|Industry|Non-fossil share",
                      "Final Energy|Non-fossil", "Final Energy", "Final Energy|Non-fossil share"
)
all_FE_overview <- filter(all, variable %in% vars_FE_overview, region %in% regs)
write.table(all_FE_overview, "data/all_FE_overview.csv", sep=";", row.names=F)
