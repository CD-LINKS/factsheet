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
#source('NatComPaper/Data Natcom paper.R')

# adjust data for check IAM models
convert_GWP = FALSE

# convert CH4 and N2O emissions to CO2eq
if (convert_GWP) {
  GWP_CH4 <- 25
  GWP_N2O <- 298
  # CH4
  tmp1 <- filter(all_paper, grepl("Emissions\\|CH4", variable))
  tmp1$value<-tmp1$value*GWP_CH4
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_paper, !(grepl("Emissions\\|CH4", variable)))
  all_paper <- rbind(tmp2, tmp1)
  tmp1 <- filter(all_paper_before_adj, grepl("Emissions\\|CH4", variable))
  tmp1$value<-tmp1$value*GWP_CH4
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_paper_before_adj, !(grepl("Emissions\\|CH4", variable)))
  all_paper_before_adj <- rbind(tmp2, tmp1)
  tmp1 <- filter(all_hist_paper, grepl("Emissions\\|CH4", variable))
  tmp1$value<-tmp1$value*GWP_CH4
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_hist_paper, !(grepl("Emissions\\|CH4", variable)))
  all_hist_paper <- rbind(tmp2, tmp1)
  #N2O
  tmp1 <- filter(all_paper_before_adj, grepl("Emissions\\|N2O", variable))
  tmp1$value<-tmp1$value*GWP_N2O/1000
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_paper_before_adj, !(grepl("Emissions\\|N2O", variable)))
  all_paper_before_adj <- rbind(tmp2, tmp1)
  
  tmp1 <- filter(all_paper, grepl("Emissions\\|N2O", variable))
  tmp1$value<-tmp1$value*GWP_N2O/1000
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_paper, !(grepl("Emissions\\|N2O", variable)))
  all_paper <- rbind(tmp2, tmp1)

  tmp1 <- filter(all_hist_paper, grepl("Emissions\\|N2O", variable))
  tmp1$value<-tmp1$value*GWP_N2O
  tmp1$unit <- "MtCO2eq"
  tmp2 <- filter(all_hist_paper, !(grepl("Emissions\\|N2O", variable)))
  all_hist_paper <- rbind(tmp2, tmp1)
}

# read in NDC growth rates between 2010 and 2030
NDCgrowth <- read.table("data/INDC growth rates protocol.csv", sep=";", header=TRUE)

# settings
regs_data<- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World", "Bunkers")
regs_check<- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World")
cats_check <- c("No new policies", "National policies","NDC", "Carbon budget 1000", "Carbon budget 400")
models_global <- filter(all_paper, Scope=="global", model != "AIM/CGE") %>% select(model) %>% unique() %>% as.matrix() %>% as.vector()
models_national <- filter(all_paper, Scope=="national") %>% select(model) %>% unique()
vars_GHG <- c("Emissions|Kyoto Gases", "Emissions|AFOLU", "Emissions|CO2", 
              "Emissions|CO2|Energy and Industrial Processes", "Emissions|AFOLU", 
              "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")
vars_FE <- c("Final Energy", "Final Energy|Other",  
             "Secondary Energy|Electricity", 
             "Final Energy|Residential and Commercial", 
             "Final Energy|Transportation",  
             "Final Energy|Industry")

# I. Check model gaps, data export to Excel
write.table(all_hist, "NatComPaper/data/all_hist.csv", sep=";", row.names=F)
all_paper_before_adj_emissions <- filter(all_paper_before_adj, grepl("Emissions", variable), Scope=="global", region %in% regs_data, Category%in%cats_check) %>%
  select(scenario, Category, Baseline, model, region, period, Scope, value, unit,variable)
write.table(all_paper_before_adj_emissions, "NatComPaper/data/all_before_emissions.csv", sep=";", row.names = F)

all_check_emissions_project <- filter(all_paper, grepl("Emissions", variable), Scope=="global", region %in% regs_data, Category%in%cats_check) %>%
  select(scenario, Category, Baseline, model, region, period, Scope, value, unit,variable)
all_check_emissions_hist <- filter(all_hist, grepl("Emissions", variable), region %in% regs_data) %>%
  select(scenario, Category, Baseline, model, region, period, Scope, value, unit,variable)
all_check_gdp_pop_project <- filter(all_paper, grepl("GDP|Population", variable), Scope=="global", region %in% regs_data, Category%in%cats_check) %>%
  select(scenario, Category, Baseline, model, region, period, Scope, value, unit,variable)
all_check_gdp_pop_hist <- filter(all_hist, grepl("GDP|Population", variable), region %in% regs_data) %>% 
                          select(scenario, Category, Baseline, model, region, period, Scope, value, unit,variable)
all_check_emissions <- rbind(all_check_emissions_project, all_check_emissions_hist) %>% rbind(all_check_gdp_pop_project) %>% rbind(all_check_gdp_pop_hist)
write.table(all_check_emissions, "NatComPaper/data/all_emissions.csv", sep=";", row.names = F)

all_paper_before_adj_final_energy <- filter(all_paper_before_adj, grepl("Final Energy", variable), Scope=="global", region %in% regs_data)
all_paper_before_adj_secondary_energy <- filter(all_paper_before_adj, grepl("Secondary Energy", variable), Scope=="global", region %in% regs_data)
all_paper_before_adj_energy <- rbind(all_paper_before_adj_final_energy, all_paper_before_adj_secondary_energy)
all_paper_before_adj_energy <- select(all_paper_before_adj_energy, scenario,	Category,	Baseline,	model, region, period, unit, value, Scope, variable)
write.table(all_paper_before_adj_energy, "NatComPaper/data/all_before_energy.csv", sep=";", row.names = F)

all_paper_before_adj_primary_energy <- filter(all_paper_before_adj, grepl("Primary Energy", variable), Scope=="global", region %in% regs_data)
all_paper_before_adj_gdp <- filter(all_paper_before_adj, grepl("GDP|MER", variable), Scope=="global", region %in% regs_data)
all_paper_before_adj_select <- rbind(all_paper_before_adj_emissions, all_paper_before_adj_final_energy) %>% rbind(all_paper_before_adj_secondary_energy) %>% rbind(all_paper_before_adj_primary_energy) %>% rbind(all_paper_before_adj_gdp)
write.table(all_paper_before_adj_select, "NatComPaper/data/all_before_select.csv", sep=";", row.names = F)

all_check_final_energy <- filter(all_paper, grepl("Final Energy", variable), Scope=="global", region %in% regs_data, Category%in%cats_check)
all_check_secondary_energy <- filter(all_paper, grepl("Secondary Energy", variable), Scope=="global", region %in% regs_data, Category%in%cats_check)
all_hist_final_energy <- filter(all_hist_paper, grepl("Final Energy", variable),region %in% regs_data)
all_hist_secondary_energy <- filter(all_hist_paper, grepl("Secondary Energy", variable), region %in% regs_data)
all_check_energy <- rbind(all_check_final_energy, all_check_secondary_energy)
all_hist_energy <- rbind(all_hist_final_energy, all_hist_secondary_energy)
all_energy <- rbind(all_hist_energy, all_check_energy)
all_energy <- select(all_energy, scenario,	Category,	Baseline,	model, region, period, unit, value, Scope, variable)
write.table(all_check_energy, "NatComPaper/data/all_check_energy.csv", sep=";", row.names = F)
write.table(all_energy, "NatComPaper/data/all_energy.csv", sep=";", row.names = F)

all_check_primary_energy <- filter(all_paper, grepl("Primary Energy", variable), Scope=="global", region %in% regs_data)
all_check_gdp <- filter(all_paper, grepl("GDP|MER", variable), Scope=="global", region %in% regs_data, region %in% regs_data)
all_check_select <- rbind(all_check_emissions, all_check_final_energy) %>% rbind(all_check_secondary_energy) %>% rbind(all_check_primary_energy) %>% rbind(all_check_gdp)
write.table(all_check_select, "NatComPaper/data/all_select.csv", sep=";", row.names = F)

all_check_final_energy_transport <- filter(all_paper, grepl("Final Energy\\|Transportation", variable), Scope=="global", region %in% regs_data, region %in% regs_data)
all_check_final_energy_industry <- filter(all_paper, grepl("Final Energy\\|Industry", variable), Scope=="global", region %in% regs_data, region %in% regs_data)
all_check_final_energy_buildings <- filter(all_paper, grepl("Final Energy\\|Residential and Commerical", variable), Scope=="global", region %in% regs_data, region %in% regs_data)
all_check_final_biomass <- filter(all_paper, grepl("Biomass", variable), Scope=="global", region %in% regs_data)

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
# select regionsINDC_PBLCLIMA <- filter(INDC_PBLCLIMA, region %in% regs_check)
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
Model_global_data <- filter(all_paper, Scope=="global", Category %in% cats_check_NDC, region %in% regs_check, variable%in%c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|Excl. AFOLU CO2"))

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
NDCgrowth_models_incl <- filter(all_paper, variable == "Emissions|Kyoto Gases", model %in% models_global, Category=="NDC", (period==2010 | period==2030)) %>% 
  spread(key=period, value=value) %>%
  mutate(EM_rel_2010=`2030`/`2010`) %>%
  mutate(NDC="incl") %>%
  select(model, region, NDC, EM_rel_2010)
NDCgrowth_models_excl <- filter(all_paper, variable == "Emissions|Kyoto Gases|Excl. AFOLU CO2", model %in% models_global, Category=="NDC", (period==2010 | period==2030)) %>% 
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
d_Total_GHG <- data.frame(scenario=character(), Category=character(), Baseline=character(), model=character(), region=character(),
                         period=numeric(), Scope=character(), value=numeric(), unit=character(), variable=character(), diff=numeric(), adjust=character())
for (i in 1:2) {
  if (i==1){
    all_check_graph <- all_paper_before_adj
    check<-"before"
  }
  else{
    all_check_graph <- all_paper
    check<-"after"
    }
  for (v in vars_GHG) { 
    cat(paste0(v, "-", check, "\n"))
    for (r in regs_check) { 
      #v="Emissions|Kyoto Gases|Excl. AFOLU CO2"
      #r="World"
      cat(paste0("- ", r, "\n"))
      d1 <- filter(all_check_graph, model %in% models_global, period>=2005, period<=2015, Category=="National policies", region==r, variable==v)
      d2 <- filter(all_hist_paper, model=="History", period>=2005, period<=2015, region==r, variable==v) %>% select(period, value)
      d2$period  <- as.integer(d2$period)
      d <- left_join(d1, d2, by=c('period'))
      d <- mutate(d, diff=round(value.x/value.y, digits=2), adjust=check)
      d_Total_GHG <- rbind(d_Total_GHG, d)
      #d <- rbind(d1, d2)
      g_hist <- ggplot(data=d) + 
                geom_point(aes(x=period, y=value.x, colour="model"), show.legend = TRUE) +
                geom_point(aes(x=period, y=value.y, colour="History"), show.legend = TRUE) +
                geom_line(aes(x=period, y=value.x, colour="model")) +
                geom_line(aes(x=period, y=value.y, colour="History"),linetype=2) +
                geom_text(aes(x=period, y=diff, label=diff)) +
                facet_wrap(~model) +
                theme_bw() +
                ylim(min(0,min(d1$value)),NA) + 
                xlab("Year") +
                ylab(d1$unit) +
                scale_x_continuous(breaks=seq(2005, 2015, 5)) +
                scale_colour_manual(name="Source", values=c(model="cornflowerblue", History="darkgrey")) +
                ggtitle(paste(r, "-", v, sep=""))
      gg_hist <- plot(g_hist)
      ggsave(file=paste("NatComPaper/graphs/review/hist/check_hist_GHG_", check, "_", v, "_", r, ".png", sep=""),gg_hist, height=10, width=15)
      d_plot <-rbind(mutate(d2,model="History"), select(d1,period, model, value)) %>% filter(period==2010)
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
write.table(d_Total_GHG, "NatComPaper/graphs/review/hist/compare_hist_GHG.csv", sep=";", row.names=F)

# IIIb. Compare CD-LINKS NPi and INDCi with historical IEA data
# before and after adjustments (adjust_reporting_indc_Mark)
#d_Total <- NA
#d_Total <- data.frame(colnames(all_paper))
#d_Total <- all_paper[0,]
d_Total_FE <- data.frame(scenario=character(), Category=character(), Baseline=character(), model=character(), region=character(),
                      period=numeric(), Scope=character(), value=numeric(), unit=character(), variable=character(), diff=numeric(), adjust=character())
for (i in 1:2) {
  if (i==1){
    all_check_graph <- all_paper
    check<-"after"
  }
  else{
    all_check_graph <- all_paper_before_adj
    check<-"before"
  }
  for (v in vars_FE_overview) { 
    cat(paste0(v, "-", check, "\n"))
    for (r in regs_check) { 
      #v="Final Energy"
      #r="World"
      cat(paste0("- ", r, "\n"))
      d1 <- filter(all_check_graph, model %in% models_global, period>=2005, period<=2015, Category=="National policies", region==r, variable==v)
      d2 <- filter(all_hist_paper, model=="History", period>=2005, period<=2015, region==r, variable==v) %>% select(period, value)
      d2$period  <- as.integer(d2$period)
      d <- left_join(d1, d2, by=c('period'))
      d <- mutate(d, diff=round(value.x/value.y, digits=2), adjust=check)
      d_Total_FE <- rbind(d_Total_FE, d)
      #d <- rbind(d1, d2)
      g_hist <- ggplot(data=d) + 
        geom_point(aes(x=period, y=value.x, colour="model"), show.legend = TRUE) +
        geom_point(aes(x=period, y=value.y, colour="History"), show.legend = TRUE) +
        geom_line(aes(x=period, y=value.x, colour="model")) +
        geom_line(aes(x=period, y=value.y, colour="History"),linetype=2) +
        geom_text(aes(x=period, y=diff, label=diff)) +
        facet_wrap(~model) +
        theme_bw() +
        ylim(min(0,min(d1$value)),NA) + 
        xlab("Year") +
        ylab("EJ/yr") +
        scale_x_continuous(breaks=seq(2005, 2015, 5)) +
        scale_colour_manual(name="Source", values=c(model="brown3", History="darkgoldenrod3")) +
        ggtitle(paste(r, "-", v, sep=""))
      gg_hist <- plot(g_hist)
      ggsave(file=paste("NatComPaper/graphs/review/hist/check_hist_FE_", check, "_", v, "_", r, ".png", sep=""),gg_hist, height=10, width=15)
    }
  }
}
write.table(d_Total_FE, "NatComPaper/graphs/review/hist/compare_hist_FE.csv", sep=";", row.names=F)

# Compare bunkers
var_bunkers="Emissions|Kyoto Gases"
#var_bunkers=="Emissions|CO2"
#var_bunkers=="Emissions|CO2|Energy and Industrial Processes"
d1 <- filter(all_paper, variable==var_bunkers, Category=="National policies", region=="Bunkers", model%in%models_global, 
             period>=2010, period<=2030) %>% as.data.frame()
d2_1 <- filter(EDGAR_bunkers, period>=2010, period<=2012) %>% as.data.frame() %>% select(period, region, unit, value)
d2_2 <- filter(EDGAR_bunkers_split, period>=2010, period<=2012) %>% as.data.frame() %>% select(period, region, unit, value)
d2 <- rbind(d2_1, d2_2) %>% as.data.frame()
d2$value <- as.double(d2$value)
p = ggplot()+
  geom_line(data=d1, aes(x=period, y=value, colour=model, linetype=model))+
  geom_point(data=d2, aes(x=period, y=value, shape=region), size=3)+
  #facet_wrap(~region, scales = "free")+
  scale_shape_discrete(name="EDGAR bunkers", labels=c('total', 'aviation', 'shipping')) +
  theme_bw()
plot(p)
ggsave(file=paste("NatComPaper/graphs/review/Bunkers_CO2_Kyoto", sep=""),p, height=10, width=15)


# Compare GDP
d1 <- filter(all_paper, variable=="GDP|MER", Category=="National policies", region%in%regs_check, model%in%models_global, period>=2010, period<=2030)
d2 <- filter(all_hist_paper, variable=="GDP|MER", region%in%regs_check, period>=2010, period<=2030)
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
all_FE <- filter(all_paper, variable %in% vars_FE, region %in% regs_check)
write.table(all_FE, "data/all_check_FE.csv", sep=";", row.names=F)

vars_FE_overview <- c("Secondary Energy|Electricity|Fossil", "Secondary Energy|Electricity|Non-fossil", "Secondary Energy|Electricity|Non-fossil share",
                      "Final Energy|Residential and Commercial|Non-fossil", "Final Energy|Residential and Commercial|Non-fossil share",
                      "Final Energy|Transportation|Non-fossil", "Final Energy|Transportation|Non-fossil share",
                      "Final Energy|Industry|Non-fossil", "Final Energy|Industry|Non-fossil share",
                      "Final Energy|Non-fossil", "Final Energy", "Final Energy|Non-fossil share"
)
all_check_FE_overview <- filter(all_paper, variable %in% vars_FE_overview, region %in% regs_check)
write.table(all_check_FE_overview, "data/all_check_FE_overview.csv", sep=";", row.names=F)


# OUTLIER test
source("functions/DeanDixonTest.R")

regs <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
vars_outlier <- c("Emissions|Kyoto Gases", "Emissions|Kyoto Gases|rel2010", 
                  "Emissions|CO2|FFI|gross|rel2010", "Emissions|CO2|Energy and Industrial Processes")
#var_outlier="Emissions|Kyoto Gases|Excl. AFOLU CO2|rel2010"
for (var_outlier in vars_outlier) {
cat(var_outlier, "\n")
test_DDT <- NA
test_DDT <- data.frame(region=character(), model=character(), location=character(), outlier=logical())
test_DDT$location <- factor(test_DDT$location, levels=c('small', 'large'))
IAM_models <- unique(all_paper$model)
test_DDT$model <- factor(test_DDT$model, levels=IAM_models)
alpha_DDT <- 0.05
d_regions <- NULL
for (r in regs){
  cat(paste0(r,"\n"))
  #r="BRA"
  d_NPi_largest <- filter(all_paper, Category=="National policies", region==r, period==2030, variable==var_outlier, Scope=="global") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(desc(value))
  d_NPi_smallest <- filter(all_paper, Category=="National policies", region==r, period==2030, variable==var_outlier, Scope=="global") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(value) 
  
  #http://www.sthda.com/english/wiki/ggplot2-qq-plot-quantile-quantile-graph-quick-start-guide-r-software-and-data-visualization
  #p <- qplot(sample = value, data = d_NPi_smallest) + theme_bw()
  p <- qqnorm(d_NPi_smallest$value)
  p <- qqline(d_NPi_smallest$value, col="red")

  p_tmp <- ggplot(data=d_NPi_smallest, aes(sample=value))  +
           stat_qq() +
           stat_qq_line(colour="red") +
           theme_bw()
  d_new <- ggplot_build(p_tmp)$data[[1]]
  d_new <- cbind(d_new, d_NPi_smallest$model) %>% rename(model=`d_NPi_smallest$model`)
  d_new_tmp <- mutate(d_new, region=r)
  d_regions <- rbind(d_regions, d_new_tmp)
  p <- ggplot(data=d_new, aes(theoretical,sample, label=model)) + 
               stat_qq_line(aes(sample=sample),colour="red") +
               geom_text(size=8) +
               theme_bw() +
               ggtitle(paste0("QQ-plot for ", var_outlier, "- ", r)) + 
               theme(plot.title = element_text(size = 48, face = "bold"), 
                     axis.text.x = element_text(size=40, face="bold"),
                     axis.text.y = element_text(size=40, face="bold"))
  plot(p)
  ggsave(file=paste0("NatComPaper/graphs/review/outliers/outlier_DDT_", var_outlier, "_",r,".jpg",sep=""),p,width=20,height=12,dpi=200)
  
  cat(" - test largest value for outlier\n")
  dl <- as.vector(d_NPi_largest$value)
  dl_outlier <- DeanDixonTest(dl, alpha_DDT,TRUE)
  xl <- data.frame(region=r, model=d_NPi_largest[1,]$model, location="large", outlier=dl_outlier)
  test_DDT <- rbind(test_DDT, xl)
  
  cat(" - test smallest value for outlier\n")
  ds <- as.vector(d_NPi_smallest$value)
  ds_outlier <- DeanDixonTest(ds, alpha_DDT, TRUE)
  xs <- data.frame(region=r, model=d_NPi_smallest[1,]$model, location="small", outlier=ds_outlier)
  test_DDT <- rbind(test_DDT, xs)
}
p <- ggplot(data=d_regions, aes(theoretical,sample, label=model)) + 
     stat_qq_line(aes(sample=sample),colour="red") +
     facet_wrap(~region,nrow=3, scales = "free_y") +
     theme_bw() +
     ylab("MtCO2eq") +
     ylim(0, NA) +
     theme(axis.text.x = element_text(size=40, face="bold"), axis.title.x=element_text(size=40,face="bold")) +
     theme(axis.text.y = element_text(size=40, face="bold"), axis.title.y=element_text(size=40,face="bold")) +
     theme(legend.title=element_text(size=32), legend.text=element_text(size=24), legend.position="bottom") +
     theme(strip.text.x = element_text(size=40, face="bold"),
           strip.text.y = element_text(size=40, face="bold"))+
     scale_colour_brewer(palette="Set2")+
     geom_text(size=8) +
     ggtitle(paste0("NPi scenario (2030): QQ-plot ", var_outlier, "- ", r))
plot(p)
ggsave(file=paste0("NatComPaper/graphs/review/outliers/outliers_DDT_", var_outlier, ".jpg",sep=""),p,width=20,height=12,dpi=200)

outlier_IAM <- filter(test_DDT, outlier==TRUE) %>% arrange(desc(outlier))
# html table
html.head <- paste("<head>" ,
                   '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                   "</head>",sep='\n')
html.table <- paste(print(xtable(test_DDT),type='html',"NatComPaper/graphs/review/outliers/DeanDixonTestResults_table.html"), 
                    collapse = "\n", caption=paste0("NPi scenario: Dean Dixon test for outliers, with alpha=",round(alpha_DDT,2)))
html.body <- paste("<body>", html.table,"</body>")
write(paste(html.head,html.body,sep='\n'),paste0("NatComPaper/graphs/review/outliers/DeanDixonTestResults_table_", var_outlier, ".html"))

# html table
html.head <- paste("<head>" ,
                   '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
                   "</head>",sep='\n')
html.table <- paste(print(xtable(outlier_IAM),type='html', "NatComPaper/graphs/review/outliers/outliers_table.html"), 
                    collapse = "\n", caption=paste0("NPi scenario: Dean Dixon test for outliers, with alpha=",round(alpha_DDT,2)))
html.body <- paste("<body>", html.table,"</body>")
write(paste(html.head,html.body,sep='\n'),paste0("NatComPaper/graphs/review/outliers/outliers_table_", var_outlier, ".html"))
}
# check DNE bunkers
x1<-filter(all_paper, variable%in%c("Emissions|Kyoto Gases", "Emissions|CO2|Energy and Industrial Processes"), model=="DNE21+ V.14", region%in%c("Bunkers"), 
           Category=="National policies", period>=2010, period<=2030)
x2<-filter(all_paper_before_adj, variable%in%c("Emissions|Kyoto Gases", "Emissions|CO2|Energy and Industrial Processes"), model=="DNE21+ V.14", 
                                 region%in%c("World", "R5MAF", "R5LAM", "R5ASIA", "R5OECD90+EU", "R5REF"), 
                                 Category=="National policies", period>=2010, period<=2030)
x2<-spread(x2, key=region, value=value) %>% mutate(diff=`World`-`R5MAF`-`R5LAM`-`R5ASIA`-`R5OECD90+EU`-`R5REF`)

# Check REMIND double counting
remind<-filter(all_import, VARIABLE%in%c("Emissions|Kyoto Gases", "Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases"),
                           MODEL=="REMIND-MAgPIE 1.7-3.0", 
                           REGION%in%c('World'),
                           SCENARIO%in%c('NPi_V3')
)
write.table(remind, "NatComPaper/data/remind.csv", row.names=F, sep=";")

# IMAGE update policies scenario
timer_NPi <- filter(NPi_indicators$EMISCO2EQ, year>=2010, year<=2030, 
                                              region%in%c('USA', 'INDIA', 'CHN', 'EU', 'World'),
                                              main_sector=="Total", GHG_Category=="EMISCO2EQ") %>% 
             mutate(scenario="NPi")
timer_NPi_update <- filter(NPi_update_indicators$EMISCO2EQ, year>=2010, year<=2030, 
                                                            region%in%c('USA','INDIA', 'CHN', 'EU', 'World'),
                                                            main_sector=="Total", GHG_Category=="EMISCO2EQ") %>% 
                    mutate(scenario="NPi_update")
timer_NPi_compare <- rbind(timer_NPi, timer_NPi_update) %>% as.data.frame()
timer_NPi_compare$value <- timer_NPi_compare$value/1000
ggplot(data=timer_NPi_compare) + geom_line(aes(x=year,y=value,colour=scenario)) +
                                 facet_wrap(~region,nrow=3, scales = "free_y") +
                                 theme_bw() +
                                 ylab("Total Kyoto emissions (GtCO2eq)") +
                                 ylim(0, NA) +
                                 theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
                                 theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
                                 theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") +
                                 theme(strip.text.x = element_text(size=16, face="bold"),
                                 strip.text.y = element_text(size=16, face="bold"))+
                                 scale_colour_brewer(palette="Set1")

# DRIVERS
# compare GDP between models
regs_GDP_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
GDP_models <- filter(all_paper, variable=="GDP|MER", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=GDP_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("bn US$2010)") +
  ggtitle("GDP (MER)")+
  ylim(0, NA) +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_GDP_MER.jpg",sep=""),g,width=20,height=12,dpi=200)

# compare population between models
regs_POP_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
POP_models <- filter(all_paper, variable=="Population", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=POP_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("mln")+
  ggtitle("Population") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_POP.jpg",sep=""),g,width=20,height=12,dpi=200)

pop_gdp <- rbind(POP_models, GDP_models)
write.table(pop_gdp, "NatComPaper/data/pop_gpd_models.csv", sep=";", row.names=F)

# compare energy intensity between models
regs_EI_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
EI_models <- filter(all_paper, variable=="Energy intensity of GDP", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=FE_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("EJ") +
  ggtitle("Energy intensity of GDP") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_EI.jpg",sep=""),g,width=20,height=12,dpi=200)


# compare final energy between models
regs_FE_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
FE_models <- filter(all_paper, variable=="Final Energy", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=FE_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("EJ") +
  ggtitle("Final Energy") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_FE.jpg",sep=""),g,width=20,height=12,dpi=200)

# compare final energy industry between models
regs_FE_industry_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
FE_industry_models <- filter(all_paper, variable=="Final Energy|Industry", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=FE_industry_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("EJ") +
  ggtitle("Final Energy industry") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_FE_Industry.jpg",sep=""),g,width=20,height=12,dpi=200)

# compare final energy between models
regs_FE_transport_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
FE_transport_models <- filter(all_paper, variable=="Final Energy|Transportation", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=FE_transport_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("EJ") +
  ggtitle("Final Energy transport") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_FE_transport.jpg",sep=""),g,width=20,height=12,dpi=200)

# compare final energy between models
regs_FE_buildings_models<- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "RoW")
FE_buildings_models <- filter(all_paper, variable=="Final Energy|Residential and Commercial", region%in%regs_GDP_models, Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
g <- ggplot(data=FE_buildings_models) + geom_line(aes(x=period,y=value,colour=model), size=1.5) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("EJ") +
  ggtitle("Final Energy buildings") +
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"))+
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_FE_buildings.jpg",sep=""),g,width=20,height=12,dpi=200)


# HISTORICAL EMISSIONS/ENERGY

# Historical total Kyoto
period_hist=2015
Kyoto_hist_models <- filter(all_paper, variable=="Emissions|Kyoto Gases", region%in%regs_GDP_models, 
                            Category%in%c('National policies'), period%in%c(period_hist), Scope=="global")
Kyoto_hist_primap <- filter(all_hist, variable=="Emissions|Kyoto Gases", region%in%regs_GDP_models, 
                            period%in%c(period_hist))
Kyoto_hist <- rbind(Kyoto_hist_models, Kyoto_hist_primap) 
k<-select(Kyoto_hist_primap, region, value)
g <- ggplot(data=Kyoto_hist) + geom_bar(aes(x=model,y=value,fill=model), stat="identity", position=position_dodge()) +
  geom_hline(data=k, aes(yintercept=value))+
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("(MtCO2eq)") +
  ggtitle(paste0(period_hist, " Total GHG emissions"))+
  theme(axis.text.x = element_text(angle=90, size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE))
ggsave(file=paste("NatComPaper/graphs/review",paste0("/Compare_Kyoto_hist_", period_hist, ".jpg"),sep=""),g,width=20,height=12,dpi=200)

# Historical total Kyoto excl AFOLU CO2
period_hist=2010
Kyoto_hist_models <- filter(all_paper, variable=="Emissions|Kyoto Gases|Excl. AFOLU CO2", region%in%regs_GDP_models, 
                            Category%in%c('National policies'), period%in%c(period_hist), Scope=="global")
Kyoto_hist_primap <- filter(all_hist, variable=="Emissions|Kyoto Gases|Excl. AFOLU CO2", region%in%regs_GDP_models, 
                            period%in%c(period_hist))
Kyoto_hist <- rbind(Kyoto_hist_models, Kyoto_hist_primap) 
k<-select(Kyoto_hist_primap, region, value)
g <- ggplot(data=Kyoto_hist) + geom_bar(aes(x=model,y=value,fill=model), stat="identity", position=position_dodge()) +
  geom_hline(data=k, aes(yintercept=value))+
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("(MtCO2eq)") +
  ggtitle(paste0(period_hist, " Total GHG emissions (excl AFOLU CO2)"))+
  theme(axis.text.x = element_text(angle=90, size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE))
ggsave(file=paste("NatComPaper/graphs/review",paste0("/Compare_Kyoto_hist_exclAFOLU", period_hist, ".jpg"),sep=""),g,width=20,height=12,dpi=200)


# POLICY IMPLEMENTATION
# Show NPi scenario per model
NPi_results <- filter(all_paper, variable=="Emissions|Kyoto Gases", region%in%regs_GDP_models, 
                      Category%in%c('National policies'), period>=2010, period<=2030, Scope=="global")
p_NPi <- ggplot(NPi_results) + geom_line(aes(x=period, y=value, colour=model)) +
         facet_wrap(~region, scales = "free_y") +
         ylim(0,NA) +
         theme_bw()
plot(p_NPi)
p_NPi_reg <- ggplot(filter(NPi_results, region=="World")) + geom_line(aes(x=period, y=value, colour=model)) +
  ylim(0,NA) +
  theme_bw()
plot(p_NPi_reg)

# compare total reductions between models
Reduction_abs_models <- filter(all_paper, variable=="Emissions|Kyoto Gases", region%in%regs_GDP_models, 
                           Category%in%c('No policy', 'National policies'), period>=2010, period<=2030, Scope=="global") %>%
                    select(Category, model, period, region, value) %>%
                    spread(key=Category, value=value) %>%
                    mutate(reduction=`No policy`-`National policies`)
g <- ggplot(data=Reduction_abs_models) + geom_line(aes(x=period,y=reduction,colour=model)) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("Total GHG emissions (MtCO2eq") +
  ggtitle("Absolut reductions relative to no policy scenario")+
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(limits=c(0,NA), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold")) +
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_Reductions_abs.jpg",sep=""),g,width=20,height=12,dpi=200)

Reduction_rel_models <- filter(all_paper, variable=="Emissions|Kyoto Gases", region%in%regs_GDP_models, 
                               Category%in%c('No policy', 'National policies'), period>=2010, period<=2030, Scope=="global") %>%
  select(Category, model, period, region, value) %>%
  spread(key=Category, value=value) %>%
  mutate(reduction=ifelse(is.na(`No policy`), 0, `National policies`/`No policy`-1)) %>%
  select(model, period, region, reduction)
g <- ggplot(data=Reduction_rel_models) + geom_line(aes(x=period,y=reduction,colour=model)) +
  facet_wrap(~region,nrow=3, scales = "free_y") +
  theme_bw() +
  ylab("Total GHG emissions (MtCO2eq") +
  ggtitle("Relative reductions relative to no policy scenario")+
  theme(axis.text.x = element_text(size=16, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
  theme(axis.text.y = element_text(size=16, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
  theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold")) +
  scale_colour_brewer(palette="Set2")
ggsave(file=paste("NatComPaper/graphs/review","/Compare_Reductions_rel.jpg",sep=""),g,width=20,height=12,dpi=200)


# decompostion of drivers NPi reduction relative to NoPolicy by 2030
regs_drivers<- c("World", "USA", "RUS", "JPN", "IND", "EU", "CHN", "BRA")
#drivers <- c("EM_growth_NPi", "EM_history", "Population", "GDP_per_capita", "EM_per_GDP", "Policy_implementation")
drivers <- c("Error_term", "Policy_implementation", "EM_per_GDP", "GDP_per_capita", "Population", "EM_history","EM_growth_NPi")

# read in drivers 2015
Drivers_NPi <- read.table("data/Compare NPi ranges.csv", sep=";", header=TRUE)
Drivers_NPi <- filter(Drivers_NPi, !(is.na(EM_growth_NPi)))
Drivers_NPi <- gather(Drivers_NPi, 3:ncol(Drivers_NPi), key="driver", value=value)
Drivers_NPi$driver <- factor(Drivers_NPi$driver, levels=drivers)    

Drivers_NPi_median <- group_by(Drivers_NPi, driver, country) %>%
  summarize(median=median(value),
            min=quantile(value,prob=0.1),max=quantile(value,prob=0.9)) %>%
  as.data.frame()
Drivers_NPi_median$country <- factor(Drivers_NPi_median$country, levels=regs_drivers)   

p_drivers_median <- ggplot(data=Drivers_NPi_median) + 
  geom_bar(aes(x=driver, y=median, fill=driver), stat="identity", position=position_dodge(width=1), width=0.6) +
  geom_errorbar(aes(x=driver, ymin=min, ymax=max), width=.2,position=position_dodge(.9)) +
  facet_wrap(~country, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  ggtitle("Decomposition drivers of change in total GHG emissions excluding AFOLU in National policies scenario relative to no policy scenario") +
  scale_fill_manual(values=c('moccasin', 'khaki4', 'khaki3', 'khaki2', 'khaki1', 'khaki4','blue3'),
                    labels=c("EM_growth_NPi"="Emission growth 2015-2030", "EM_history"="Difference with historical emissions", 
                             "Polulation"="Population", "GDP_per_capita"="GDP per capita", "EM_per_GDP"="Emissions per GDP", 
                             "Policy_implementation"="Policy impact", "Error_term"="Unexplained"),
                    guide = guide_legend(reverse=TRUE))
plot(p_drivers_median)

# ALTERNATIVE
# decompostion of drivers NPi reduction relative to NoPolicy by 2030
regs_drivers<- c("World", "USA", "RUS", "JPN", "IND", "EU", "CHN", "BRA")
#drivers <- c("EM_growth_NPi", "EM_history", "Population", "GDP_per_capita", "EM_per_GDP", "Policy_implementation")
drivers <- c("Error_term", "Policy_implementation", "EM_growth_NoPolicy", "EM_history","EM_growth_NPi")

year_hist <- 2015
vars_decomp <- c('Emissions|Kyoto Gases', 'Population', 'GDP|MER')
scens_decomp <- c('No policy', 'National policies')
decomposition_hist <- filter(all_hist, period==2015, variable=="Emissions|Kyoto Gases", region%in%regs_check) %>%
  select(-unit, -Baseline, -Scope, -scenario)
decomposition_projections <- filter(all_paper, Category%in%scens_decomp, period%in%c(2015, 2030), Scope=="global", 
                                    model!='GEM-E3', variable%in%vars_decomp, region%in%regs_check) %>%
  select(-unit, -Baseline, -Scope, -scenario)

decomposition_hist_tmp <- decomposition_hist %>% 
  gather(var_tmp, val_tmp, -(c(model,region, variable, Category, variable, period))) %>%
  unite(temp, c(Category, variable, period), var_tmp) %>%
  spread(temp, val_tmp)
decomposition_projections_tmp <- decomposition_projections %>% 
  gather(var_tmp, val_tmp, -(c(model,region, variable, Category, variable, period))) %>%
  unite(temp, c(Category, variable, period), var_tmp) %>%
  spread(temp, val_tmp)
decomposition <- left_join(decomposition_hist_tmp, decomposition_projections_tmp, by=c('region')) %>%
  rename(model=model.y) %>% select(-model.x) 
decomposition <-  select(decomposition, model, region, everything())

decomposition <- transmute(decomposition, model=model, region=region,
                           EM_growth_NPi=`National policies_Emissions|Kyoto Gases_2030_value`-`National policies_Emissions|Kyoto Gases_2015_value`,
                           EM_history=`National policies_Emissions|Kyoto Gases_2015_value`-`Historical_Emissions|Kyoto Gases_2015_value`,
                           EM_growth_NoPolicy=`No policy_Emissions|Kyoto Gases_2030_value`-`No policy_Emissions|Kyoto Gases_2015_value`,
                           Policy_implementation=`No policy_Emissions|Kyoto Gases_2030_value`-`National policies_Emissions|Kyoto Gases_2030_value`)
decomposition <- mutate(decomposition, Error_term=EM_growth_NPi-EM_history-EM_growth_NoPolicy-Policy_implementation)
decomposition <- gather(decomposition, EM_growth_NPi, EM_history, EM_growth_NoPolicy, Policy_implementation, Error_term, key="driver", value=value)

decomposition$driver <- factor(decomposition$driver, levels=drivers)   
decomposition$region <- factor(decomposition$region, levels=regs_drivers)

decomposition_median <- group_by(decomposition, driver, region) %>%
  summarize(median=median(value),
            min=quantile(value,prob=0.1),max=quantile(value,prob=0.9)) %>%
  as.data.frame()
decomposition_median$driver <- factor(decomposition_median$driver, levels=drivers)   
decomposition_median$region <- factor(decomposition_median$region, levels=regs_drivers)


p_drivers_median <- ggplot(data=decomposition_median) + 
  geom_bar(aes(x=driver, y=median, fill=driver), stat="identity", position=position_dodge(width=1), width=0.6) +
  geom_errorbar(aes(x=driver, ymin=min, ymax=max), width=.2,position=position_dodge(.9)) +
  facet_wrap(~region, scales = "free_x") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values=c('moccasin', 'khaki4', 'khaki3', 'khaki2', 'blue3'),
                    labels=c("EM_growth_NPi"="Emission growth National policies scenario 2015-2030", 
                             "EM_history"="Difference with historical emissions", 
                             "EM_growth_NoPolicy"="Emission growth 2015-3030 No policy scenario",
                             "Policy_implementation"="Policy impact", 
                             "Error_term"="Unexplained"),
                    guide = guide_legend(reverse=TRUE)
                    ) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.y=element_blank())
plot(p_drivers_median)

# only for World
decomposition_world <- filter(decomposition, region=="World")
decomposition_world <- mutate(decomposition_world, estimate="model")
tmp <- read.table("NatComPaper/data/Additional_reductions.csv", sep=";", header=TRUE)
decomposition_world <- rbind(decomposition_world, tmp)
decomposition_world_median <- group_by(decomposition_world, driver, estimate, region) %>%
  summarize(median=median(value),
            min=quantile(value,prob=0.1),max=quantile(value,prob=0.9)) %>%
  as.data.frame()
decomposition_world_median$driver <- factor(decomposition_world_median$driver, levels=drivers)   
decomposition_world_median$region <- factor(decomposition_world_median$region, levels=regs_drivers)
decomposition_world_median$estimate <- factor(decomposition_world_median$estimate, levels=c('outside', 'model'))

# for error bar we need to sum the policy impact uncertainties
decomposition_world_error <- spread(decomposition_world, key=estimate, value=value)
decomposition_world_error[is.na(decomposition_world_error)] <- 0
decomposition_world_error <- mutate(decomposition_world_error, value=model+outside) %>% select(-outside, -model)
decomposition_world_error_median <- group_by(decomposition_world_error, driver, region) %>%
  summarize(median=median(value),
            min=quantile(value,prob=0.1),max=quantile(value,prob=0.9)) %>%
  as.data.frame()
decomposition_world_error_median$driver <- factor(decomposition_world_error_median$driver, levels=drivers)   
decomposition_world_error_median$region <- factor(decomposition_world_error_median$region, levels=regs_drivers)

p_drivers_world_median <- ggplot(data=decomposition_world_median) + 
  #geom_bar(aes(x=driver, y=median, fill=driver), stat="identity", position=position_dodge(width=1), width=0.6) +
  geom_bar(aes(x=driver, y=median, fill=interaction(driver, estimate)), stat="identity") +
  geom_errorbar(data=decomposition_world_error_median, aes(x=driver, ymin=min, ymax=max), width=.2,position=position_dodge(.9)) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(name = "Driver",
                    values=c('moccasin', 'khaki4', 'khaki3', 'khaki2', 'khaki1', 'blue3'),
                    breaks=c("EM_growth_NPi.model", 
                             "EM_history.model", 
                             "EM_growth_NoPolicy.model",
                             "Policy_implementation.outside", 
                             "Policy_implementation.model",
                             "Error_term.model"),
                    labels=c("EM_growth_NPi.model"="Emission growth National policies scenario", 
                             "EM_history.model"="Historical calibration", 
                             "EM_growth_NoPolicy.model"="Emission growth No policy scenario",
                             "Policy_implementation.outside"="Policy impact not covered by model", 
                             "Policy_implementation.model"="Policy impact",
                             "Error_term.model"="Other")
                    ) +
  scale_y_continuous(labels = comma) +
  
  xlab("Decomposition of reductions") +
  theme(axis.title.x = element_text(size = 24, face="bold")) +
  theme(axis.text.x=element_text(size = 24, face="bold")) +
  
  #ylab("") +
  ylab("Global emission growth from 2015 to 2030 (MtCO2eq)") +
  theme(axis.title.y = element_text(size = 24, face="bold")) +
  theme(axis.text.y=element_blank()) +
  #theme(axis.text.y=element_text(size = 24, face="bold")) +
  
  theme(legend.position="right",
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30))
plot(p_drivers_world_median)
ggsave(file=paste("NatComPaper/graphs","/Figure1_methods.jpg",sep=""),p_drivers_world_median,width=20,height=12,dpi=200)


# show differences SSP1, SSP2, SSP3

data_SSP <- filter(all_paper_SSP, model%in%c('AIM V2.1', 'MESSAGEix-GLOBIOM_1.0'), variable=="Emissions|Kyoto Gases", 
                   region=="World", period>=2010, period<=2030)
p_SSP <- ggplot(data=data_SSP) + 
         geom_line(aes(x=period, y=value, group=scenario, colour=Category, linetype=SSP), size=2) +
         facet_wrap(~model, scales = "free_y") +
         ylim(0,NA) +
         theme(axis.text.x = element_text(size=24, face="bold"), axis.title.x=element_text(size=14,face="bold")) +
         theme(axis.text.y = element_text(size=24, face="bold"), axis.title.y=element_text(size=14,face="bold")) +
         theme(legend.title=element_text(size=16), legend.text=element_text(size=16), legend.position="bottom") + 
         theme(strip.text.x = element_text(size=16, face="bold"),
               strip.text.y = element_text(size=16, face="bold")) +
         theme_bw()
plot(p_SSP)

# check DNE
x1 <-filter(all_paper_before_adj, model=="DNE21+ V.14", region%in%c("World","R5MAF", "R5LAM", "R5ASIA","R5OECD90+EU","R5REF"), 
            Category=="National policies", variable%in%c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU"))
write.table(x1, "data/tmp.csv", sep=";", row.names=F)
x2<-filter(x1, region!="World") %>% group_by(period, variable) %>% summarize(value=sum(value))
View(x2)

# check projections until 2100
x <- filter(all_paper, variable=="Emissions|Kyoto Gases", Category %in% c('National policies', 'NDC'), region=="World")
ggplot(data=x) + 
  geom_line(aes(x=period, y=value, colour=model)) +
  facet_wrap(~Category) +
  ylim(0,NA) +
  theme_bw()
