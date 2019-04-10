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


# check update
#c("PV", "CSP", "Wind Onshore", "Wind Offshore", "Hydro", "Other Renewable", "Nuclear", "Conv. Coal", "Conv. Oil", "Conv. Natural gas", "Waste")
#c("CAN", USA   MEX   RCAM  BRA   RSAM  NAF   WAF   EAF   SAF   WEU   CEU   TUR   UKR   STAN  RUS   ME    INDIA KOR   CHN   SEAS  INDO  JAP   OCE   RSAS  RSAF  World EU 
tech=c("PV", "CSP", "Wind Onshore", "Wind Offshore", "Hydro", "Other Renewable", "Nuclear", "Conv. Coal", "Conv. Oil", "Conv. Natural gas", "Waste")
tech_plot = "Hydro"
cntr="INDIA"

d_NoPolicy <- filter(NoPolicy$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech) %>% mutate(scenario="No policy") %>% mutate(version="current")
d_NPi      <- filter(NPi$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech)  %>% mutate(scenario="NPi")  %>% mutate(version="current")
d_NDC      <- filter(INDCi$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech)  %>% mutate(scenario="INDCi")  %>% mutate(version="current")

d_NoPolicy_V4 <- filter(NoPolicy_V4$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech) %>% mutate(scenario="No policy") %>% mutate(version="update")
d_NPi_V4      <- filter(NPi_V4$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech)  %>% mutate(scenario="NPi") %>% mutate(version="update")
d_NDC_V4      <- filter(INDCi_V4$ElecCap, region==cntr, year>=2010, year<=2030, energy_technology%in%tech)  %>% mutate(scenario="INDCi") %>% mutate(version="update")
d <- rbind(d_NoPolicy, d_NPi) %>% rbind(d_NDC) %>% 
                 rbind(d_NoPolicy_V4) %>% rbind(d_NPi_V4) %>% rbind(d_NDC_V4)
d$value <- d$value/1000
d$scenario <- factor(d$scenario,levels=c("No policy", "NPi","INDCi"))
p1 <- ggplot(data=filter(d, energy_technology==tech_plot)) + geom_line(aes(x=year, y=value, colour=scenario)) + 
                             facet_wrap(~version) +
                             theme_bw() + 
                             ggtitle(paste0(cntr, "-", tech_plot, " capacity")) +
                             ylab("Capacity (GW)")
p2 <- ggplot(data=filter(d, energy_technology==tech_plot)) + geom_line(aes(x=year, y=value, colour=scenario, linetype=version)) + 
                     theme_bw() + 
                     ggtitle(paste0(cntr, "-", tech_plot, " capacity")) +
                     ylab("Capacity (GW)")

y=2030
p3 <- ggplot(data=filter(d,year==y)) + geom_bar(aes(scenario, value, group = interaction(scenario, version, energy_technology), fill = energy_technology), stat="identity", position="stack") +
      facet_wrap(~version) +
      scale_x_discrete(limits=c("No policy", "NPi","INDCi")) +
      ggtitle(paste0(cntr, "-", y)) +
      ylab("Capacity (GW)")

d_NoPolicy_indicators <- filter(NoPolicy_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="current")
d_NPi_indicators      <- filter(NPi_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030)  %>% mutate(scenario="NPi")  %>% mutate(version="current")
d_NDC_indicators      <- filter(INDCi_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030)  %>% mutate(scenario="INDCi")  %>% mutate(version="current")

d_NoPolicy_V4_indicators <- filter(NoPolicy_V4_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="update")
d_NPi_V4_indicators      <- filter(NPi_V4_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030)  %>% mutate(scenario="NPi") %>% mutate(version="update")
d_NDC_V4_indicators      <- filter(INDCi_V4_indicators$NonFossilElecShare, region==cntr, year>=2010, year<=2030)  %>% mutate(scenario="INDCi") %>% mutate(version="update")
d_ind <- rbind(d_NoPolicy_indicators, d_NPi_indicators) %>% rbind(d_NDC_indicators) %>% 
         rbind(d_NoPolicy_V4_indicators) %>% rbind(d_NPi_V4_indicators) %>% rbind(d_NDC_V4_indicators)
d_ind$scenario <- factor(d_ind$scenario,levels=c("No policy", "NPi","INDCi"))

p4 <- ggplot(data=d_ind) + geom_line(aes(x=year, y=value, colour=scenario)) + 
  facet_wrap(~version) +
  theme_bw() + 
  ggtitle(paste0(cntr, "-Non-fossil share")) +
  ylab("(%)")

lay_fig1<-rbind(c(1,2),c(3,4))
fig=grid.arrange(p1,p2,p3, p4)
plot(fig)

# check all capacities
tech_plot_cap <- c("PV", "Wind Onshore", "Wind Offshore", "Hydro", "Other Renewable", "Nuclear", "Conv. Coal", "Waste")
p_cap <- ggplot(data=filter(d, energy_technology%in%tech_plot_cap)) + geom_line(aes(x=year, y=value, colour=scenario)) + 
                         facet_wrap(version~energy_technology, nrow=2) +
                         theme_bw() + 
                         ggtitle(paste0(cntr, " capacity")) +
                         ylab("Capacity (GW)")
plot(p_cap)

# check CO2 intensity reduction relative to 2015
# check REN TPES China
d_CHN_NoPolicy_indicators <- filter(NoPolicy_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="current")
d_CHN_NPi_indicators      <- filter(NPi_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="NPi")  %>% mutate(version="current")
d_CHN_NDC_indicators      <- filter(INDCi_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="INDCi")  %>% mutate(version="current")

d_CHN_NoPolicy_V4_indicators <- filter(NoPolicy_V4_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="update")
d_CHN_NPi_V4_indicators      <- filter(NPi_V4_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="NPi") %>% mutate(version="update")
d_CHN_NDC_V4_indicators      <- filter(INDCi_V4_indicators$RenNucTPESShare_CHN_acccounting, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="INDCi") %>% mutate(version="update")
d_CHN_ind <- rbind(d_CHN_NoPolicy_indicators, d_CHN_NPi_indicators) %>% rbind(d_CHN_NDC_indicators) %>% 
  rbind(d_CHN_NoPolicy_V4_indicators) %>% rbind(d_CHN_NPi_V4_indicators) %>% rbind(d_CHN_NDC_V4_indicators)
d_CHN_ind$scenario <- factor(d_CHN_ind$scenario,levels=c("No policy", "NPi","INDCi"))

p1_chn <- ggplot(d_CHN_ind) + geom_line(aes(x=year, y=value, colour=scenario)) +   
                              facet_wrap(~version, scales = "free_y") + 
                              theme_bw() + 
                              ylim(0,NA) +
                              ggtitle("China: Non-fossil share in TPES (substitution method)")

d_CHN_NoPolicy_indicators <- filter(NoPolicy_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="current")
d_CHN_NPi_indicators      <- filter(NPi_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="NPi")  %>% mutate(version="current")
d_CHN_NDC_indicators      <- filter(INDCi_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="INDCi")  %>% mutate(version="current")

d_CHN_NoPolicy_V4_indicators <- filter(NoPolicy_V4_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030) %>% mutate(scenario="No policy") %>% mutate(version="update")
d_CHN_NPi_V4_indicators      <- filter(NPi_V4_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="NPi") %>% mutate(version="update")
d_CHN_NDC_V4_indicators      <- filter(INDCi_V4_indicators$CO2_intensity_index, region=="CHN", year>=2010, year<=2030)  %>% mutate(scenario="INDCi") %>% mutate(version="update")
d_CHN_ind <- rbind(d_CHN_NoPolicy_indicators, d_CHN_NPi_indicators) %>% rbind(d_CHN_NDC_indicators) %>% 
  rbind(d_CHN_NoPolicy_V4_indicators) %>% rbind(d_CHN_NPi_V4_indicators) %>% rbind(d_CHN_NDC_V4_indicators)
d_CHN_ind$scenario <- factor(d_CHN_ind$scenario,levels=c("No policy", "NPi","INDCi"))

p2_chn <- ggplot(d_CHN_ind) + geom_line(aes(x=year, y=value, colour=scenario)) + 
                              facet_wrap(~version, scales = "free_y") + 
                              theme_bw() + 
                              ylim(0,NA) + 
                              ggtitle("China: CO2 intensity (GDP) reduction relative to 2015")

lay_fig1<-rbind(c(1,2))
fig_chn=grid.arrange(p1_chn,p2_chn)
plot(fig_chn)
