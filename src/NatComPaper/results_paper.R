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


options(scipen=999)
options(digits = 2)

regs_paper<- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World", "Bunkers")
regs_paper_excl_bunkers <- c( "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA", "World")

all_paper_results <- filter(all_paper, Scope=="global", period>=2010, period<=2050, region%in%regs_paper) %>%
                     select(-Baseline, -scenario, -Scope)# Data processing ---------------------------------------------------------


# results in 2030
results_2030 <- filter(all_paper_results, period==2030, region!="Bunkers", 
                            variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER"),
                            Category%in%c('No new policies', 'National policies','NDC', 'Carbon budget 1000','Carbon budget 400'))
results_2030_stat <- filter(all_paper_results, period==2030, region!="Bunkers", 
                       variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER"),
                       Category%in%c('No new policies', 'National policies','NDC', 'Carbon budget 1000','Carbon budget 400')) %>% 
                group_by(Category, region, period, unit, variable) %>%
                summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                          mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T)) %>%
                arrange(variable, Category)
write.table(results_2030_stat, "NatComPaper/data/results_paper/results_2030_stat.csv", row.names=F, sep=";")

results_2030_2015_stat <- filter(all_paper_results, period%in%c(2015, 2030), region!="Bunkers", 
                       variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER"),
                       Category%in%c('No new policies', 'National policies','NDC', 'Carbon budget 1000','Carbon budget 400')) %>% 
                spread(key=period, value=value) %>%
                mutate(improvement=`2030`/`2015`-1) %>%
                group_by(Category, region, unit, variable) %>%
                summarise(median=median(improvement, na.rm=T), perc_10=quantile(improvement, probs=0.1, na.rm=T), perc_90=quantile(improvement, probs=0.9, na.rm=T),
                          mean=mean(improvement, na.rm=T), min=min(improvement, na.rm=T), max=max(improvement, na.rm=T)) %>%
                arrange(variable, Category)
write.table(results_2030_2015_stat, "NatComPaper/data/results_paper/results_2030_2015_stat.csv", row.names=F, sep=";")

# Implementation impact: reduction relative to NoPolicy
Implementation_impact_abs <- filter(all_paper_results, Category%in%c('No new policies', 'National policies'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`No new policies`-`National policies`)
Implementation_impact_abs_stat <- select(Implementation_impact_abs, -`No new policies`,-`National policies`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Implementation_impact_abs_stat, "NatComPaper/data/results_paper/Implementation_impact_abs_stat.csv", row.names=F, sep=";")

Implementation_impact_rel <- filter(all_paper_results, Category%in%c('No new policies', 'National policies'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`National policies`/`No new policies`-1)
Implementation_impact_rel_stat <- select(Implementation_impact_rel, -`No new policies`,-`National policies`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Implementation_impact_rel_stat, "NatComPaper/data/results_paper/Implementation_impact_rel_stat.csv", row.names=F, sep=";")

# Implementation gap: difference between national policies and NDC
Implementation_gap_abs <- filter(all_paper_results, Category%in%c('National policies', 'NDC'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`National policies`- `NDC`)
Implementation_gap_abs_stat <- select(Implementation_gap_abs, -`National policies`, -`NDC`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Implementation_gap_abs_stat, "NatComPaper/data/results_paper/Implementation_gap_abs_stat.csv", row.names=F, sep=";")

Implementation_gap_rel <- filter(all_paper_results, Category%in%c('National policies', 'NDC'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`NDC`/`National policies`-1)
Implementation_gap_rel_stat <- select(Implementation_gap_rel, -`National policies`,-`NDC`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Implementation_gap_rel_stat, "NatComPaper/data/results_paper/Implementation_gap_rel_stat.csv", row.names=F, sep=";")

# Ambition gap, difference between NPi and Carbon budget 1000 and 400
Ambition_gap_2C_abs <- filter(all_paper_results, Category%in%c('National policies', 'Carbon budget 1000'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`National policies`-`Carbon budget 1000`)
Ambition_gap_2C_abs_stat <- select(Ambition_gap_2C_abs, -`National policies`,-`Carbon budget 1000`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Ambition_gap_2C_abs_stat, "NatComPaper/data/results_paper/Ambition_gap_2C_abs_stat.csv", row.names=F, sep=";")

Ambition_gap_2C_rel <- filter(all_paper_results, Category%in%c('National policies', 'Carbon budget 1000', 'Carbon budget 400'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`Carbon budget 1000`/`National policies`-1)
Ambition_gap_2C_rel_stat <- select(Ambition_gap_2C_rel, -`National policies`,-`Carbon budget 1000`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Ambition_gap_2C_rel_stat, "NatComPaper/data/results_paper/Ambition_gap_2C_rel_stat.csv", row.names=F, sep=";")

Ambition_gap_1_5C_abs <- filter(all_paper_results, Category%in%c('National policies', 'Carbon budget 400'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`National policies`-`Carbon budget 400`)
Ambition_gap_1_5C_abs_stat <- select(Ambition_gap_1_5C_abs, -`National policies`,-`Carbon budget 400`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Ambition_gap_1_5C_abs_stat, "NatComPaper/data/results_paper/Ambition_gap_1_5C_abs_stat.csv", row.names=F, sep=";")

Ambition_gap_1_5C_rel <- filter(all_paper_results, Category%in%c('National policies', 'Carbon budget 400', 'Carbon budget 400'), period==2030, region!="Bunkers",
                               variable%in%c("Emissions|Kyoto Gases", "Final Energy|Non-fossil share", "Energy Intensity of GDP|MER")) %>%
                               spread(key=Category, value=value) %>%
                               mutate(reduction=`Carbon budget 400`/`National policies`-1)
Ambition_gap_1_5C_rel_stat <- select(Ambition_gap_1_5C_rel, -`National policies`,-`Carbon budget 400`) %>%
                            group_by(region, period, unit, variable) %>%
                            summarise(median=median(reduction, na.rm=T), perc_10=quantile(reduction, probs=0.1, na.rm=T), perc_90=quantile(reduction, probs=0.9, na.rm=T),
                                      mean=mean(reduction, na.rm=T), min=min(reduction, na.rm=T), max=max(reduction, na.rm=T)) %>%
                            arrange(variable)
write.table(Ambition_gap_1_5C_rel_stat, "NatComPaper/data/results_paper/Ambition_gap_1_5C_rel_stat.csv", row.names=F, sep=";")

# mitigation costs
mitigation_costs <- filter(all_paper_results, Category%in%c('National policies', 'Carbon budget 1000', 'Carbon budget 400'), period==2030, region!="Bunkers",
                           variable%in%c("Mitigation Costs"))
mitigation_costs_stat <- group_by(mitigation_costs, Category, region, period, unit, variable) %>%
                         summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                         mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T))
write.table(mitigation_costs_stat, "NatComPaper/data/results_paper/mitigation_costs.csv", row.names=F, sep=";")

# %-of Kyoto emissions from gap between NoPolicy and 2C/1.5C covered by national policies
Implementation_GHG_impact_abs <- filter(Implementation_impact_abs, variable=="Emissions|Kyoto Gases") %>%
   select(-`No new policies`, -`National policies`)

Gap_GHG_NoPolicy_2C_abs <- filter(all_paper_results, Category%in%c('No new policies', 'Carbon budget 1000'), period==2030, region!="Bunkers",
                              variable%in%c("Emissions|Kyoto Gases")) %>%
                       spread(key=Category, value=value) %>%
                       mutate(reduction=`No new policies`-`Carbon budget 1000`) %>%
                       select(-`No new policies`, -`Carbon budget 1000`)
Covered_by_policy_2C <- inner_join(Gap_GHG_NoPolicy_2C_abs, Implementation_GHG_impact_abs, by=c('model', 'region', 'period', 'unit', 'variable')) %>%
                        mutate(coverage=100*reduction.y/reduction.x) %>%
                        select(-reduction.x, reduction.y) %>%
                        rename(value=coverage)
Covered_by_policy_2C_stat <- group_by(Covered_by_policy_2C, region, period, unit, variable) %>%
                             summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                             mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T))

Gap_GHG_NoPolicy_1.5C_abs <- filter(all_paper_results, Category%in%c('No new policies', 'Carbon budget 400'), period==2030, region!="Bunkers",
                                  variable%in%c("Emissions|Kyoto Gases")) %>%
                                  spread(key=Category, value=value) %>%
                                  mutate(reduction=`No new policies`-`Carbon budget 400`) %>%
                                  select(-`No new policies`, -`Carbon budget 400`)
Covered_by_policy_1.5C <- inner_join(Gap_GHG_NoPolicy_1.5C_abs, Implementation_GHG_impact_abs, by=c('model', 'region', 'period', 'unit', 'variable')) %>%
                          mutate(coverage=100*reduction.y/reduction.x) %>%
                          select(-reduction.x, reduction.y) %>%
                          rename(value=coverage)
Covered_by_policy_1.5C_stat <- group_by(Covered_by_policy_1.5C, region, period, unit, variable) %>%
                               summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                               mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T))

# budgets for NPi and NDC scenario
budget_NPi_NDC <- filter(all_paper, variable=="Carbon budget", Category %in% c('No new policies', 'National policies', 'NDC'), region=="World", period%in%c(2050,2100))
budget_NPi_NDC_stat <- group_by(budget_NPi_NDC, region, period, unit, variable, Category) %>%
                       summarise(median=median(value, na.rm=T), perc_10=quantile(value, probs=0.1, na.rm=T), perc_90=quantile(value, probs=0.9, na.rm=T),
                       mean=mean(value, na.rm=T), min=min(value, na.rm=T), max=max(value, na.rm=T)) %>%
                       gather(6:ncol(budget_NPi_NDC_stat), key='statistic', value='value')  #%>%
                       #arrange(Category)
write.table(budget_NPi_NDC_stat, "NatComPaper/data/results_paper/budget_NPi_NDC_stat.csv", row.names=F, sep=";")

# check why non-fossil share (percentile) is lower in 1.5C scenario
x1 <- mutate(Ambition_gap_1_5C_abs, scen="1.5C") %>% rename(mitigation=`Carbon budget 400`)
x2 <- mutate(Ambition_gap_2C_abs, scen="2C") %>% rename(mitigation=`Carbon budget 1000`)
x3 <- rbind(x1,x2) %>% select(-mitigation) %>% spread(key='scen', value=reduction)

x4 <- inner_join(Ambition_gap_1_5C_abs, Ambition_gap_2C_abs, by=c('model', 'region', 'period', 'unit', 'variable'))
# --> IMAGE model
y1 <- filter(NoPolicy_indicators$NonFossilElecShare, region=="EU") %>% mutate(scen="No new policies")
y2 <- filter(NPi_indicators$NonFossilElecShare, region=="EU") %>% mutate(scen="National policies")
y3 <- filter(NPi2020_1000_indicators$NonFossilElecShare, region=="EU")  %>% mutate(scen="2C")
y4 <- filter(NPi2020_400_indicators$NonFossilElecShare, region=="EU")%>% mutate(scen="1.5C") 
y <- rbind(y1, y2) %>% rbind(y3) %>% rbind(y4)
ggplot(data=filter(y, year>=2015, year<=2030)) + geom_line(aes(x=year, y=value, colour=scen)) +
   ylim(0, NA) + theme_bw()

z <- filter(all_paper, variable %in% c("Final Energy", "Secondary Energy|Electricity|Non-fossil share", "Final Energy|Electricity", 
                                        "Final Energy|Biomass|Excl. traditional", "Final Energy|Other Renewables"),
                       Category %in% c('National policies', 'Carbon budget 1000', 'Carbon budget 400'),
                                         region=="EU", model=="POLES CDL") %>% select(-unit) %>%
      spread(key=variable, value=value) %>%
      mutate(`Final Energy|Electricity|Renewable`=(1/100)*(`Secondary Energy|Electricity|Non-fossil share`)*(`Final Energy|Electricity`)) %>%
      select(-`Secondary Energy|Electricity|Non-fossil share`, -`Final Energy|Electricity`)
z <- gather(z, 8:ncol(z), key='variable', value=value)
z$Category <- factor(z$Category, levels=c('National policies', 'Carbon budget 1000', 'Carbon budget 400'))
ggplot(data=filter(z, period==2030)) + geom_bar(aes(x=variable, y=value, fill=Category), stat="identity", position="dodge") +
     theme_bw() +
     guides(fill = guide_legend(reverse = TRUE)) +
     xlab("EJ") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))
# --> explanation, IMAGE has same absolute amount of renewable enery by 2030 in 2C and 1.5C scenario, but more efficiency improvement in 1.5C, 
# resulting in a lower share of renewale final energy