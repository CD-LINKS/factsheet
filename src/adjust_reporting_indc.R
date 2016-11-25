# Collection of fixes for reporting issues and other adjustments --------------------------------
#
#RU-TIMES: use PPP values for MER comparisons
tmp <- all[model=="RU-TIMES 3.2"&variable=="GDP|PPP"&region=="RUS"]
tmp$variable <- "GDP|MER"

#RU-TIMES: problem with Energy and Industrial Processes (probably only industrial processes??)
all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy and Industrial Processes"&region=="RUS"]$value <-
  all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy"&region=="RUS"]$value

all <- rbind(all,tmp)
# Multiplying Brazil GDP for COPPE by 1000 because reported differently (factor 1000 different from global models)
all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value=all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value*1000

#Adding "Emissions|CO2|Energy and Industrial Processes" to scenarios that don't report them, but have "Emissions|CO2|Energy"
tmp1 <- all[scenario %in% setdiff(unique(all[variable=="Emissions|CO2|Energy"]$scenario),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$scenario)) &
              variable == "Emissions|CO2|Energy"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2|Energy"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
              variable == "Emissions|CO2|Energy"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
                variable == "Emissions|CO2"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2" to models that don't report them, but have "Emissions|CO2|Energy and Industrial Processes"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model),unique(all[variable=="Emissions|CO2"]$model)) &
              variable == "Emissions|CO2|Energy and Industrial Processes"]
tmp1$variable <- "Emissions|CO2"
all <- rbind(all,tmp1)

#Adding "w/o CCS" PE and SE types to models that don't report them, assuming that there is no CCS (which is probably not true -> ask teams to submit)
alternatives <- data.frame(plot_var=c("Primary Energy|Biomass",
                                      "Primary Energy|Coal",
                                      "Primary Energy|Gas",
                                      "Primary Energy|Oil",
                                      "Secondary Energy|Electricity|Biomass",
                                      "Secondary Energy|Electricity|Coal",
                                      "Secondary Energy|Electricity|Gas",
                                      "Secondary Energy|Electricity|Oil"), 
                           alt_var=c("Primary Energy|Biomass|w/o CCS",
                                     "Primary Energy|Coal|w/o CCS",
                                     "Primary Energy|Gas|w/o CCS",
                                     "Primary Energy|Oil|w/o CCS",
                                     "Secondary Energy|Electricity|Biomass|w/o CCS",
                                     "Secondary Energy|Electricity|Coal|w/o CCS",
                                     "Secondary Energy|Electricity|Gas|w/o CCS",
                                     "Secondary Energy|Electricity|Oil|w/o CCS"
                                     ))
for (i in (1:dim(alternatives)[1])){
tmp1 <- all[model %in% setdiff(unique(all[variable==as.character(alternatives[i,1])]$model),unique(all[variable==as.character(alternatives[i,2])]$model)) &
              variable == as.character(alternatives[i,1])]
tmp1$variable <- alternatives[i,2]
all <- rbind(all,tmp1)
}

# #special case IPAC-AIM/technology V1.0: has Emissions|CO2|Energy and Industrial Processe for one scenario
# tmp2 <- all[model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2"]
# tmp2$variable <- "Emissions|CO2|Energy and Industrial Processes"
# all <- rbind(all[!(model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2|Energy and Industrial Processes")],tmp1,tmp2)




