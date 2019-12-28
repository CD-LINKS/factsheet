# Collection of fixes for reporting issues and other adjustments --------------------------------
#

# For EU keep data only that has CO2 emissions in 2010 in right range
mods <- unique(all[period=="2010" & variable=="Emissions|CO2" & region=="EU" & value>1000, model, with=TRUE])
dat2 <- subset(all, model %in% mods & region=="EU")
all  <- rbind(subset(all, !region=="EU"),dat2)

# Multiplying Brazil GDP for COPPE by 1000 because reported differently (factor 1000 different from global models)
all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value=all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value*1000

#Adding "Emissions|CO2|Energy and Industrial Processes" to scenarios that don't report them, but have "Emissions|CO2|Energy"
tmp1 <- all[scenario %in% setdiff(unique(all[variable=="Emissions|CO2"]$scenario),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$scenario)) &
              variable == "Emissions|CO2"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
                variable == "Emissions|CO2"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
# all <- rbind(all,tmp1)
#special case IPAC-AIM/technology V1.0: has Emissions|CO2|Energy and Industrial Processe for one scenario
tmp2 <- all[model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2"]
tmp2$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all[!(model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2|Energy and Industrial Processes")],tmp1,tmp2)

#For IPAC-AIM/technology V1.0, assign PE|Coal|Total to PE|Coal|w/o CCS
tmp1 <- all[model =="IPAC-AIM/technology V1.0" & variable == "Primary Energy|Coal"]
tmp1$variable <- "Primary Energy|Coal|w/o CCS"
tmp2 <- all[model =="IPAC-AIM/technology V1.0" & variable == "Primary Energy|Gas"]
tmp2$variable <- "Primary Energy|Gas|w/o CCS"
tmp3 <- all[model =="IPAC-AIM/technology V1.0" & variable == "Primary Energy|Oil"]
tmp3$variable <- "Primary Energy|Oil|w/o CCS"
tmp4 <- all[model =="IPAC-AIM/technology V1.0" & variable == "Secondary Energy|Electricity|Coal"]
tmp4$variable <- "Secondary Energy|Electricity|Coal|w/o CCS"
tmp5 <- all[model =="IPAC-AIM/technology V1.0" & variable == "Secondary Energy|Electricity|Gas"]
tmp5$variable <- "Secondary Energy|Electricity|Gas|w/o CCS"
all <- rbind(all[!(model=="IPAC-AIM/technology V1.0" &
                       (variable=="Primary Energy|Coal|w/o CCS" | variable=="Primary Energy|Gas|w/o CCS" |
                            variable=="Primary Energy|Oil|w/o CCS"|variable=="Secondary Energy|Electricity|Coal|w/o CCS"|
                            variable=="Secondary Energy|Electricity|Gas|w/o CCS"))],tmp1,tmp2,tmp3,tmp4,tmp5)

#Get rid of 2025 data for IPAC-AIM/technology V1.0, only existing in few variables and scenarios
all <- all[!(model=="IPAC-AIM/technology V1.0" & period == 2025)]


