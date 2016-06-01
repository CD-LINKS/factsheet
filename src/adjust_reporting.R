# Collection of fixes for reporting issues and other adjustments --------------------------------
#

# For EU keep data only that has CO2 emissions in 2010 in right range
mods <- unique(all[Year=="2010" & Variable=="Emissions|CO2" & Region=="EU" & value>1000, Model, with=TRUE])
dat2 <- subset(all, Model %in% mods & Region=="EU")
all  <- rbind(subset(all, !Region=="EU"),dat2)

# Multiplying Brazil GDP for COPPE by 1000 because reported differently (factor 1000 different from global models)
all[Model=="COPPE-MSB_v1.3.2"&Variable=="GDP|MER"&Region=="BRA"]$value=all[Model=="COPPE-MSB_v1.3.2"&Variable=="GDP|MER"&Region=="BRA"]$value*1000

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2"
tmp1 <- all[Model %in% setdiff(unique(all[Variable=="Emissions|CO2"]$Model),unique(all[Variable=="Emissions|CO2|Energy and Industrial Processes"]$Model)) &
                Variable == "Emissions|CO2"]
tmp1$Variable <- "Emissions|CO2|Energy and Industrial Processes"
# all <- rbind(all,tmp1)
#special case IPAC-AIM/technology V1.0: has Emissions|CO2|Energy and Industrial Processe for one scenario
tmp2 <- all[Model=="IPAC-AIM/technology V1.0" & Variable == "Emissions|CO2"]
tmp2$Variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all[!(Model=="IPAC-AIM/technology V1.0" & Variable == "Emissions|CO2|Energy and Industrial Processes")],tmp1,tmp2)

#For IPAC-AIM/technology V1.0, assign PE|Coal|Total to PE|Coal|w/o CCS
tmp1 <- all[Model =="IPAC-AIM/technology V1.0" & Variable == "Primary Energy|Coal"]
tmp1$Variable <- "Primary Energy|Coal|w/o CCS"
tmp2 <- all[Model =="IPAC-AIM/technology V1.0" & Variable == "Primary Energy|Gas"]
tmp2$Variable <- "Primary Energy|Gas|w/o CCS"
tmp3 <- all[Model =="IPAC-AIM/technology V1.0" & Variable == "Primary Energy|Oil"]
tmp3$Variable <- "Primary Energy|Oil|w/o CCS"
tmp4 <- all[Model =="IPAC-AIM/technology V1.0" & Variable == "Secondary Energy|Electricity|Coal"]
tmp4$Variable <- "Secondary Energy|Electricity|Coal|w/o CCS"
tmp5 <- all[Model =="IPAC-AIM/technology V1.0" & Variable == "Secondary Energy|Electricity|Gas"]
tmp5$Variable <- "Secondary Energy|Electricity|Gas|w/o CCS"
all <- rbind(all[!(Model=="IPAC-AIM/technology V1.0" &
                       (Variable=="Primary Energy|Coal|w/o CCS" | Variable=="Primary Energy|Gas|w/o CCS" |
                            Variable=="Primary Energy|Oil|w/o CCS"|Variable=="Secondary Energy|Electricity|Coal|w/o CCS"|
                            Variable=="Secondary Energy|Electricity|Gas|w/o CCS"))],tmp1,tmp2,tmp3,tmp4,tmp5)

#Get rid of 2025 data for IPAC-AIM/technology V1.0, only existing in few variables and scenarios
all <- all[!(Model=="IPAC-AIM/technology V1.0" & Year == 2025)]


