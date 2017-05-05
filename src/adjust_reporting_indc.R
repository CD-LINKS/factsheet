# Collection of fixes for reporting issues and other adjustments --------------------------------
#
#RU-TIMES: use PPP values for MER comparisons
tmp <- all[model=="RU-TIMES 3.2"&variable=="GDP|PPP"&region=="RUS"]
tmp$variable <- "GDP|MER"
all <- rbind(all,tmp)

#RU-TIMES: problem with Energy and Industrial Processes (probably only industrial processes??)
# all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy and Industrial Processes"&region=="RUS"]$value <-
#   all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy"&region=="RUS"]$value

#IPAC/AIM:
# Baseline not NoPOL but NPi, because IPAC does not have NoPOL
all[model =="IPAC-AIM/technology V1.0"]$Baseline <- ""
all[model =="China TIMES"]$Baseline <- ""

#COPPE-MSB: no Emissions|CO2|Energy for INDC
# tmp <-all[model=="COPPE-MSB_v1.3.2" &variable=="Emissions|CO2"&region=="BRA" & scenario =="INDCi",  ]
# tmp$variable <- "Emissions|CO2|Energy"
# all <- rbind(all,tmp)
# all <- all[ !(model %in% c("*COPPE-MSB_v1.3.2", "COPPE-MSB_v1.3.2") & variable =="Emissions|CO2|Energy and Industrial Processes"), ]

# Multiplying Brazil GDP for COPPE by 1000 because reported differently (factor 1000 different from global models)
all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value=all[model=="COPPE-MSB_v1.3.2"&variable=="GDP|MER"&region=="BRA"]$value*1000

# GCAM-USA: no total final energy, only in demand sectors -> adding demand sectors to get total
tmp1 <- all[model %in% setdiff(unique(all[variable=="Final Energy|Transportation"]$model),unique(all[variable=="Final Energy"]$model)) &
              variable %in% c("Final Energy|Transportation","Final Energy|Industry","Final Energy|Residential and Commercial")]
if(dim(tmp1)[1]!=0){
tmp=spread(tmp1,variable, value)
tmp=na.omit(tmp)
tmp=tmp %>% mutate(`Final Energy`=`Final Energy|Transportation` + `Final Energy|Industry` + `Final Energy|Residential and Commercial`)
tmp1=gather(tmp, variable, value, c(`Final Energy`,`Final Energy|Transportation`, `Final Energy|Industry`,`Final Energy|Residential and Commercial`))
tmp1=data.table(tmp1)
tmp1=tmp1[variable=="Final Energy"]
all <- rbind(all,tmp1)} 

## Energy and industrial processes...
#Adding "Emissions|CO2|Energy and Industrial Processes" to scenarios that don't report them, but have "Emissions|CO2|Energy" and "Emissions|CO2|Industrial Processes"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Industrial Processes"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
              variable %in% c("Emissions|CO2|Energy","Emissions|CO2|Industrial Processes")]
if(dim(tmp1)[1]!=0){
tmp=spread(tmp1,variable, value)
tmp=na.omit(tmp)
tmp=tmp %>% mutate(`Emissions|CO2|Energy and Industrial Processes`=`Emissions|CO2|Energy` + `Emissions|CO2|Industrial Processes`)
tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy and Industrial Processes`,`Emissions|CO2|Energy`, `Emissions|CO2|Industrial Processes`))
tmp1=data.table(tmp1)
tmp1=tmp1[variable=="Emissions|CO2|Energy and Industrial Processes"]
all <- rbind(all,tmp1)} 

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2|Energy"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
              variable == "Emissions|CO2|Energy"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2" (GEM-E3)
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2"]$model),unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model)) &
              variable == "Emissions|CO2"]
tmp1$variable <- "Emissions|CO2|Energy and Industrial Processes"
all <- rbind(all,tmp1)

#Adding "Emissions|CO2|Energy" to models that don't report them, but have "Emissions|CO2|Energy and Industrial Processes"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy and Industrial Processes"]$model),unique(all[variable=="Emissions|CO2|Energy"]$model)) &
              variable == "Emissions|CO2|Energy and Industrial Processes"]
tmp1$variable <- "Emissions|CO2|Energy"
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
setdiff(unique(all[variable==as.character(alternatives[i,1])]$model),unique(all[variable==as.character(alternatives[i,2])]$model))
tmp1$variable <- alternatives[i,2]
all <- rbind(all,tmp1)
}

# #special case IPAC-AIM/technology V1.0: has Emissions|CO2|Energy and Industrial Processe for one scenario
# tmp2 <- all[model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2"]
# tmp2$variable <- "Emissions|CO2|Energy and Industrial Processes"
# all <- rbind(all[!(model=="IPAC-AIM/technology V1.0" & variable == "Emissions|CO2|Energy and Industrial Processes")],tmp1,tmp2)

#plausibility check: get rid of negative energy values, write model-scenario-region-variable into file
tmp <- all[unit=="EJ/yr" & value <0 & variable!="Primary Energy|Secondary Energy Trade"]
tmp <- tmp %>% select(model,scenario,region,variable,unit,period,value) %>% arrange(model,scenario,variable)
write.csv(tmp,file="Check_negative_energy_values.csv",row.names=F,quote=F)
all[unit=="EJ/yr" & value <0 & variable!="Primary Energy|Secondary Energy Trade"]$value<- 0

#plausibility check: get rid of excesively high values:
tmp <- all[unit=="EJ/yr" & value >600 & !(variable %in% c("Primary Energy","Secondary Energy","Final Energy"))]
tmp <- tmp %>% select(model,scenario,region,variable,unit,period,value) %>% arrange(model,scenario,variable)
write.csv(tmp,file="Check_toohigh_energy_values.csv",row.names=F,quote=F)
all[unit=="EJ/yr" & value >600 & !(variable %in% c("Primary Energy","Secondary Energy","Final Energy","Primary Energy|Non-Biomass Renewables",
                      "Secondary Energy|Electricity","Secondary Energy|Electricity|Non-Biomass Renewables"))]$value<- 0



