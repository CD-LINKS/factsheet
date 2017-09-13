# Collection of fixes for reporting issues and other adjustments --------------------------------
#

# Remove GEM-E3_V1 as newest results are uploaded under GEM-E3
all<-all[!model=="GEM-E3_V1"]

#RU-TIMES: use PPP values for MER comparisons
tmp <- all[model=="RU-TIMES 3.2"&variable=="GDP|PPP"&region=="RUS"]
tmp$variable <- "GDP|MER"
all <- rbind(all,tmp)

#RU-TIMES: problem with Energy and Industrial Processes (probably only industrial processes??)
all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy and Industrial Processes"&region=="RUS"]$value <-
  all[model=="RU-TIMES 3.2"&variable=="Emissions|CO2|Energy"&region=="RUS"]$value

#IPAC/AIM:
# Baseline not NoPOL but NPi, because IPAC does not have NoPOL
#all[model =="IPAC-AIM/technology V1.0"]$Baseline <- ""
all[model =="China TIMES"]$Baseline <- ""

# Adding geothermal to models that don't report it, needed for calculation of total renewable energy share
tmp1<-all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity"]$model),unique(all[variable=="Secondary Energy|Electricity|Geothermal"]$model))
          &variable=="Secondary Energy|Electricity"]
tmp1$variable<-"Secondary Energy|Electricity|Geothermal"
tmp1$value<-0
all<-rbind(all,tmp1)

# Adding R5 regions to get World total for DNE21+
# if("World"%in%cfg$regions){
#   tmp1<-all[model=="DNE21+ V.14"&region%in%c("R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")]
# tmp=spread(tmp1,region, value)
# tmp=na.omit(tmp)
# tmp=tmp %>% mutate(World=R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF)
# tmp1=gather(tmp, region, value, c(World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
# tmp1=data.table(tmp1)
# tmp1=tmp1[region=="World"&!variable=="Price|Carbon"]
# all <- rbind(all,tmp1)}

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
if(dim(tmp1)[1]!=0 & "Emissions|CO2|Energy" %in% unique(tmp1$variable)){
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

## GEM-E3 & DNE21+: using average of other models for missing emission sources/sectors (FIXME: to generalise-see start in lines commented out)
#tmp1 <- all[,list(setdiff(unique(vars$variable),unique(all$variable))),by=c("model")] # Search for missing variables per model-intersect?

# tmp1 <- all[,list(unique(variable)),by=c("model")]
# tmp1=data.table(tmp1)
# tmp1=tmp1[model%in%c("DNE21+ V.14","GEM-E3")]
# if(c("Emissions|N2O|Energy","Emissions|CH4|Energy|Supply","Emissions|CH4|Energy|Demand|Industry",
#      "Emissions|CH4|Energy|Demand|Residential and Commercial","Emissions|CH4|Energy|Demand|Transportation",
#      "Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")!%in% unique(tmp1$V1)){}

tmp=all[variable%in%c("Emissions|N2O|Energy",
                      "Emissions|CH4|Energy|Supply", 
                      "Emissions|CH4|Energy|Demand|Industry","Emissions|CH4|Energy|Demand|Residential and Commercial","Emissions|CH4|Energy|Demand|Transportation",
                      "Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpG=tmp
tmpD=tmp[!(variable=="Emissions|CO2|AFOLU" & region=="World")]
tmpG$model<-"GEM-E3"
tmpD$model<-"DNE21+ V.14"
scenarios=all[model%in%c("GEM-E3","DNE21+ V.14"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category")]
tmpG=merge(tmpG,scenarios,by=c("Category"))
tmpD=merge(tmpD,scenarios,by=c("Category"))
setcolorder(tmpG,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpD,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("GEM-E3","DNE21+ V.14"),list(region=unique(region)),by=c("model")]
scenarios=all[model%in%c("GEM-E3","DNE21+ V.14"),list(scenario=unique(scenario)),by=c("model")]
tmpG=tmpG[region%in%regions[model=="GEM-E3"]$region & scenario%in%scenarios[model=="GEM-E3"]$scenario]
tmpD=tmpD[region%in%regions[model=="DNE21+ V.14"]$region & scenario%in%scenarios[model=="DNE21+ V.14"]$scenario]
all<-rbind(all,tmpG,tmpD)

# Add AFOLU to total Kyoto Gases/CO2/CH4/N2O for DNE & GEM-E3 - TODO? Do all at once (clean up)
# tmp=all[model%in%c("GEM-E3","DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")]
# tmp[variable=="Emissions|CH4|AFOLU"]$value<-tmp[variable=="Emissions|CH4|AFOLU"]$value*25
# tmp[variable=="Emissions|CH4|AFOLU"]$unit<-"Mt CO2-equiv/yr"
# tmp[variable=="Emissions|N2O|AFOLU"]$value<-tmp[variable=="Emissions|N2O|AFOLU"]$value*298/1000
# tmp[variable=="Emissions|N2O|AFOLU"]$unit<-"Mt CO2-equiv/yr"
# tmp[variable=="Emissions|CO2|AFOLU"]$unit<-"Mt CO2-equiv/yr"
# tmp=spread(tmp,variable,value)
# tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|Kyoto Gases`+`Emissions|CO2|AFOLU`+`Emissions|N2O|AFOLU`+`Emissions|CH4|AFOLU`)
# tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`,`Emissions|CO2|AFOLU`,`Emissions|N2O|AFOLU`,`Emissions|CH4|AFOLU`))
# tmp=data.table(tmp)
# tmp=tmp[variable=="Emissions|Kyoto Gases"]
# setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
# all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("GEM-E3","DNE21+ V.14"))]
# all<-rbind(all,tmp)

# tmp=all[model%in%c("GEM-E3","DNE21+ V.14")&variable%in%c("Emissions|CO2","Emissions|CO2|AFOLU")]
# tmp=spread(tmp,variable,value)
# tmp=tmp%>%mutate(`Emissions|CO2`=`Emissions|CO2`+`Emissions|CO2|AFOLU`)
# tmp=gather(tmp,variable,value,c(`Emissions|CO2`,`Emissions|CO2|AFOLU`))
# tmp=data.table(tmp)
# tmp=tmp[variable=="Emissions|CO2"]
# setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
# all=all[!(variable=="Emissions|CO2" & model%in%c("GEM-E3","DNE21+ V.14"))]
# all<-rbind(all,tmp)

# tmp=all[model%in%c("GEM-E3","DNE21+ V.14")&variable%in%c("Emissions|CH4","Emissions|CH4|AFOLU")]
# tmp=spread(tmp,variable,value)
# tmp=tmp%>%mutate(`Emissions|CH4`=`Emissions|CH4`+`Emissions|CH4|AFOLU`)
# tmp=gather(tmp,variable,value,c(`Emissions|CH4`,`Emissions|CH4|AFOLU`))
# tmp=data.table(tmp)
# tmp=tmp[variable=="Emissions|CH4"]
# setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
# all=all[!(variable=="Emissions|CH4" & model%in%c("GEM-E3","DNE21+ V.14"))]
# all<-rbind(all,tmp)

# tmp=all[model%in%c("GEM-E3","DNE21+ V.14")&variable%in%c("Emissions|N2O","Emissions|N2O|AFOLU")]
# tmp=spread(tmp,variable,value)
# tmp=tmp%>%mutate(`Emissions|N2O`=`Emissions|N2O`+`Emissions|N2O|AFOLU`)
# tmp=gather(tmp,variable,value,c(`Emissions|N2O`,`Emissions|N2O|AFOLU`))
# tmp=data.table(tmp)
# tmp=tmp[variable=="Emissions|N2O"]
# setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
# all=all[!(variable=="Emissions|N2O" & model%in%c("GEM-E3","DNE21+ V.14"))]
# all<-rbind(all,tmp)

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



