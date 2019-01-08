# Collection of examples for fixing reporting issues and other adjustments
# Change model names if you want to use the same adjustment as in these examples (see 'change model name here')
# Add your own adjustments and comment lines out if you only want to use some of the adjustments - and source this script from main_ by setting b.adjustrep = T

# Add missing variables ---------------------------------------------------
# Energy ------------------------------------------------------------------
# Adding geothermal to models that don't report it, needed for calculation of total renewable energy share
tmp1<-all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity"]$model),unique(all[variable=="Secondary Energy|Electricity|Geothermal"]$model))
          &variable=="Secondary Energy|Electricity"]
tmp1$variable<-"Secondary Energy|Electricity|Geothermal"
tmp1$value<-0
all<-rbind(all,tmp1)

# If no total final energy is reported, but only FE in demand sectors: adding demand sectors to get total
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
  
# Primary/secondary energy with/without CCS --------------------------------------------------------
#Adding "w/o CCS" PE and SE types to models that don't report them, assuming that there is no CCS (check with teams)
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

# CO2 and sub-categories --------------------------------------------------

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

#Adding "Emissions|CO2|Energy and Industrial Processes" to models that don't report them, but have "Emissions|CO2"
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

# CH4 and N2O sub-categories ----------------------------------------------
# Adding emissions sub-categories to models that don't report it, needed for calculation of total waste emissions - copy for other sub-categories
tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|N2O"]$model),unique(all[variable=="Emissions|N2O|Other"]$model))
          &variable=="Emissions|N2O"]
tmp1$variable<-"Emissions|N2O|Other"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|N2O"]$model),unique(all[variable=="Emissions|N2O|Energy"]$model))
          &variable=="Emissions|N2O"]
tmp1$variable<-"Emissions|N2O|Energy"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|N2O"]$model),unique(all[variable=="Emissions|N2O|AFOLU"]$model))
          &variable=="Emissions|N2O"]
tmp1$variable<-"Emissions|N2O|AFOLU"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CH4"]$model),unique(all[variable=="Emissions|CH4|AFOLU"]$model))
          &variable=="Emissions|CH4"]
tmp1$variable<-"Emissions|CH4|AFOLU"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CH4"]$model),unique(all[variable=="Emissions|CH4|Energy|Supply"]$model))
          &variable=="Emissions|CH4"]
tmp1$variable<-"Emissions|CH4|Energy|Supply"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CH4"]$model),unique(all[variable=="Emissions|CH4|Energy|Demand|Industry"]$model))
          &variable=="Emissions|CH4"]
tmp1$variable<-"Emissions|CH4|Energy|Demand|Industry"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CH4"]$model),unique(all[variable=="Emissions|CH4|Energy|Demand|Transportation"]$model))
          &variable=="Emissions|CH4"]
tmp1$variable<-"Emissions|CH4|Energy|Demand|Transportation"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CH4"]$model),unique(all[variable=="Emissions|CH4|Energy|Demand|Residential and Commercial"]$model))
          &variable=="Emissions|CH4"]
tmp1$variable<-"Emissions|CH4|Energy|Demand|Residential and Commercial"
tmp1$value<-0
all<-rbind(all,tmp1)

# set emissions to zero if not available
tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CO2"]$model),unique(all[variable=="Emissions|CO2|Industrial Processes"]$model))
          &variable=="Emissions|CO2"]
tmp1$variable<-"Emissions|CO2|Industrial Processes"
tmp1$value<-0
all<-rbind(all,tmp1)

tmp1<-all[model %in% setdiff(unique(all[variable=="Emissions|CO2"]$model),unique(all[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]$model))
          &variable=="Emissions|CO2"]
tmp1$variable<-"Emissions|CO2|Energy|Demand|Residential and Commercial"
tmp1$value<-0
all<-rbind(all,tmp1)

# Replace missing variables by other models' average  ----------------------

##Search for missing variables per model - only for global models, as national model data are difficult to replace with other models' average
tmp1<-all[Scope=="global",list(variable=unique(variable)),by=c("model","scenario","Category","Baseline","region","Scope")]
vars1=data.table(vars)
vars1$id<-seq(1,length(vars1$variable))
tmp2=tmp1[,list(missing=which(!vars1$variable%in%variable)),by=c("model","scenario","Category","Baseline","region","Scope")]
setnames(tmp2,"missing","id")
tmp2=merge(tmp2,vars1,by="id")
tmp2$id<-NULL

tmp3=all[variable%in%unique(tmp2$variable)]
tmp3=tmp3[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmp4=merge(tmp2,tmp3,by=c("variable","Category","region","Scope"),allow.cartesian = T)
setcolorder(tmp4,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
all<-rbind(all,tmp4)



# Add bunker emissions as separate region -------------------------------
if("World"%in%cfg$regions){
  tmp1<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable=="Emissions|Kyoto Gases"]
  tmp=spread(tmp1,region, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(Bunkers=World - (R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF))
  tmp1=gather(tmp, region, value, c(Bunkers,World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
  tmp1=data.table(tmp1)
  tmp1=tmp1[region=="Bunkers"]
  setcolorder(tmp1,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
  tmp2=tmp1
  tmp2$variable<-"Emissions|CO2|Energy and Industrial Processes"
  tmp2$unit<-"Mt CO2/yr"
  all <- rbind(all,tmp1,tmp2)}


# Change from SAR GWP to AR4 GWP for Emissions|Kyoto Gases ----------------
# If Kyto Gases are calculated using SAR GWPs, and this should be changed to AR4 GWP
tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")] #change model name here
tmp[variable=="Emissions|CO2"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CH4"]$value<-tmp[variable=="Emissions|CH4"]$value*25
tmp[variable=="Emissions|CH4"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|N2O"]$value<-tmp[variable=="Emissions|N2O"]$value*298/1000
tmp[variable=="Emissions|N2O"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|CO2`+`Emissions|CH4`+`Emissions|N2O`+`Emissions|F-Gases`) #+`Emissions|N2O|AFOLU`+`Emissions|CH4|AFOLU`
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`, `Emissions|CO2`, `Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("DNE21+ V.14"))]  #change model name here
all<-rbind(all,tmp)

# Plausibility checks -----------------------------------------------------
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



