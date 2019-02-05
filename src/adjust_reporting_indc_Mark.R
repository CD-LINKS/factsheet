# Collection of fixes for reporting issues and other adjustments --------------------------------
#

# Model-specific issues ---------------------------------------------------

# Remove GEM-E3_V1 as newest results are uploaded under GEM-E3
all<-all[!model=="GEM-E3_V1"]

#Relabel COFFEE's baseline to NoPolicy, in line with other global models
all[model=="COPPE-COFFEE 1.0"]$scenario=str_replace_all(all[model=="COPPE-COFFEE 1.0"]$scenario,"NoPOL_V4","NoPolicy_V4")

#Relabel COPPE-MSB's baseline categorization to NoPOL
all[model=="COPPE-MSB_v2.0"]$Baseline=str_replace_all(all[model=="COPPE-MSB_v2.0"]$Baseline,"NoPolicy_V4","NoPOL_V4")

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

#India MARKAL: multiply emissions|CO2|Energy|Demand / Supply and Energy&Industrial Process by 1000 (reporting error)
# all[model=="India MARKAL"&
#       variable%in%c("Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy and Industrial Processes")]$value=
#   all[model=="India MARKAL"&
#         variable%in%c("Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy and Industrial Processes")]$value*1000

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

# MESSAGE and COPPE adjust regions 
all[Category=="NoPOL"]$Baseline <- str_replace_na(all[Category=="NoPOL"]$Baseline,"-")
tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.0","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                                     "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                                     "Emissions|CO2|Energy|Demand|Transportation",
                                                                                     "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                                     "Emissions|F-Gases",
                                                                                     "Emissions|CO2|Energy|Demand|Industry",
                                                                                     "Emissions|CO2|Industrial Processes") & region%in%c("EU")]
tmp1 <- tmp1[!c(model=="COPPE-COFFEE 1.0"&variable%in%c("Emissions|F-Gases","Emissions|CO2|Energy|Demand|Residential and Commercial"))]
tmp2 <- all[model=="IMAGE 3.0" & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                 "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                 "Emissions|CO2|Energy|Demand|Transportation",
                                                 "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                 "Emissions|F-Gases",
                                                 "Emissions|CO2|Energy|Demand|Industry") #,
                                                 #"Emissions|CO2|Industrial Processes") & 
                                                 & region=="TUR"]
tmp3 <- tmp2[!c(variable%in%c("Emissions|F-Gases","Emissions|CO2|Energy|Demand|Residential and Commercial"))]
tmp2$model<-"MESSAGEix-GLOBIOM_1.0"
tmp3$model<-"COPPE-COFFEE 1.0"
tmp1 <- rbind(tmp1,tmp2,tmp3)
#tmp1[Category=="NoPOL"]$Baseline <- str_replace_na(tmp1[Category=="NoPOL"]$Baseline,"-")
tmp1=spread(tmp1,region,value)
tmp1 = na.omit(tmp1)
#tmp1[Category=="NoPOL" & Baseline =="-"] <- NA
tmp1 = tmp1%>%mutate(EU=EU-TUR)
tmp1=gather(tmp1,region,value,c(EU,TUR))
tmp1=data.table(tmp1)
tmp1=tmp1[region=="EU"]
all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.0","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                                       "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                                       "Emissions|CO2|Energy|Demand|Transportation",
                                                                                       "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                                       "Emissions|F-Gases",
                                                                                       "Emissions|CO2|Energy|Demand|Industry",
                                                                                       "Emissions|CO2|Industrial Processes") & region%in%c("EU"))]
all<-rbind(all,tmp1)

tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.0","IMAGE 3.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                              "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                              "Emissions|CO2|Energy|Demand|Transportation",
                                                                              "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                              "Emissions|F-Gases",
                                                                              "Emissions|CO2|Energy|Demand|Industry",
                                                                              "Emissions|CO2|Industrial Processes") & region%in%c("USA","CAN")]
tmp1=tmp1[!c(model=="IMAGE 3.0"&region=="USA")]
tmp1$model<-"MESSAGEix-GLOBIOM_1.0"
#tmp1[Category=="NoPOL"]$Baseline <- str_replace_na(tmp1[Category=="NoPOL"]$Baseline,"-")
tmp1=spread(tmp1,region,value)
tmp1 = na.omit(tmp1)
#tmp1[Category=="NoPOL" & Baseline =="-"] <- NA
tmp1 = tmp1%>%mutate(USA=USA-CAN)
tmp1=gather(tmp1,region,value,c(USA,CAN))
tmp1=data.table(tmp1)
tmp1=tmp1[region=="USA"]
all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                    "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                    "Emissions|CO2|Energy|Demand|Transportation",
                                                                    "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                    "Emissions|F-Gases",
                                                                    "Emissions|CO2|Energy|Demand|Industry",
                                                                    "Emissions|CO2|Industrial Processes") & region%in%c("USA"))]
all<-rbind(all,tmp1)
  
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

# With/without CCS --------------------------------------------------------


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


# Replace missing variables by other models' average  ----------------------

##Search for missing variables per model - only for global models, as national model data difficult to replace:
## Not used now, but can be used as general code to replace missing data with other models' average
# tmp1<-all[Scope=="global",list(variable=unique(variable)),by=c("model","scenario","Category","Baseline","region","Scope")] 
# vars1=data.table(vars)
# vars1$id<-seq(1,length(vars1$variable))
# tmp2=tmp1[,list(missing=which(!vars1$variable%in%variable)),by=c("model","scenario","Category","Baseline","region","Scope")]
# setnames(tmp2,"missing","id")
# tmp2=merge(tmp2,vars1,by="id")
# tmp2$id<-NULL
# 
# tmp3=all[variable%in%unique(tmp2$variable)]
# tmp3=tmp3[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
# tmp4=merge(tmp2,tmp3,by=c("variable","Category","region","Scope"),allow.cartesian = T)
# setcolorder(tmp4,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
# all<-rbind(all,tmp4)


# Other model-specific fixes - part 1 ----------------------------------------------


# Several fixes
# 1. Change model name "AIM Enduse 3.0" to "AIM-India [IIMA]
# 2. DNE21+: change from SAR GWP to AR4 GWP for Emissions|Kyoto Gases
# 3. GEM-E3: Add AFOLU CO2 emissions based on average other models
#    DNE21+: Add AFOLU emisisons based on average other models, except for World
# 4. COPPE-COFEE: Add F-gases emissions based on average other models
# 5. POLES: harmonise AFOLU CO2 emissions using FAOSTAT data

# 1. change AIM|Enduse 3.0 to AIM-India[IIMA]
all[model=="AIM/Enduse 3.0","model"] <- "AIM-India [IIMA]"

# 2. DNE21+ Kyto Gases are calculated using SAR GWPs, this should be changed to AR4 GWP
tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")]
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
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("DNE21+ V.14"))]
all<-rbind(all,tmp)

## 4. COPPE-COFFEE: using average of other models for missing emission sources/sectors 
#Quickfix for COPPE-COFFEE:
# First calculate average over other models, then add F-gases to database and alls add to Kyoto Emissions
# CALCULATE AVERAGE
tmp=all[variable%in%c("Emissions|F-Gases")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpC=tmp
tmpC$model<-"COPPE-COFFEE 1.0"
scenarios=all[model%in%c("COPPE-COFFEE 1.0"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category")]
tmpC=merge(tmpC,scenarios,by=c("Category"))
setcolorder(tmpC,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("COPPE-COFFEE 1.0"),list(region=unique(region)),by=c("model")]
scenarios=all[model%in%c("COPPE-COFFEE 1.0"),list(scenario=unique(scenario)),by=c("model")]
tmpC=tmpC[region%in%regions[model=="COPPE-COFFEE 1.0"]$region & scenario%in%scenarios[model=="COPPE-COFFEE 1.0"]$scenario]
all<-rbind(all,tmpC)

# Add F-gases for COPPE-COFFEE
tmp=all[model%in%c("COPPE-COFFEE 1.0")&variable%in%c("Emissions|Kyoto Gases", "Emissions|F-Gases")] 
tmp[variable=="Emissions|F-Gases"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
#tmp=na.omit(tmp)
# Add F-gases to Kyoto Gases
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|Kyoto Gases`+`Emissions|F-Gases`)
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`,`Emissions|F-Gases`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("COPPE-COFFEE 1.0"))]
all<-rbind(all,tmp)

# Add bunker emissions as separate region -------------------------------
# to check if error in data processing can be solved
# GHG emissions
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
# Final Energy
if("World"%in%cfg$regions){
  tmp1<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable=="Final Energy"]
  tmp=spread(tmp1,region, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(Bunkers=World - (R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF))
  tmp1=gather(tmp, region, value, c(Bunkers,World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
  tmp1=data.table(tmp1)
  tmp1=tmp1[region=="Bunkers"]
  setcolorder(tmp1,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
  tmp2=tmp1
  tmp2$variable<-"Final Energy|Transportation"
  tmp2$unit<-"EJ/yr"
  all <- rbind(all,tmp1,tmp2)}

# Other model-specific fixes - part 2 -------------------------------------

## 3. GEM-E3 & DNE21+: using average of other models for missing emission sources/sectors 
#Quickfix for DNE and GEM-E3:
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

# Add AFOLU to total Kyoto Gases/CO2/CH4/N2O for DNE & GEM-E3 - except for DNE-World
tmp=all[model%in%c("GEM-E3")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU")] #,"Emissions|CH4|AFOLU","Emissions|N2O|AFOLU"
# tmp[variable=="Emissions|CH4|AFOLU"]$value<-tmp[variable=="Emissions|CH4|AFOLU"]$value*25
# tmp[variable=="Emissions|CH4|AFOLU"]$unit<-"Mt CO2-equiv/yr"
# tmp[variable=="Emissions|N2O|AFOLU"]$value<-tmp[variable=="Emissions|N2O|AFOLU"]$value*298/1000
# tmp[variable=="Emissions|N2O|AFOLU"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CO2|AFOLU"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|Kyoto Gases`+`Emissions|CO2|AFOLU`) #+`Emissions|N2O|AFOLU`+`Emissions|CH4|AFOLU`
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`,`Emissions|CO2|AFOLU`)) #,`Emissions|N2O|AFOLU`,`Emissions|CH4|AFOLU`
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("GEM-E3"))]
all<-rbind(all,tmp)

tmp=all[model%in%c("GEM-E3")&variable%in%c("Emissions|CO2","Emissions|CO2|AFOLU")] #,"Emissions|CH4","Emissions|CH4|AFOLU","Emissions|N2O","Emissions|N2O|AFOLU"
tmp$unit<-NULL
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|CO2`=`Emissions|CO2`+`Emissions|CO2|AFOLU`
                  # ,`Emissions|CH4`=`Emissions|CH4`+`Emissions|CH4|AFOLU`
                  # ,`Emissions|N2O`=`Emissions|N2O`+`Emissions|N2O|AFOLU`
                 )
tmp=gather(tmp,variable,value,c(`Emissions|CO2`,`Emissions|CO2|AFOLU`)) #,`Emissions|CH4`,`Emissions|CH4|AFOLU`,`Emissions|N2O`,`Emissions|N2O|AFOLU`
tmp=data.table(tmp)
tmp=tmp[variable%in%c("Emissions|CO2")] #,"Emissions|CH4","Emissions|N2O"
tmp$unit<-""
tmp[variable%in%c("Emissions|CO2")]$unit<-"Mt CO2/yr"
# tmp[variable%in%c("Emissions|CH4")]$unit<-"Mt CH4/yr"
# tmp[variable%in%c("Emissions|N2O")]$unit<-"kt N2O/yr"
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable%in%c("Emissions|CO2") & model%in%c("GEM-E3"))] #,"Emissions|CH4","Emissions|N2O"
all<-rbind(all,tmp)

tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")&region!="World"]
tmp[variable=="Emissions|CH4|AFOLU"]$value<-tmp[variable=="Emissions|CH4|AFOLU"]$value*25
tmp[variable=="Emissions|CH4|AFOLU"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|N2O|AFOLU"]$value<-tmp[variable=="Emissions|N2O|AFOLU"]$value*298/1000
tmp[variable=="Emissions|N2O|AFOLU"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CO2|AFOLU"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|Kyoto Gases`+`Emissions|CO2|AFOLU`+`Emissions|N2O|AFOLU`+`Emissions|CH4|AFOLU`)
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`,`Emissions|CO2|AFOLU`,`Emissions|N2O|AFOLU`,`Emissions|CH4|AFOLU`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("DNE21+ V.14")&region!="World")]
all<-rbind(all,tmp)

tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|CO2","Emissions|CO2|AFOLU","Emissions|CH4","Emissions|CH4|AFOLU","Emissions|N2O","Emissions|N2O|AFOLU")&region!="World"]
tmp$unit<-NULL
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|CO2`=`Emissions|CO2`+`Emissions|CO2|AFOLU`,
                 `Emissions|CH4`=`Emissions|CH4`+`Emissions|CH4|AFOLU`,
                 `Emissions|N2O`=`Emissions|N2O`+`Emissions|N2O|AFOLU`)
tmp=gather(tmp,variable,value,c(`Emissions|CO2`,`Emissions|CO2|AFOLU`,`Emissions|CH4`,`Emissions|CH4|AFOLU`,`Emissions|N2O`,`Emissions|N2O|AFOLU`))
tmp=data.table(tmp)
tmp=tmp[variable%in%c("Emissions|CO2","Emissions|CH4","Emissions|N2O")]
tmp$unit<-""
tmp[variable%in%c("Emissions|CO2")]$unit<-"Mt CO2/yr"
tmp[variable%in%c("Emissions|CH4")]$unit<-"Mt CH4/yr"
tmp[variable%in%c("Emissions|N2O")]$unit<-"kt N2O/yr"
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable%in%c("Emissions|CO2","Emissions|CH4","Emissions|N2O") & model%in%c("DNE21+ V.14")&region!="World")]
all<-rbind(all,tmp)


# National models add global model average for selected variables  --------
#Quickfix: CO2 industrial processes for China TIMES, AIM India, MARKAL India
tmp=all[variable%in%c("Emissions|CO2|Industrial Processes")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpC=tmp
tmpA=tmp
tmpM=tmp
tmpC$model<-"China TIMES"
tmpA$model<-"AIM-India [IIMA]"
tmpM$model<-"India MARKAL"
tmpC$Scope<-"national"
tmpA$Scope<-"national"
tmpM$Scope<-"national"
scenarios=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category","model")]
tmpC=merge(tmpC,scenarios[model=="China TIMES"],by=c("Category","model"))
tmpA=merge(tmpA,scenarios[model=="AIM-India [IIMA]"],by=c("Category","model"))
tmpM=merge(tmpM,scenarios[model=="India MARKAL"],by=c("Category","model"))
setcolorder(tmpC,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpA,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpM,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL"),list(region=unique(region)),by=c("model")]
years=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL"),list(period=unique(period)),by=c("model")]
tmpC=tmpC[region%in%regions[model=="China TIMES"]$region & period%in%years[model=="China TIMES"]$period]
tmpA=tmpA[region%in%regions[model=="AIM-India [IIMA]"]$region& period%in%years[model=="AIM-India [IIMA]"]$period]  
tmpM=tmpM[region%in%regions[model=="India MARKAL"]$region& period%in%years[model=="India MARKAL"]$period] 
all<-all[!(model=="India MARKAL"&variable=="Emissions|CO2|Industrial Processes")]
all<-rbind(all,tmpC,tmpA,tmpM)

# Quick fix: F-gases for COPPE MSB
tmp=all[variable%in%c("Emissions|F-Gases")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpC=tmp
tmpC$model<-"COPPE-MSB_v2.0"
tmpC$Scope<-"global"
scenarios=all[model%in%c("COPPE-MSB_v2.0"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category","model")]
tmpC=merge(tmpC,scenarios[model=="COPPE-MSB_v2.0"],by=c("Category","model"))
setcolorder(tmpC,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("COPPE-MSB_v2.0"),list(region=unique(region)),by=c("model")]
years=all[model%in%c("COPPE-MSB_v2.0"),list(period=unique(period)),by=c("model")]
tmpC=tmpC[region%in%regions[model=="COPPE-MSB_v2.0"]$region & period%in%years[model=="COPPE-MSB_v2.0"]$period]
all<-all[!(model=="COPPE-MSB_v2.0"&variable=="Emissions|F-Gases")]
all<-rbind(all,tmpC)

#Quickfix: non-CO2 emissions for all national models except JPN,BRA,USA
tmp=all[variable%in%c("Emissions|CH4","Emissions|N2O","Emissions|F-Gases")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpC=tmp
tmpA=tmp
tmpM=tmp
tmpI=tmp
tmpP=tmp
tmpR=tmp
tmpC$model<-"China TIMES"
tmpA$model<-"AIM-India [IIMA]"
tmpM$model<-"India MARKAL"
tmpI$model<-"IPAC-AIM/technology V1.0"
tmpP$model<-"PRIMES_V1"
tmpR$model<-"RU-TIMES 3.2"
tmpC$Scope<-"global"
tmpA$Scope<-"global"
tmpM$Scope<-"global"
tmpI$Scope<-"global"
tmpP$Scope<-"global"
tmpR$Scope<-"global"
scenarios=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL","IPAC-AIM/technology V1.0","PRIMES_V1","RU-TIMES 3.2"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category","model")]
tmpC=merge(tmpC,scenarios[model=="China TIMES"],by=c("Category","model"))
tmpA=merge(tmpA,scenarios[model=="AIM-India [IIMA]"],by=c("Category","model"))
tmpM=merge(tmpM,scenarios[model=="India MARKAL"],by=c("Category","model"))
tmpI=merge(tmpI,scenarios[model=="IPAC-AIM/technology V1.0"],by=c("Category","model"))
tmpP=merge(tmpP,scenarios[model=="PRIMES_V1"],by=c("Category","model"))
tmpR=merge(tmpR,scenarios[model=="RU-TIMES 3.2"],by=c("Category","model"))
setcolorder(tmpC,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpA,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpM,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpI,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpP,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
setcolorder(tmpR,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL","IPAC-AIM/technology V1.0","PRIMES_V1","RU-TIMES 3.2"),list(region=unique(region)),by=c("model")]
years=all[model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL","IPAC-AIM/technology V1.0","PRIMES_V1","RU-TIMES 3.2"),list(period=unique(period)),by=c("model")]
tmpC=tmpC[region%in%regions[model=="China TIMES"]$region & period%in%years[model=="China TIMES"]$period]
tmpA=tmpA[region%in%regions[model=="AIM-India [IIMA]"]$region& period%in%years[model=="AIM-India [IIMA]"]$period]  
tmpM=tmpM[region%in%regions[model=="India MARKAL"]$region& period%in%years[model=="India MARKAL"]$period] 
tmpI=tmpI[region%in%regions[model=="IPAC-AIM/technology V1.0"]$region& period%in%years[model=="IPAC-AIM/technology V1.0"]$period] 
tmpP=tmpP[region%in%regions[model=="PRIMES_V1"]$region& period%in%years[model=="PRIMES_V1"]$period]
tmpR=tmpR[region%in%regions[model=="RU-TIMES 3.2"]$region& period%in%years[model=="RU-TIMES 3.2"]$period]
tmpI=tmpI[!c(scenario%in%c("NoPOL_V4","NPi2020_low_V4","INDC2030_low_V4","INDC2030_high_V4","INDC_V4")&variable%in%c("Emissions|CH4","Emissions|N2O"))]
all<-all[!c(model=="IPAC-AIM/technology V1.0"&variable%in%c("Emissions|CH4","Emissions|N2O")&scenario%in%c("NPi_V4","NPi2020_high_V4"))]
all<-rbind(all,tmpC,tmpA,tmpM,tmpI,tmpP,tmpR)

#AFOLU CO2 for RU-TIMES - also to total CO2
tmp=all[variable%in%c("Emissions|CO2|AFOLU")]
tmp=tmp[,list(value=mean(value)),by=c("Category","region","variable","unit","period","Scope")]
tmpC=tmp
tmpC$model<-"RU-TIMES 3.2"
tmpC$Scope<-"global"
scenarios=all[model%in%c("RU-TIMES 3.2"),list(scenario=unique(scenario),Baseline=unique(Baseline)),by=c("Category","model")]
tmpC=merge(tmpC,scenarios[model=="RU-TIMES 3.2"],by=c("Category","model"))
setcolorder(tmpC,c("scenario","Category","Baseline","model","region","variable","unit","period","value","Scope"))
regions=all[model%in%c("RU-TIMES 3.2"),list(region=unique(region)),by=c("model")]
years=all[model%in%c("RU-TIMES 3.2"),list(period=unique(period)),by=c("model")]
tmpC=tmpC[region%in%regions[model=="RU-TIMES 3.2"]$region & period%in%years[model=="RU-TIMES 3.2"]$period]
all<-all[!(model=="RU-TIMES 3.2"&variable=="Emissions|CO2|AFOLU")]
all<-rbind(all,tmpC)

tmp=all[model%in%c("RU-TIMES 3.2")&variable%in%c("Emissions|CO2","Emissions|CO2|AFOLU")] 
tmp$unit<-NULL
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|CO2`=`Emissions|CO2`+`Emissions|CO2|AFOLU`)
tmp=gather(tmp,variable,value,c(`Emissions|CO2`,`Emissions|CO2|AFOLU`))
tmp=data.table(tmp)
tmp=tmp[variable%in%c("Emissions|CO2")]
tmp$unit<-""
tmp[variable%in%c("Emissions|CO2")]$unit<-"Mt CO2/yr"
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable%in%c("Emissions|CO2") & model%in%c("RU-TIMES 3.2"))] 
all<-rbind(all,tmp)

# Add Kyoto Gases for IIM, IPAC, China TIMES, Markal, RU-TIMES
tmp=all[model%in%c("India MARKAL","RU-TIMES 3.2","IPAC-AIM/technology V1.0","China TIMES","AIM-India [IIMA]")&variable%in%c("Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")]
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
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("India MARKAL"))]
all<-rbind(all,tmp)

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

# set to zero if not available, will need to change to replace by average other models for COPPE/WITCH
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
# Other model-specific fixes - part 3 -------------------------------------

## 5. adjust POLES AFOLU CO2 emissions, becasue they have different accounting method
## harmonisation based on FAOSTAT and offset (diff POLES and FAOSTAT) is added to Emissions|Kyoto and Emissions|CO2|AFOLU
## on global level (World) and for individual countries for which data is available
##rm(tmp1); rm(tmp2); rm(tmp3); rm(tmp)
data_POLES_AFOLU <- data.table(read.csv('data/POLES AFOLU emissions.csv', sep=";"))
colnames(data_POLES_AFOLU)[1] <- 'region'
data_POLES_AFOLU <- data_POLES_AFOLU[, diff:= FAOSTAT - POLES]
all_original <- all
tmp1 <- all_original[variable%in%c("Emissions|Kyoto Gases") & model%in%c("POLES CDL") & region%in%data_POLES_AFOLU$region]
tmp2 <- all_original[variable%in%c("Emissions|CO2|AFOLU") & model%in%c("POLES CDL") & region%in%data_POLES_AFOLU$region]
tmp3 <- all_original[variable%in%c("Emissions|CO2") & model%in%c("POLES CDL") & region%in%data_POLES_AFOLU$region]
# Kyoto gases
setkey(tmp1, region)
setkey(data_POLES_AFOLU, region)
tmp1 <-merge(tmp1, data_POLES_AFOLU)
tmp1=spread(tmp1,variable,value)
tmp1=tmp1%>%mutate(`Emissions|Kyoto Gases`=`Emissions|Kyoto Gases`+ `diff`)

## AFOLU CO2 emissions
setkey(tmp2, region)
setkey(data_POLES_AFOLU, region)
tmp2 <-merge(tmp2, data_POLES_AFOLU)
tmp2=spread(tmp2,variable,value)
tmp2=tmp2%>%mutate(`Emissions|CO2|AFOLU`=`Emissions|CO2|AFOLU`+ `diff`)
# CO2 emissions
setkey(tmp3, region)
setkey(data_POLES_AFOLU, region)
tmp3 <-merge(tmp3, data_POLES_AFOLU)
tmp3=spread(tmp3,variable,value)
tmp3=tmp3%>%mutate(`Emissions|CO2`=`Emissions|CO2`+ `diff`)

tmp1 <- data.table(tmp1); tmp2 <- data.table(tmp2); tmp3 <- data.table(tmp3);
tmp1 <- tmp1[ ,`:=`(POLES = NULL, FAOSTAT = NULL, diff = NULL)]
tmp2 <- tmp2[ ,`:=`(POLES = NULL, FAOSTAT = NULL, diff = NULL)]
tmp3 <- tmp3[ ,`:=`(POLES = NULL, FAOSTAT = NULL, diff = NULL)]
tmp1$variable <- "Emissions|Kyoto Gases"
tmp2$variable <- "Emissions|CO2|AFOLU"
tmp3$variable <- "Emissions|CO2"

setnames(tmp1, "Emissions|Kyoto Gases", "value")
setnames(tmp2, "Emissions|CO2|AFOLU", "value")
setnames(tmp3, "Emissions|CO2", "value")
tmp <- rbind(tmp1,tmp2,tmp3)
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all_original[!(variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU","Emissions|CO2") & model%in%c("POLES CDL"))]
all<-rbind(all,tmp)

# For figure 2 Nature Communciations paper
# add variables 'Secondary Energy|Electricity|Coal|w/ CCS' and 'Secondary Energy|Electricity|Gas|w/ CCS' with zero value for AIM/CGE
tmp_var <- c('Secondary Energy|Electricity|Coal|w/ CCS', 'Secondary Energy|Electricity|Gas|w/ CCS')
tmp <- filter(all, variable %in% tmp_var, model=="IMAGE 3.0")
tmp$model <- "AIM/CGE"
tmp$value <- 0
#setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
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



