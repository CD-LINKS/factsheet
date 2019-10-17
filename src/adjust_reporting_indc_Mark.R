# function necassary for EU scaling IMAGE model
harm_scale <- function(t)
{ 
  if (t<2010) 
  { 0
  }
  else
  {    
    if (t>=2010 & t <= 2030)
    { 1
    }
    else max(0,(2100-t)/(2100-2030))
  }
}

# Collection of fixes for reporting issues and other adjustments --------------------------------
# Data changes for 'taking stock of climate policies' paper
# I. specific model changes for variable/scenario names

# II. General changes to all models. Add variables to models that do not report them. Either based on other variables, or set them to zero

# III. Add new variables for all models

# IV. model specific adjustemens to data
# IV.1 GCAM-USA: no total final energy, only in demand sectors -> adding demand sectors to get total
# IV.2 WITCH does not report Emissions|CO2|Energy and Emissions|CO2|Energy and Industrial processes 
# IV.3. COPPE Emissions|CO2|Demand|Residential and Commercial are missing, calculate from seperate residential and commercial results
# IV.4. MESSAGE and COPPE adjust regions (EU without Turkey)
# IV.5 MESSAGE  adjust regions (USA without CAN)
# IV.7. DNE21+: change from SAR GWP to AR4 GWP for Emissions|Kyoto Gases
# IV.8 REMIND from AR5 to AR4 and f-gases to Kyoto emissions
# IV.9a COPPE-COFEE: Add F-gases emissions based on average other models
# IV.9b Add GDP and population based on IMAGE
# IV.10 COPPE-COFEE and GEM-E3: Final Energy|Biomass, and Final Energy|Solids|Biomass should include traditional biomass
# IV.11.a Add Secondary Energy|Electricity|Geothermal for AIM V2.1 if this is missing
#       b Add  "Secondary Energy|Electricity|Coal|w/o CCS", "Secondary Energy|Electricity|Gas|w/o CCS","Secondary Energy|Electricity|Oil|w/o CCS" with zero values
# IV.12. Add bunker emissions for COPPE to Emissions|CO2|Energy and Industrial Processes
#        by recalculating Emissions|CO2|Energy and Industrial Processes as sum of Energy and Industrial Processes
# IV.13.     Add bunker emissions for POLES to Emissions|CO2|Energy|Demand|Transportation (which is not included)
#        As it is included in CO2 Emissions|Energy, calculate bunkers from difference
# IV.14.a. GEM-E3 & DNE21+: using average of other models for missing emission sources/sectors 
#       b. Add AFOLU to total Kyoto Gases/CO2/CH4/N2O for DNE & GEM-E3 - except for DNE-World 
# IV.15. adjust POLES AFOLU CO2 emissions, becasue they have different accounting method
# harmonisation based on FAOSTAT and offset (diff POLES and FAOSTAT) is added to Emissions|Kyoto and Emissions|CO2|AFOLU
# on global level (World) and for individual countries for which data is available
# IV.16.  Add variables 'Secondary Energy|Electricity|Coal|w/ CCS' and 'Secondary Energy|Electricity|Gas|w/ CCS' with zero value for AIM/CGE
# IV.17. Scale EU for IMAGE model
# IV.18 add GDP and population to COPPE data
# IV. 19 (Re-) calculate Final Energy|Other sector for COPPE, WITCH and GEM-E3

# V. National models add global model average for selected variables  --------
# V.1 Quickfix: CO2 industrial processes for China TIMES, AIM India, MARKAL India
# V.2 Quick fix: F-gases for COPPE MSB
# V.3 Quickfix: non-CO2 emissions for all national models except JPN,BRA,USA
# V.4 AFOLU CO2 for RU-TIMES - also to total CO2
# V.5. Add Kyoto Gases for IIM, IPAC, China TIMES, Markal, RU-TIMES
# V.6. 20 Add NDC scenario for PRIMES, based on INDC2030_low_V3 (advise from Zoi), only for 2005 to 2030

# VI. Post-process variables, set to zero. For variables that were added in the individual model 
#     changes (IV)
# VI.1 AFOLU emissions for CH4 and N2O
# VI.2. Add bunker emissions and energy as separate region
# VI.3 Adding "Emissions|CO2|Energy|Demand|Residential and Commercial" to models that don't report them, but have "Emissions|CO2|Energy|Demand|Residential"

# VII. interpolate variables that only report 10 years, add 5 year interpolations

# I. specific model changes for variable/scenario names -------------------------------------
# Model-specific issues

all <- as.data.table(all)

# Remove GEM-E3_V1 as newest results are uploaded under GEM-E3
all<-all[model!="GEM-E3_V1"]

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

# do not use AIM/CGE
all<-all[!(model=="AIM/CGE")]

# change AIM|Enduse 3.0 to AIM-India[IIMA]
all[model=="AIM/Enduse 3.0","model"] <- "AIM-India [IIMA]"

# .................................................................................................................................
# .................................................................................................................................

# II. General changes to all models. Add variables to models that do not report them. Either based on other variables, or set them to zero -----------------------------

# Adding geothermal to models that don't report it, needed for calculation of total renewable energy share
tmp1<-all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity"]$model),unique(all[variable=="Secondary Energy|Electricity|Geothermal"]$model))
          &variable=="Secondary Energy|Electricity"]
tmp1$variable<-"Secondary Energy|Electricity|Geothermal"
tmp1$value<-0
all<-rbind(all,tmp1)

# Adding final energy other sector to models that don't report it, needed for calculation of total renewable energy share
# Final Energy|Other Sector|
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Other Sector"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Other Sector"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Other Sector|Electricity
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy|Electricity"]$model),unique(all[variable=="Final Energy|Other Sector|Electricity"]$model))
          &variable=="Final Energy|Electricity"]
tmp1$variable<-"Final Energy|Other Sector|Electricity"
tmp1$value<-0
all<-rbind(all,tmp1)
# Adding final energy biomass variables to models that don't report it, needed for calculation of total renewable energy share
# Final Energy|Residential and Commercial|Biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Residential and Commercial|Solids|Biomass"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Residential and Commercial|Solids|Biomass"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Indudstry|Biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Industry|Solids|Biomass"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Industry|Solids|Biomass"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Other Sector|Solids|Biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Other Sector|Solids|Biomass"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Other Sector|Solids|Biomass"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Solids|Biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Solids|Biomass"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Solids|Biomass"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Non-Energy Use|Biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Non-Energy Use|Biomass"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Non-Energy Use|Biomass"
tmp1$value<-0
all<-rbind(all,tmp1)

# Adding final energy renewable variables to models that don't report it, needed for calculation of total renewable energy share
# Final Energy|Solar
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Solar"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Solar"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Solar
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Wind"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Wind"
tmp1$value<-0
all<-rbind(all,tmp1)
# Final Energy|Solar
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Geothermal"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Geothermal"
tmp1$value<-0
all<-rbind(all,tmp1)

# Adding electricity CCS varialbes to models that don't report it, needed for calculation of total renewable energy share
# Secondary Energy|Electricity|Coal|w/ CCS
tmp1<-all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity"]$model),unique(all[variable=="Secondary Energy|Electricity|Coal|w/ CCS"]$model))
          &variable=="Secondary Energy|Electricity"]
tmp1$variable<-"Secondary Energy|Electricity|Coal|w/ CCS"
tmp1$value<-0
all<-rbind(all,tmp1)
# Secondary Energy|Electricity|Gas|w/ CCS
tmp1<-all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity"]$model),unique(all[variable=="Secondary Energy|Electricity|Gas|w/ CCS"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Secondary Energy|Electricity"
tmp1$value<-0
all<-rbind(all,tmp1)

# traditinoal biomass
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Residential and Commercial|Solids|Biomass|Traditional"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Residential and Commercial|Solids|Biomass|Traditional"
tmp1$value<-0
all<-rbind(all,tmp1)
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Solids|Biomass|Traditional"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Solids|Biomass|Traditional"
tmp1$value<-0
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

#Adding "Secondary Energy|Electricity|Coal|w/ CCS" to scenarios that don't report them, but have "Secondary Energy|Electricity|Coal" and "Secondary Energy|Electricity|Coal|w/o CCS"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Secondary Energy|Electricity|Coal|w/ CCS"]$model),unique(all[variable=="Secondary Energy|Electricity|Coal|w/o CCS"]$model)) &
              variable %in% c("Secondary Energy|Electricity|Coal","Secondary Energy|Electricity|Coal|w/o CCS")]
if(dim(tmp1)[1]!=0 & "Secondary Energy|Electricity|Coal" %in% unique(tmp1$variable)){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Secondary Energy|Electricity|Coal|w/ CCS`=`Secondary Energy|Electricity|Coal` - `Secondary Energy|Electricity|Coal|w/o CCS`)
  tmp1=gather(tmp, variable, value, c(`Secondary Energy|Electricity|Coal|w/ CCS`,`Secondary Energy|Electricity|Coal`, `Secondary Energy|Electricity|Coal|w/o CCS`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Secondary Energy|Electricity|Coal|w/ CCS"]
  all <- rbind(all,tmp1)} 

# .................................................................................................................................
# .................................................................................................................................

# III. Add new variables for all models

# IV. model specific adjustemens to data ---------------------------------

# .................................................................................................................................
# .................................................................................................................................

# IV.1. GCAM-USA: no total final energy, only in demand sectors -> adding demand sectors to get total
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

# IV.2 WITCH does not report Emissions|CO2|Energy and Emissions|CO2|Energy and Industrial processes 
#    --> calculate Emissions|CO2|Energy= Emissions|CO2|Energy|Supply + Emissions|CO2|Energy|Demand|Industry + Emissions|CO2|Energy|Demand|Residential and Commercial
#                                        Emissions|CO2|Energy|Demand|Transportation + Emissions|CO2|Energy|Demand|AFOFI
#                  Emissions|CO2|Industrial processes = Emissions|CO2|Energy and Industrial Processes - Emissions|CO2|Energy
# a. Emissions|CO2|Energy
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy|Supply"]$model),unique(all[variable=="Emissions|CO2|Energy"]$model)) &
              variable %in% c("Emissions|CO2|Energy|Supply", "Emissions|CO2|Energy|Demand|Industry", 
                              "Emissions|CO2|Energy|Demand|Residential and Commercial",
                              "Emissions|CO2|Energy|Demand|Transportation", "Emissions|CO2|Energy|Demand|AFOFI")]
if(dim(tmp1)[1]!=0){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|CO2|Energy`=`Emissions|CO2|Energy|Supply` + 
                       `Emissions|CO2|Energy|Demand|Industry` + 
                       `Emissions|CO2|Energy|Demand|Residential and Commercial` +
                       `Emissions|CO2|Energy|Demand|Transportation`+
                       `Emissions|CO2|Energy|Demand|AFOFI`)
  tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy`,`Emissions|CO2|Energy|Supply`,`Emissions|CO2|Energy|Demand|Industry`,
                                      `Emissions|CO2|Energy|Demand|Residential and Commercial`,
                                      `Emissions|CO2|Energy|Demand|Transportation`,`Emissions|CO2|Energy|Demand|AFOFI`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Emissions|CO2|Energy"]
  all <- rbind(all,tmp1)} 

# b.Emissions|CO2|Industrial processes
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy"]$model),unique(all[variable=="Emissions|CO2|Industrial processes"]$model)) & !model%in%c("China TIMES","AIM-India [IIMA]","India MARKAL")
              &variable %in% c("Emissions|CO2|Energy", "Emissions|CO2|Energy and Industrial Processes")]
if(dim(tmp1)[1]!=0){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|CO2|Industrial processes`= `Emissions|CO2|Energy and Industrial Processes`-`Emissions|CO2|Energy`)
  tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Industrial processes`,`Emissions|CO2|Energy`,`Emissions|CO2|Energy and Industrial Processes`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Emissions|CO2|Industrial processes"]
  all <- rbind(all,tmp1)}

# IV.3. COPPE Emissions|CO2|Demand|Residential and Commercial are missing, calculate from seperate residential and commercial results
# Assume that Emissions|CO2|Demand|Residential and Commercial = Emissinos|CO2|Energy - "Emissions|CO2|Energy|Supply" 
#                                                                                    - "Emissions|CO2|Energy|Demand|Industry",
#                                                                                    - "Emissions|CO2|Energy|Demand|Transportation", 
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Energy"]$model),unique(all[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]$model)) &
              variable %in% c("Emissions|CO2|Energy","Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand|Industry",
                              "Emissions|CO2|Energy|Demand|Transportation")]
if(dim(tmp1)[1]!=0){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|CO2|Energy|Demand|Residential and Commercial`=`Emissions|CO2|Energy` - `Emissions|CO2|Energy|Supply`
                     - `Emissions|CO2|Energy|Demand|Industry`
                     - `Emissions|CO2|Energy|Demand|Transportation`)
  tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy|Demand|Residential and Commercial`,`Emissions|CO2|Energy`,`Emissions|CO2|Energy|Supply`,
                                      `Emissions|CO2|Energy|Demand|Industry`,
                                      `Emissions|CO2|Energy|Demand|Transportation`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
  all <- rbind(all,tmp1)}                                                                                     

# IV.4. MESSAGE and COPPE adjust regions (EU without Turkey)
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
                                                 "Emissions|CO2|Energy|Demand|Industry",
                                                 "Emissions|CO2|Industrial Processes")  
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

# IV.5 MESSAGE  adjust regions (USA without CAN)
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
  

# IV.7. DNE21+: change from SAR GWP to AR4 GWP for Emissions|Kyoto Gases
# IV.9.a. COPPE-COFEE: Add F-gases emissions based on average other models
# IV.9.b. Add GDP and population based on IMAGE
# IV.10 COPPE-COFEE and GEM-E3: Final Energy|Biomass, and Final Energy|Solids|Biomass should include traditional biomass
# IV.11.a Add Secondary Energy|Electricity|Geothermal for AIM V2.1 if this is missing
#       b Add  "Secondary Energy|Electricity|Coal|w/o CCS", "Secondary Energy|Electricity|Gas|w/o CCS","Secondary Energy|Electricity|Oil|w/o CCS" with zero values
# IV.12. Add bunker emissions for COPPE to Emissions|CO2|Energy and Industrial Processes
#        by recalculating Emissions|CO2|Energy and Industrial Processes as sum of Energy and Industrial Processes
# IV.13.     Add bunker emissions for POLES to Emissions|CO2|Energy|Demand|Transportation (which is not included)
#        As it is included in CO2 Emissions|Energy, calculate bunkers from difference
# IV.14.a. GEM-E3 & DNE21+: using average of other models for missing emission sources/sectors 
#       b. Add AFOLU to total Kyoto Gases/CO2/CH4/N2O for DNE & GEM-E3 - except for DNE-World 
# IV.15. adjust POLES AFOLU CO2 emissions, becasue they have different accounting method
# harmonisation based on FAOSTAT and offset (diff POLES and FAOSTAT) is added to Emissions|Kyoto and Emissions|CO2|AFOLU
# on global level (World) and for individual countries for which data is available
# IV.16 Add variables 'Secondary Energy|Electricity|Coal|w/ CCS' and 'Secondary Energy|Electricity|Gas|w/ CCS' with zero value for AIM/CGE
# IV.17. Scale EU for IMAGE model
# IV.18 add GDP and population to COPPE data
# IV. 19 (Re-) calculate Final Energy|Other sector for COPPE, WITCH and GEM-E3

# IV.7a. DNE21+ Kyto Gases are calculated using SAR GWPs, this should be changed to AR4 GWP
tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")&region!="Bunkers"]
tmp[variable=="Emissions|CO2"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CH4"]$value<-tmp[variable=="Emissions|CH4"]$value*25
tmp[variable=="Emissions|CH4"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|N2O"]$value<-tmp[variable=="Emissions|N2O"]$value*298/1000
tmp[variable=="Emissions|N2O"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|CO2`+`Emissions|CH4`+`Emissions|N2O`+`Emissions|F-Gases`)
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`, `Emissions|CO2`, `Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("DNE21+ V.14")&region!="Bunkers")]
all<-rbind(all,tmp)

# IV.8 REMIND Emissions|Kyoto Gases from AR5 to AR4 and add F-gases to Kyoto emissions
tmp=all[model%in%c("REMIND-MAgPIE 1.7-3.0")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")]
tmp[variable=="Emissions|CO2"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CH4"]$value<-tmp[variable=="Emissions|CH4"]$value*25
tmp[variable=="Emissions|CH4"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|N2O"]$value<-tmp[variable=="Emissions|N2O"]$value*298/1000
tmp[variable=="Emissions|N2O"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp,variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|CO2`+`Emissions|CH4`+`Emissions|N2O`+`Emissions|F-Gases`)
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`, `Emissions|CO2`, `Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("REMIND-MAgPIE 1.7-3.0"))]
all<-rbind(all,tmp)

## IV.9. COPPE-COFFEE: using average of other models for missing emission sources/sectors 
# Quickfix for COPPE-COFFEE:
# a. Add F-gases
# First calculate average over other models, then add F-gases to database and alls add to Kyoto Emissions
# CALCULATE AVERAGE fgases
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

# b. add population and GDP(MER) based on IMAGE data
tmp=all[model%in%c("IMAGE 3.0")&variable%in%c("GDP|MER", "Population")] 
tmp$model <- "COPPE-COFFEE 1.0"
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all<-rbind(all,tmp)

# IV.11.a Add Secondary Energy|Electricity|Geothermal for AIM V2.1 if this is missing
#       b Add  "Secondary Energy|Electricity|Coal|w/o CCS", "Secondary Energy|Electricity|Gas|w/o CCS","Secondary Energy|Electricity|Oil|w/o CCS" with zero values
# a)
reg_tmp <- c('BRA', 'CHN', 'IND', 'RUS')
tmp1 <- all[model =="AIM V2.1" &  region %in% reg_tmp & variable == "Final Energy|Geothermal"]
tmp1$variable <- "Secondary Energy|Electricity|Geothermal"
all <- rbind(all,tmp1)

# b) Add (for AIM V2.1 "Secondary Energy|Electricity|Coal|w/o CCS", "Secondary Energy|Electricity|Gas|w/o CCS","Secondary Energy|Electricity|Oil|w/o CCS" with zero values
tmp1<-all[variable=="Secondary Energy|Electricity|Coal|w/o CCS" & model=="AIM V2.1"]
tmp1$variable="Secondary Energy|Electricity|Coal|w/ CCS"
tmp1$value<-0
all <- rbind(all,tmp1)
tmp1<-all[variable=="Secondary Energy|Electricity|Gas|w/o CCS" & model=="AIM V2.1"]
tmp1$variable="Secondary Energy|Electricity|Gas|w/ CCS"
tmp1$value<-0
all <- rbind(all,tmp1)
tmp1<-all[variable=="Secondary Energy|Electricity|Oil|w/o CCS" & model=="AIM V2.1"]
tmp1$variable="Secondary Energy|Electricity|Oil|w/ CCS"
tmp1$value<-0
all <- rbind(all,tmp1)

# IV.12. Add bunker emissions for COPPE to Emissions|CO2|Energy and Industrial Processes
#        by recalculating Emissions|CO2|Energy and Industrial Processes as sum of Energy and Industrial Processes
tmp1 <- all[model %in% c('COPPE-COFFEE 1.0') & region=="World" & 
           variable %in% c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Energy", "Emissions|CO2|Industrial Processes")]
tmp=spread(tmp1,variable, value)
tmp=na.omit(tmp)
tmp=tmp %>% mutate(`Emissions|CO2|Energy and Industrial Processes`=`Emissions|CO2|Energy` + `Emissions|CO2|Industrial Processes`)
tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy and Industrial Processes`,`Emissions|CO2|Energy`, `Emissions|CO2|Industrial Processes`))
tmp1=data.table(tmp1)
tmp1=tmp1[variable=="Emissions|CO2|Energy and Industrial Processes"]
all <- all[!(model %in% c('COPPE-COFFEE 1.0') & region=="World" & variable %in% c("Emissions|CO2|Energy and Industrial Processes"))]
all <- rbind(all,tmp1)

# VI.2. Add bunker emissions and energy as separate region -------------------------------
# to check if error in data processing can be solved
# GHG emissions
if("World"%in%cfg$r){
  # with "Emissions|Kyoto Gases"
  # For DNE, only global total includes AFOLU CO2, regions/countries do not.
  tmp1<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable%in%c("Emissions|Kyoto Gases", "Emissions|CO2|AFOLU") & !(is.na(value))]
  tmp1$unit<-"Mt CO2-equiv/yr"
  tmp=spread(tmp1,variable, value) %>% as.data.table()
  tmp[is.na(tmp$`Emissions|CO2|AFOLU`)]$`Emissions|CO2|AFOLU` <- 0
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|Kyoto Gases`= ifelse(model=="DNE21+ V.14", `Emissions|Kyoto Gases`-`Emissions|CO2|AFOLU`,`Emissions|Kyoto Gases`))
  tmp1=gather(tmp, variable, value, c(`Emissions|Kyoto Gases`, `Emissions|CO2|AFOLU`)) %>% as.data.table()
  tmp1=tmp1[variable%in%c("Emissions|Kyoto Gases")]
  tmp1=data.table(tmp1)
  # calculate bunkers
  #tmp1<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable=="Emissions|Kyoto Gases" & !(is.na(value))]
  tmp=spread(tmp1,region, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(Bunkers=World - (R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF))
  tmp1=gather(tmp, region, value, c(Bunkers,World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
  tmp1=data.table(tmp1)
  tmp1=tmp1[region=="Bunkers"]
  setcolorder(tmp1,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
  
  # with "Emissions|CO2|Energy and Industrial Processes"
  #tmp2=tmp1
  #tmp2$variable<-"Emissions|CO2|Energy and Industrial Processes"
  #tmp2$unit<-"Mt CO2/yr"
  tmp2<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable=="Emissions|CO2|Energy and Industrial Processes"]
  tmp=spread(tmp2,region, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(Bunkers=World - (R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF))
  tmp2=gather(tmp, region, value, c(Bunkers,World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
  tmp2=data.table(tmp2)
  tmp2=tmp2[region=="Bunkers"]
  setcolorder(tmp2,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
  
  # with "Emissions|CO2|Energy"
  #tmp2=tmp1
  #tmp2$variable<-"Emissions|CO2|Energy"
  #tmp2$unit<-"Mt CO2/yr"
  tmp2<-all[region%in%c("World","R5MAF","R5LAM","R5ASIA","R5OECD90+EU","R5REF")&variable=="Emissions|CO2|Energy"]
  tmp=spread(tmp2,region, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(Bunkers=World - (R5MAF + R5LAM + R5ASIA + `R5OECD90+EU`+R5REF))
  tmp2=gather(tmp, region, value, c(Bunkers,World,R5MAF,R5LAM,R5ASIA,`R5OECD90+EU`,R5REF))
  tmp2=data.table(tmp2)
  tmp2=tmp2[region=="Bunkers"]
  setcolorder(tmp2,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
  
  # add to all
  all <- rbind(all,tmp1,tmp2)}


# Final Energy
if("World"%in%cfg$r){
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


## IV.14.a. GEM-E3 & DNE21+: using average of other models for missing emission sources/sectors 
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

# IV.14.b. Add AFOLU to total Kyoto Gases/CO2/CH4/N2O for DNE & GEM-E3 - except for DNE-World
tmp=all[model%in%c("GEM-E3")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU")&region!="Bunkers"] #,"Emissions|CH4|AFOLU","Emissions|N2O|AFOLU"
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
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("GEM-E3")&region!="Bunkers")]
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

tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU","Emissions|CH4|AFOLU","Emissions|N2O|AFOLU")&!(region%in%c("World", "Bunkers"))]
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
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("DNE21+ V.14")&!(region%in%c("World", "Bunkers")))]
all<-rbind(all,tmp)

tmp=all[model%in%c("DNE21+ V.14")&variable%in%c("Emissions|CO2","Emissions|CO2|AFOLU","Emissions|CH4","Emissions|CH4|AFOLU","Emissions|N2O","Emissions|N2O|AFOLU")&!(region%in%c("World", "Bunkers"))]
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
all=all[!(variable%in%c("Emissions|CO2","Emissions|CH4","Emissions|N2O") & model%in%c("DNE21+ V.14")&!(region%in%c("World", "Bunkers")))]
all<-rbind(all,tmp)

## IV.15. adjust POLES AFOLU CO2 emissions, becasue they have different accounting method
## harmonisation based on FAOSTAT and offset (diff POLES and FAOSTAT) is added to Emissions|Kyoto and Emissions|CO2|AFOLU
## on global level (World) and for individual countries for which data is available
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
all=all_original[!(variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU","Emissions|CO2") & model%in%c("POLES CDL") & region%in%data_POLES_AFOLU$region)]
all<-rbind(all,tmp)

# IV.16 Add variables 'Secondary Energy|Electricity|Coal|w/ CCS' and 'Secondary Energy|Electricity|Gas|w/ CCS' with zero value for AIM/CGE
tmp_var <- c('Secondary Energy|Electricity|Coal|w/ CCS', 'Secondary Energy|Electricity|Gas|w/ CCS')
tmp <- filter(all, variable %in% tmp_var, model=="IMAGE 3.0")
tmp$model <- "AIM/CGE"
tmp$value <- 0
#setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all<-rbind(all,tmp)

# IV.17 scale EU for IMAGE model
vars_CO2_Energy <- c("Emissions|CO2|Energy|Supply",
                     "Emissions|CO2|Energy|Demand|Transportation",
                     "Emissions|CO2|Energy|Demand|Residential and Commercial",
                     "Emissions|CO2|Energy|Demand|Industry")
country_Europe <- c("ALB", "AND", "AUT", "BEL", "BIH", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "VAT", "HUN", "ISL", "IRL",
                    "ITA", "LVA", "LIE", "LTU", "LUX", "MLT", "MCO", "NLD", "NOR", "POL", "PRT", "ROU", "SMR", "SVK", "SVN", "ESP", "SWE", "CHE", "MKD", "GBR", "SRB", "MNE")
regions_PRIMAP_EU <- c("EU28", country_Europe)

# read in PRIMAP data
source("functions/HistoricalDataFunctions.R")
PRIMAP_IAM <- read.csv("data/PRIMAP-hist_v1.2_14-Dec-2017.csv", header=TRUE, sep=",",skipNul = T)
#PRIMAP_IAM <- PRIMAP
#years_CDLINKS_history <- c("X1990", "X1995", "X2000", "X2005", "X2010", "X2015")
colnames(PRIMAP_IAM)[colnames(PRIMAP_IAM)=="country"] <- "region"
colnames(PRIMAP_IAM)[colnames(PRIMAP_IAM)=="year"] <- "period"
# filter used variables and regions
PRIMAP_IAM <- filter(PRIMAP_IAM, region %in% regions_PRIMAP_EU)
PRIMAP_IAM$region=str_replace_all(PRIMAP_IAM$region,"EU28","EU")
PRIMAP_Kyoto <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "KYOTOGHGAR4", "Emissions|Kyoto Gases")
PRIMAP_CO2 <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "CO2", "Emissions|CO2")
PRIMAP_CH4 <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "CH4", "Emissions|CH4")
PRIMAP_N2O <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "N2O", "Emissions|N2O")
PRIMAP_CO2_Energy <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT1", "KYOTOGHGAR4", "Emissions|CO2|Energy")
PRIMAP_CO2_industry_process <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT2", "CAT3"), "CO2", "Emissions|CO2|Industrial Processes")
PRIMAP_CO2_Energy_industry_process <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT1", "CAT2", "CAT3"), "CO2", "Emissions|CO2|Energy and Industrial Processes")
PRIMAP_FGases <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT0", "FGASESAR4", "Emissions|F-Gases")
PRIMAP_CO2_AFOLU <- ConvertPRIMAP2IAM(PRIMAP_IAM, c("CAT4", "CAT5"), "CO2", "Emissions|CO2|AFOLU")
PRIMAP_CH4_waste <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT6", "CH4", "Emissions|CH4|Waste")
PRIMAP_N2O_waste <- ConvertPRIMAP2IAM(PRIMAP_IAM, "CAT6", "N2O", "Emissions|N2O|Waste")
PRIMAP_EU <- rbind(PRIMAP_Kyoto, PRIMAP_CO2) %>% rbind(PRIMAP_CH4) %>% rbind(PRIMAP_N2O) %>% rbind(PRIMAP_FGases) %>% 
             rbind(PRIMAP_CO2_Energy) %>% rbind(PRIMAP_CO2_industry_process) %>% rbind(PRIMAP_CO2_Energy_industry_process)   %>%
             rbind(PRIMAP_CO2_AFOLU)  %>% rbind(PRIMAP_CH4_waste) %>% rbind(PRIMAP_N2O_waste) %>%
             filter(period==2010)
source("functions/calcVariable.R")
PRIMAP_EU <- calcVariable(PRIMAP_EU,'`Emissions|Non-CO2` ~ 25*(`Emissions|CH4`)+298*(`Emissions|N2O`)+(`Emissions|F-Gases`)' , newUnit='Mt')
# Create harmonisation factors
PRIMAP_EU_harm_fac <- spread(PRIMAP_EU, region, value) %>%
  replace(is.na(.), 0) %>%
  mutate(harm_fac = ((ALB+AND+AUT+BEL+BIH+BGR+HRV+CYP+CZE+DNK+EST+FIN+FRA+DEU+GRC+VAT+HUN+ISL+IRL+ITA+LVA+LIE+LTU+LUX+MLT+MCO+NLD+NOR+POL+PRT+ROU+SMR+SVK+SVN+ESP+SWE+CHE+MKD+GBR+SRB+MNE)-EU)) %>%
  select(variable, harm_fac)
# downscaling for Energy suppy, transport, buildings, industry as this split up is not available in PRIMAP, based on TIMER shares in CO2|Energy
PRIMAP_EU_harm_fac_CO2_Energy <- filter(PRIMAP_EU_harm_fac, variable=="Emissions|CO2|Energy")$harm_fac %>% as.double()
PRIMAP_EU_harm_fac_CO2_Energy <- filter(all, Category=="National policies", region=="EU", model=="IMAGE 3.0", period==2010, variable%in%vars_CO2_Energy) %>% 
  group_by(Category, model, region, period) %>%
  mutate(share = value/sum(value)) %>%
  mutate(harm_fac = PRIMAP_EU_harm_fac_CO2_Energy*share) %>%
  ungroup() %>%
  select(-share, -scenario, -Category, -model, -region, -Baseline, -period, -unit, -Scope, -value)
PRIMAP_EU_harm_fac <- rbind(PRIMAP_EU_harm_fac_CO2_Energy, PRIMAP_EU_harm_fac)
# add factor for total industry (incl. process emissions)
PRIMAP_EU_harm_fac_industry <- filter(PRIMAP_EU_harm_fac, variable%in%c("Emissions|CO2|Energy|Demand|Industry", "Emissions|CO2|Industrial Processes")) %>%
  summarise(harm_fac = sum(harm_fac)) %>%
  mutate(variable="Emissions|CO2|Industry")
PRIMAP_EU_harm_fac <- rbind(PRIMAP_EU_harm_fac_industry, PRIMAP_EU_harm_fac)
# harmonise EU data
all_tmp_harm <- #subset(all_tmp, variable %in% PRIMAP_EU_harm_fac$variable) %>%
                filter(all, variable %in% PRIMAP_EU_harm_fac$variable, model=="IMAGE 3.0", region=="EU", period>=2010)
all_tmp_other <- filter(all, !(variable %in% PRIMAP_EU_harm_fac$variable & model=="IMAGE 3.0" & region=="EU" & period>=2010))
all_tmp_harm <- left_join(all_tmp_harm, PRIMAP_EU_harm_fac, by="variable") %>%
  mutate(value=value-harm_scale(period)*harm_fac) %>%
  select(-harm_fac)
all <- as.data.frame(all)
all <- rbind(all_tmp_other, all_tmp_harm)
all <- as.data.table(all)

# IV. 18 add GDP and population to COPPE data
gdp_pop_COPPE <- read.table("data/COFFEE_GDP_CDLinks.csv", sep=";", header=TRUE) 
gdp_pop_COPPE <- as.data.frame(gdp_pop_COPPE)
all <- rbind(all, gdp_pop_COPPE)

# IV. 19 (Re-) calculate Final Energy|Other sector for COPPE, WITCH and GEM-E3
tmp1 <- all[model %in% c('COPPE-COFFEE 1.0', 'WITCH2016', 'GEM-E3') & 
           variable %in% c("Final Energy","Final Energy|Residential and Commercial", "Final Energy|Transportation", "Final Energy|Industry")]
tmp=spread(tmp1,variable, value)
tmp=na.omit(tmp)
tmp=tmp %>% mutate(`Final Energy|Other Sector`=`Final Energy` - `Final Energy|Residential and Commercial` - `Final Energy|Transportation` - `Final Energy|Industry`)
tmp1=gather(tmp, variable, value, c(`Final Energy`, `Final Energy|Other Sector`,`Final Energy|Residential and Commercial`, `Final Energy|Transportation`, `Final Energy|Industry`))
tmp1=data.table(tmp1)
tmp1=tmp1[variable=="Final Energy|Other Sector"]
all <- all[!(model %in% c('COPPE-COFFEE 1.0', 'WITCH2016', 'GEM-E3')& variable %in% c("Final Energy|Other Sector"))]
all <- rbind(all,tmp1)

# .................................................................................................................................
# .................................................................................................................................

# V. National models add global model average for selected variables  --------

# V.1 Quickfix: CO2 industrial processes for China TIMES, AIM India, MARKAL India
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

# V.2 Quick fix: F-gases for COPPE MSB
# Make the same as COPPE-COFFEE 1.0 F-Gas emission
tmp=all[variable%in%c("Emissions|F-Gases") & model%in%c("COPPE-COFFEE 1.0")]
tmp$model<-"COPPE-MSB_v2.0"
tmp$Scope<-"national"
all<-all[!(model=="COPPE-MSB_v2.0"&variable=="Emissions|F-Gases")]
all<-rbind(all,tmpC)

# also add to Kyoto Gases
tmp1=all[model%in%c('COPPE-MSB_v2.0') & variable%in%c("Emissions|Kyoto Gases")]
if(nrow(tmp1)==0) { 
  tmp2 <- all[model %in% c('COPPE-MSB_v2.0') & 
              variable %in% c("Emissions|CO2","Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")]
  tmp2$unit <- "x"
  tmp=spread(tmp2,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|Kyoto Gases`=`Emissions|CO2` + 25*`Emissions|CH4` + (298/1000)*`Emissions|N2O` + `Emissions|F-Gases`)
  tmp=gather(tmp, variable, value, c(`Emissions|Kyoto Gases`, `Emissions|CO2`,`Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`))
  tmp=data.table(tmp)
  tmp=tmp[variable=="Emissions|Kyoto Gases"]
  tmp$unit<-"Mt CO2-equiv/yr"
  all <- all[!(model %in% c('COPPE-MSB_v2.0') & variable %in% c("Emissions|Kyoto Gases"))]
  all <- rbind(all,tmp)
}

# V.3 Quickfix: non-CO2 emissions for all national models except JPN,BRA,USA
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

# V.4 AFOLU CO2 for RU-TIMES - also to total CO2
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

# V.5. Add Kyoto Gases for IIM, IPAC, China TIMES, Markal, RU-TIMES
tmp=all[model%in%c("India MARKAL","RU-TIMES 3.2","IPAC-AIM/technology V1.0","China TIMES","AIM-India [IIMA]")&variable%in%c("Emissions|CO2", "Emissions|CH4", "Emissions|N2O", "Emissions|F-Gases")]
tmp[variable=="Emissions|CO2"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|CH4"]$value<-tmp[variable=="Emissions|CH4"]$value*25
tmp[variable=="Emissions|CH4"]$unit<-"Mt CO2-equiv/yr"
tmp[variable=="Emissions|N2O"]$value<-tmp[variable=="Emissions|N2O"]$value*298/1000
tmp[variable=="Emissions|N2O"]$unit<-"Mt CO2-equiv/yr"
tmp=spread(tmp[!duplicated(tmp)],variable,value)
tmp=tmp%>%mutate(`Emissions|Kyoto Gases`=`Emissions|CO2`+`Emissions|CH4`+`Emissions|N2O`+`Emissions|F-Gases`) #+`Emissions|N2O|AFOLU`+`Emissions|CH4|AFOLU`
tmp=gather(tmp,variable,value,c(`Emissions|Kyoto Gases`, `Emissions|CO2`, `Emissions|CH4`, `Emissions|N2O`, `Emissions|F-Gases`))
tmp=data.table(tmp)
tmp=tmp[variable=="Emissions|Kyoto Gases"]
setcolorder(tmp,c("scenario","Category","Baseline","model","region","period","Scope","value","unit","variable"))
all=all[!(variable=="Emissions|Kyoto Gases" & model%in%c("India MARKAL"))]
all<-rbind(all,tmp)

# IV.6 20 Add NDC scenario for PRIMES
tmp=all[model=="PRIMES_V1" & scenario=="INDC2030_low_V4" & period <= 2030]
tmp$scenario <- "INDC_V4"
tmp$Category <- "NDC"
all <- rbind(all, tmp)

# VI. Post-process variables, set to zerg
# VI.1 AFOLU variables
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

# VI.3 Adding "Emissions|CO2|Energy|Demand|Residential and Commercial" to models that don't report them, but have "Emissions|CO2|Energy|Demand|Residential"
tmp1 <- all[model %in% setdiff(unique(all[variable=="Emissions|CO2|Industrial Processes"]$model),unique(all[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]$model)) &
              variable %in% c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
if(dim(tmp1)[1]!=0 & "Emissions|CO2|Energy|Demand|Residential" %in% unique(tmp1$variable) & "Emissions|CO2|Energy|Demand|Commercial" %in% unique(tmp1$variable)){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Emissions|CO2|Energy|Demand|Residential and Commercial`=`Emissions|CO2|Energy|Demand|Residential` + `Emissions|CO2|Energy|Demand|Commercial`)
  tmp1=gather(tmp, variable, value, c(`Emissions|CO2|Energy|Demand|Residential and Commercial`,`Emissions|CO2|Energy|Demand|Residential`, `Emissions|CO2|Energy|Demand|Commercial`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Emissions|CO2|Energy|Demand|Residential and Commercial"]
  all <- rbind(all,tmp1)} 

# VII. interpolate variables that only report 10 years, add 5 year interpolations
source("functions/interpolate_variables_5yr.R")

all <- as.data.frame(all)
