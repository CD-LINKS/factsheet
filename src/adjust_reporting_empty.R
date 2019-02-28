# Collection of fixes for reporting issues and other adjustments --------------------------------
#

# Model-specific issues ---------------------------------------------------

all <- as.data.table(all)
# 1. change AIM|Enduse 3.0 to AIM-India[IIMA]
all=data.table(all)
all[model=="AIM/Enduse 3.0"]$model <- "AIM-India [IIMA]"

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




# Adding final energy other sector to models that don't report it, needed for calculation of total renewable energy share
# Final Energy|Other Sector|
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy"]$model),unique(all[variable=="Final Energy|Other Sector"]$model))
          &variable=="Final Energy"]
tmp1$variable<-"Final Energy|Other Sector"
tmp1$value<-0
# Final Energy|Other Sector|Electricity
tmp1<-all[model %in% setdiff(unique(all[variable=="Final Energy|Electricity"]$model),unique(all[variable=="Final Energy|Other Sector|Electricity"]$model))
          &variable=="Final Energy|Electricity"]
tmp1$variable<-"Final Energy|Other Sector|Electricity"
tmp1$value<-0

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