# Change model name -------------------------------------------------------
# 1. change AIM|Enduse 3.0 to AIM-India[IIMA]
all=data.table(all)
all[model=="AIM/Enduse 3.0"]$model <- "AIM-India [IIMA]"

# 2. Remove one MESSAGE model version
all=all[!model=="MESSAGEix-GLOBIOM_1.0"]

# Adjust regions ----------------------------------------------------------
# MESSAGE and COPPE adjust region EU (exclude Turkey)
all[Category=="NoPOL"]$Baseline <- str_replace_na(all[Category=="NoPOL"]$Baseline,"-")
tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.1","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
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
tmp2$model<-"MESSAGEix-GLOBIOM_1.1"
tmp3$model<-"COPPE-COFFEE 1.0"
tmp1 <- rbind(tmp1,tmp2,tmp3)
tmp1=spread(tmp1,region,value)
tmp1 = na.omit(tmp1)
tmp1 = tmp1%>%mutate(EU=EU-TUR)
tmp1=gather(tmp1,region,value,c(EU,TUR))
tmp1=data.table(tmp1)
tmp1=tmp1[region=="EU"]
all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.1","COPPE-COFFEE 1.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                                       "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                                       "Emissions|CO2|Energy|Demand|Transportation",
                                                                                       "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                                       "Emissions|F-Gases",
                                                                                       "Emissions|CO2|Energy|Demand|Industry",
                                                                                       "Emissions|CO2|Industrial Processes") & region%in%c("EU"))]
all<-rbind(all,tmp1)

# MESSAGE change region USA (exclude Canada)
tmp1 <- all[model%in%c("MESSAGEix-GLOBIOM_1.1","IMAGE 3.0") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                              "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                              "Emissions|CO2|Energy|Demand|Transportation",
                                                                              "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                              "Emissions|F-Gases",
                                                                              "Emissions|CO2|Energy|Demand|Industry",
                                                                              "Emissions|CO2|Industrial Processes") & region%in%c("USA","CAN")]
tmp1=tmp1[!c(model=="IMAGE 3.0"&region=="USA")]
tmp1$model<-"MESSAGEix-GLOBIOM_1.1"
tmp1=spread(tmp1,region,value)
tmp1 = na.omit(tmp1)
tmp1 = tmp1%>%mutate(USA=USA-CAN)
tmp1=gather(tmp1,region,value,c(USA,CAN))
tmp1=data.table(tmp1)
tmp1=tmp1[region=="USA"]
all <- all[!c(model%in%c("MESSAGEix-GLOBIOM_1.1") & variable %in% c("Emissions|Kyoto Gases"," Emissions|CO2|Energy|Supply",
                                                                    "Emissions|CO2|Energy|Demand|Residential and Commercial",
                                                                    "Emissions|CO2|Energy|Demand|Transportation",
                                                                    "Emissions|CO2|AFOLU","Emissions|CH4","Emissions|N2O",
                                                                    "Emissions|F-Gases",
                                                                    "Emissions|CO2|Energy|Demand|Industry",
                                                                    "Emissions|CO2|Industrial Processes") & region%in%c("USA"))]
all<-rbind(all,tmp1)

# To do/decide:
# Removing MESSAGE model for India, as MESSAGE has South Asia (including Afghanistan, Bangladesh, Bhutan, Maldives, Nepal, Pakistan, Sri Lanka), not India separately
India = all[region=="IND"]
India = India[!model=="MESSAGEix-GLOBIOM_1.1"]
all=rbind(subset(all, !region=="IND"),India)

# Removing MESSAGE model for China, as MESSAGE has CPA (including Cambodia, Hong Kong, Korea, Lao, Macau, Mongolia, Taiwan, Viet Nam), not China separately
# China = all[region=="CHN"]
# China = China[!model=="MESSAGE V.4"]
# all=rbind(subset(all, !region=="CHN"),China)


# Add total as sum of sub-categories, when missing ------------------------
tmp1 <- all[model %in% setdiff(unique(all[variable=="Agricultural Production|Energy|Crops"]$model),unique(all[variable=="Agricultural Production|Energy"]$model)) &
              variable %in% c("Agricultural Production|Energy|Crops","Agricultural Production|Energy|Residues")]
if(dim(tmp1)[1]!=0 & "Agricultural Production|Energy|Crops" %in% unique(tmp1$variable)){
  tmp=spread(tmp1,variable, value)
  tmp=na.omit(tmp)
  tmp=tmp %>% mutate(`Agricultural Production|Energy`=`Agricultural Production|Energy|Crops` + `Agricultural Production|Energy|Residues`)
  tmp1=gather(tmp, variable, value, c(`Agricultural Production|Energy`,`Agricultural Production|Energy|Crops`, `Agricultural Production|Energy|Residues`))
  tmp1=data.table(tmp1)
  tmp1=tmp1[variable=="Agricultural Production|Energy"]
  all <- rbind(all,tmp1)} 


# Fix historical CO2 AFOLU emissions IMAGE --------------------------------
image=all[model=="IMAGE 3.0"&variable%in%c("Emissions|CO2|AFOLU","Emissions|Kyoto Gases")&Category%in%c("2020_verylow","2020_low","NoPOL")&period%in%c(2005,2010)]
image=spread(image[,!c('scenario','Baseline'),with=FALSE],Category,value)
image=image%>%mutate(`2020_low`=`NoPOL`,`2020_verylow`=`NoPOL`)
image$`NoPOL`<-NULL
image=data.table(gather(image,Category,value,c("2020_low","2020_verylow")))
image$scenario<-""
image$Baseline<-"NoPolicy_V4"
image[Category=="2020_low"]$scenario<-"NPi2020_1000_V4"
image[Category=="2020_verylow"]$scenario<-"NPi2020_400_V4"
setcolorder(image,colnames(all))
all <- rbind(all[!c(model=="IMAGE 3.0"&variable%in%c("Emissions|CO2|AFOLU","Emissions|Kyoto Gases")&Category%in%c("2020_low","2020_verylow")&period%in%c(2005,2010))],image)
