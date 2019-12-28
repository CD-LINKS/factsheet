# Selection of data for effort sharing paper

# Emission pathways -------------------------------------------------------
ep=all[variable%in%c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU")&Scope=="global"&
         region%in%c("BRA","JPN","EU","USA","CHN","IND","RUS")&
         Category%in%c("2020_low","2020_verylow","NoPOL")]
ep$Baseline<-NULL
ep$Scope<-NULL
ep$Category<-NULL
setnames(ep,"variable","Category")
setnames(ep,"unit","Unit")
setnames(ep,"region","Region")
setnames(ep,"scenario","Scenario")
ep$Scenario<-str_replace_all(ep$Scenario,"NoPolicy_V3","BAU")
ep$Scenario<-str_replace_all(ep$Scenario,"NPi2020_400_V3","Gt400")
ep$Scenario<-str_replace_all(ep$Scenario,"NPi2020_1000_V3","Gt1000")
ep=spread(ep,period,value)
write.csv(ep,"CO_BAU_pathways.csv")


# Carbon budgets ----------------------------------------------------------
source("RegionalBudgetsChecks2100.R")
v_emi_cumrel=data.table(v_emi_cumrel)
cb=v_emi_cumrel[region%in%c("BRA","JPN","EU","USA","CHN","IND","RUS")&
                  scenario%in%c("NoPolicy_V3","NPi2020_1000_V3","NPi2020_400_V3")]
cb$`Emissions|CO2|Energy and Industrial Processes`<-NULL
cb$FFIrel2010<-NULL
cb$`Emissions|CO2`<-NULL
cb$CO2rel2010<-NULL
cb$`Emissions|CO2|Energy`<-NULL
cb$`Carbon budget|Energy`<-NULL
setnames(cb,"Emissions|CO2|FFI|aggregated","Emissions|CO2|Energy and Industrial processes")
setnames(cb,"Emissions|CO2|aggregated","Emissions|CO2")
write.csv(cb,"CO_BAU_carbonbudgets.csv")
