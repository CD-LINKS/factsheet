#var <- fread(paste("settings/",variables,".csv",sep=""),header=TRUE,stringsAsFactors=FALSE,sep='\n')
var <- c("Emissions|Kyoto Gases", "Emissions|CO2|Energy|Supply",
         "Emissions|CO2|Energy|Demand|Transportation",
         "Emissions|CO2|Energy|Demand|Residential and Commercial",
         "Emissions|CO2|Industry",
         "Emissions|CO2|AFOLU",
         "Emissions|Non-CO2",
         "Final Energy",
         "Final Energy|Transportation", 
         "Final Energy|Residential and Commercial",
         "Final Energy|Industry",
         "Primary Energy",
         "GDP|MER",
         "Emissions|CO2|Energy",
         "Final Energy|Electricity",
         "Final Energy|Solar",
         "Final Energy|Wind",
         "Final Energy|Geothermal",
         "Final Energy|Transportation|Liquids|Biomass",
         "Final Energy|Solids|Biomass",
         "Final Energy|Solids|Biomass|Traditional",
         "Secondary Energy|Electricity",
         "Secondary Energy|Electricity|Coal|w/ CCS",
         "Secondary Energy|Electricity|Gas|w/ CCS",
         "Secondary Energy|Electricity|Biomass",
         "Secondary Energy|Electricity|Hydro",
         "Secondary Energy|Electricity|Solar",
         "Secondary Energy|Electricity|Wind",
         "Secondary Energy|Electricity|Geothermal",
         "Secondary Energy|Electricity|Nuclear")
mods <- c("AIM V2.1", "COPPE-COFFEE 1.0", "DNE21+ V.14")#, "GEM-E3", "IMAGE 3.0", "MESSAGEix-GLOBIOM_1.0", "POLES CDL", "REMIND-MAgPIE 1.7-3.0", "WITCH2016")
#mods <- c("DNE21+ V.14")
dat=all[variable %in% var & Scope=="global" & model%in%mods & 
         Category%in%c('NoPOL', 'NPi', 'INDC', '2020_low', '2020_very_low') & !(is.na(value))]
dat <- as.data.table(dat)
######2050 budgets
# Interpolate to get values for each year
yy=seq(2010,2050, by=5)
dt = dat[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
setnames(dt,"V1","value")
setnames(dt,"V2","period")
dat <- as.data.frame(dat)
all<-rbind(all,dt) %>% as.data.frame()
