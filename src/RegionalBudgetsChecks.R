#### script for doing analysis of regional emission budgets

regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "RoW","World")

#read in PRIMAP-hist data (available at: http://doi.org/10.5880/PIK.2016.003 or http://dataservices.gfz-potsdam.de/pik/showshort.php?id=escidoc:1504004)
primap <- read.csv("data/PRIMAP-hist_v1.0_14-Apr-2016.csv") 
#get rid of unnecessary timesteps pre-1990
primap <- primap[,c(1,2,3,4,5,seq(146,170))]
#long- to wide format
primap <- invisible(melt(primap,measure.vars=names(primap)[grep("[0-9]+",names(primap))],variable.name = "period",variable.factor=FALSE))
primap$period <- as.numeric(substr(as.character(primap$period),start = 2,stop = 5))
#convert to Mt CO2:                     
primap$value <- primap$value /1000
#select countries of interest
map_pri <- data.frame(primap = c("BRA","CHN","IND","EU28","JPN","USA","RUS","EARTH"),
                      dat = c("BRA","CHN","IND","EU","JPN","USA","RUS","World"))
primap$region <- map_pri$dat[match(primap$country,map_pri$primap)]
primap <- primap[primap$region %in% map_pri$dat,]

#select relevant categories and entities:
# Change entity for other variables
entity = "CO2" #"CO2", "KYOTOGHGAR4" 

if(entity=="CO2"){category=c("CAT1A","CAT1B1","CAT1B2","CAT2A","CAT2B","CAT2C","CAT2D","CAT2G","CAT5")}
if(entity=="KYOTOGHGAR4"){category=c("CAT1","CAT2","CAT3","CAT4","CAT5","CAT6","CAT7")}
primap <- primap[primap$category %in% category & primap$entity == entity,]
primap$country <- NULL

#read in CDIAC data for bunkers (regionalized to remind regions: 7 seperate countries (without brazil))
cdiac <- read.csV3("data/CDIAC_bunkers.csv")
cdiac <- invisible(melt(cdiac,measure.vars=names(cdiac)[grep("[0-9]+",names(cdiac))],variable.name = "period",variable.factor=FALSE))
cdiac$period <- as.numeric(substr(as.character(cdiac$period),start = 2,stop = 5))
cdiac$value <- as.numeric(cdiac$value)
cdiac <- cdiac[cdiac$period>1989,]
cdiac <- unique(cdiac)
cdiac$category <- "bunkers"
cdiac$entity <- "CO2"
cdiac$unit <- "Mt CO2"
cdiac$variable <- NULL
cdiac$model <- NULL

if (entity=="CO2") {primap <- rbind(primap,cdiac)}

primap[primap$region == "EUR",]$region <- "EU"
if(entity=="CO2"){primap$category <- factor(primap$category,levels = c("CATM0EL","CAT7","CAT6","CAT5","CAT4","CAT3","bunkers","CAT2G",  
                    "CAT2D","CAT2C","CAT2B","CAT2A","CAT2","CAT1B2","CAT1B1","CAT1A","CAT1","CAT0"))}
if(entity=="KYOTOGHGAR4"){primap$category <- factor(primap$category,levels = c("CATM0EL","CAT7","CAT6","CAT5","CAT4","CAT3","CAT2","CAT1","CAT0"))}

# define global models and variables to look at
# mods <- c("AIM/CGE","IMAGE 3.0","MESSAGE-GLOBIOM_1.0","REMIND-MAgPIE 1.7-3.0","DNE21+ V.14","WITCH2016","COPPE-COFFEE 1.0")
mods <- unique(all$model)
mods <- mods[mods!="global"]

#national modesl
# nat_mods <- unique(all$model)[grep(pattern =  "*",x = unique(all$model),fixed=TRUE)]

# define global scenarios to look at for budget analysis
scens <- c("NoPolicy_V3","NoPOL_V3", "NPi_V3", "INDCi_V3", "NPi2020_1600_V3","INDC2030i_1600_V3", "NPi2020_1000_V3", "INDC2030i_1000_V3", "NPi2020_400_V3",
            "INDC2030i_400_V3","NPi2020_low_V3","NPi2020_high_V3","INDC2030_low_V3","INDC2030_high_V3")
scens2deg <- c("NPi_V3","INDCi_V3", "NPi2020_1600_V3","NPi2020_1000_V3", "NPi2020_400_V3")

#prepare data
v_emireg <- all
v_emireg$period <- as.numeric(v_emireg$period)
v_emireg <- v_emireg[!is.na(v_emireg$period),]
v_emireg <- v_emireg[!(Scope=="global" & scenario == "INDC_V3"),]

v_plot <- v_emireg
v_plot[model=="*PRIMES_V1"& scenario=="INDC2030_low_V3",]$scenario <- "INDC_V3"

#####first comparisons: overlay historical and model data
if(entity=="CO2"){vars <- data.frame(long=c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2","Emissions|CO2|Energy"),short=c("co2ffi","co2tot","co2ene"))}
if(entity=="KYOTOGHGAR4"){vars <- data.frame(long=c("Emissions|Kyoto Gases|Excl. AFOLU CO2","Emissions|Kyoto Gases"),short=c("Kyotoexcl","Kyoto"))}

for (reg in map_pri$dat){
    for (var in c(1,2)){
    p = ggplot()
    if(var == 1){#without LU
      p = p + geom_area(data=primap[primap$region == reg & primap$category != "CAT5",],aes(period,value,group = interaction(category),fill = category))
    if(length(unique(primap[primap$region ==reg,]$category))==7){ 
        p = p + scale_fill_manual(values=c("#000000","red","green","orange","purple","#777777"))}
    if(length(unique(primap[primap$region ==reg,]$category))==9){ 
      p = p + scale_fill_manual(values=c("#884444","blue","orange","red","purple","#663333","#000000","#777777"))}
    if(length(unique(primap[primap$region ==reg,]$category))==10){ 
      p = p + scale_fill_manual(values=c("#884444","blue","grey","orange","red","purple","#663333","#000000","#777777"))}                                                                
    } else { # with LU
      p = p + geom_area(data=primap[primap$region == reg,],aes(period,value,group = interaction(category),fill = category)) 
      if(length(unique(primap[primap$region ==reg,]$category))==7){ 
        p = p + scale_fill_manual(values=c("#000000","red","darkgreen","green","orange","purple","#777777"))}
      if(length(unique(primap[primap$region ==reg,]$category))==9){ 
        p = p + scale_fill_manual(values=c("darkgreen","#884444","blue","orange","red","purple","#663333","#000000","#777777"))}
      if(length(unique(primap[primap$region ==reg,]$category))==10){ 
        p = p + scale_fill_manual(values=c("darkgreen","#884444","blue","grey","orange","red","purple","#663333","#000000","#777777"))}                                                                
    }
    p = p + geom_path(data=v_plot[scenario %in% c("INDCi_V3","INDC_V3") & region == reg & variable == as.character(vars$long[var]) & period < 2031,],
        aes(period, value, group = interaction(model,Scope),linetype=model,size=Scope,color=model)) 
    p = p + scale_size_manual(values = c(0.5,1))
    p = p + scale_color_manual(values = rep(c("#000000","#880000","#0000aa"),6))
    p = p + ggtitle(paste0(vars$long[var]))
    if(entity=="CO2"){p = p + ylab("Mt CO2/yr") + xlab("year")}
    if(entity=="KYOTOGHGAR4"){p = p + ylab("Mt CO2e/yr") + xlab("year")}
    ggsave(paste0("plots/base_year_emi_",reg,vars$short[var],".pdf"))
    }
  }


varis <- c("Emissions|CO2","Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Energy","Carbon budget","Carbon budget|Energy and Industry","Carbon budget|Energy")

v_emireg <- v_emireg %>% 
  filter (!Category =="Historical" & variable %in% varis & region %in% regs & !is.na(value) & scenario %in% scens & model %in% mods) %>%
  factor.data.frame() %>% data.table()


#### Analysis of global budgets - comparison with chosen budgets and national scenarios

#assign national scenarios to "corresponding" global scenarios 
v_emireg[v_emireg$scenario == "NPi2020_low_V3",]$scenario <- "NPi2020_1000_V3"
v_emireg[v_emireg$scenario == "NPi2020_high_V3",]$scenario <- "NPi2020_1600_V3"
v_emireg[v_emireg$scenario == "INDC2030_low_V3",]$scenario <- "INDC2030i_1000_V3"
v_emireg[v_emireg$scenario == "INDC2030_high_V3",]$scenario <- "INDC2030i_1600_V3"


#select variables in right point in time
tmp1 <- v_emireg %>% filter(variable %in% c("Carbon budget", "Carbon budget|Energy and Industry", "Carbon budget|Energy"), period == 2050)
tmp2 <- v_emireg %>% filter(variable %in% c("Emissions|CO2",  "Emissions|CO2|Energy and Industrial Processes",  "Emissions|CO2|Energy"),period == 2010)

## #Calculate remaining budgets
tmp1 <- rbind(tmp1,tmp2)  %>%
select(-period, -unit) %>%
  spread(key = region, value = value,fill = 0) %>% 
  mutate( RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` ) %>%
  select(model, scenario, variable, Scope, `RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS` ) %>%
  arrange(variable, scenario,  model ) %>% gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS` ) %>%
  filter(!value==0)
#get rid of values < 0 for RoW
tmp1 <- tmp1[!(tmp1$region == "RoW" & tmp1$value <0),]

ref_budgets <- ref_budgets %>% spread(key = region, value = value) %>%
mutate( RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` ) %>%
  gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS` )

### budgets expressed as multiples of 2010 to get rid of baseyear differences
v_emi_cumrel <- tmp1 %>%
# #     select(-period, -unit) %>%
#    filter(!value==0) %>%
    spread(key = variable, value = value) %>%
    mutate( FFIrel2010 = 1000* `Carbon budget|Energy and Industry` / `Emissions|CO2|Energy and Industrial Processes` ) %>%
    mutate( CO2rel2010 = 1000* `Carbon budget` / `Emissions|CO2` ) %>%
  mutate( ENErel2010 = 1000* `Carbon budget|Energy` / `Emissions|CO2|Energy` ) %>%
    select(model, scenario, region, Scope, `Emissions|CO2|Energy and Industrial Processes`,    `Carbon budget|Energy and Industry`,
            `FFIrel2010` , `Emissions|CO2`,`Carbon budget`,  `CO2rel2010` ,`Emissions|CO2|Energy`, `Carbon budget|Energy`) %>%
  filter(!is.na(`Carbon budget`)) %>%
        rename( `Emissions|CO2|FFI|aggregated` = `Carbon budget|Energy and Industry`,
                `Emissions|CO2|aggregated` = `Carbon budget` ) %>%
        arrange(region, scenario,  model ) %>% factor.data.frame()


write.csv(v_emi_cumrel, file = "EmissionBudgets.csv", row.names = F,
          col.names = c("MODEL", "SCENARIO", "REGION", "CO2 Energy&Ind 2010",  "CO2 E&I 2010-2050", "Emission Years E&I",
                        "CO2 total 2010",  "CO2 total 2010-2050", "Emission Years CO2 total"))
write.xlsx(v_emi_cumrel, file = "EmissionBudgets.xlsx")




theme_set(ggplot2::theme_bw(base_size = 15))

v_plot <-  filter(v_emi_cumrel, scenario %in% scens2deg) 

v_plot$scenario =  factor(v_plot$scenario, levels = scens2deg, ordered = T)
v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)

### cumulative budget plots
#1) Total (incl. AFOLU)
ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=scenario,y=`Emissions|CO2|aggregated`, fill = scenario), outlier.size = 0) +
    geom_point(data=v_plot,aes(x=scenario,y=`Emissions|CO2|aggregated`,shape=model,color=model,size=model)) +
    facet_wrap(~region, scales = "free_y") +
    ggtitle(paste0(" Cumulative CO2 (incl. AFOLU) 2011-2050")) + ylab("Gt CO2") +
    scale_color_manual(values = c(rep("black",10),rep("red",10)))+
    scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
    theme(axis.text.x  = element_blank())
ggsave(file=paste0("plots/","CO2tot_budget_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")

#2) Energy and Industry
ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=scenario,y=`Emissions|CO2|FFI|aggregated`, fill = scenario), outlier.size = 0) +
  geom_point(data=v_plot,aes(x=scenario,y=`Emissions|CO2|FFI|aggregated`,shape=model,color=model,size=model)) +
  geom_text(data=ref_budgets,aes(x="NPi2020_400_V3",y=value*0.97,label=as.character(round(value))),colour=rep(c("#aa0000","#0000aa"),9))+ #
  geom_hline(data=ref_budgets,aes(yintercept=value),colour=rep(c("#aa0000","#0000aa"),9))+  #
    facet_wrap(~region, scales = "free_y") +
    ggtitle(paste0(" Cumulative CO2 Energy and Industry 2011-2050")) + ylab("Gt CO2") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = c(seq(1,10),seq(1,10))) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
    theme(axis.text.x  = element_blank() )
ggsave(file=paste0("plots/","CO2EneInd_budget_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")

#3) Energy
ggplot() +
  geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=scenario,y=`Carbon budget|Energy`, fill = scenario), outlier.size = 0) +
  geom_point(data=v_plot,aes(x=scenario,y=`Carbon budget|Energy`,shape=model,color=model,size=model)) +
  geom_text(data=ref_budgets,aes(x="NPi2020_400_V3",y=value*0.97,label=as.character(round(value))),colour=rep(c("#aa0000","#0000aa"),9))+ #
  geom_hline(data=ref_budgets,aes(yintercept=value),colour=rep(c("#aa0000","#0000aa"),9))+  #
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0(" Cumulative CO2 Energy 2011-2050")) + ylab("Gt CO2") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = c(seq(1,10),seq(1,10))) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
  theme(axis.text.x  = element_blank() )
ggsave(file=paste0("plots/","CO2EneOnly_budget_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")

### emissions years
ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=scenario,y=`FFIrel2010`, fill = scenario), outlier.size = 0) +
  geom_point(data=v_plot,aes(x=scenario,y=`FFIrel2010`,shape=model,color=model,size=model)) +
    facet_wrap(~region, scales = "free_y") +
    ggtitle(paste0("CO2 Energy and Industry (2011-2050 rel. to 2010)")) + ylab("Emission Years") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
    theme(axis.text.x  = element_blank() )
ggsave(file=paste0("plots/","CO2Ene_EmissionYears_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")


ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=scenario,y=`CO2rel2010`, fill = scenario), outlier.size = 0) +
  geom_point(data=v_plot,aes(x=scenario,y=`CO2rel2010`,shape=model,color=model,size=model)) +
    facet_wrap(~region, scales = "free_y") +
    ggtitle(paste0("CO2 total (2011-2050 rel. to 2010)")) + ylab("Emission Years") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
    theme(axis.text.x  = element_blank() )
ggsave(file=paste0("plots/","CO2tot_EmissionYears_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")

