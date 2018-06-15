#### script for doing analysis of regional emission budgets

#process data for all regions (first run main_xCut until 156)
cfg$regions <-c("ARG","AUS","BRA","CAN","CHN","EU","IDN","IND","JPN","MEX","R5ASIA","R5LAM","R5MAF","R5OECD90+EU","R5REF","ROK","RUS","SAF","SAU","TUR","USA","World")

regs <- unique(all$region)

# define global models and variables to look at
# mods <- c("AIM/CGE","IMAGE 3.0","MESSAGE-GLOBIOM_1.0","REMIND-MAgPIE 1.7-3.0","DNE21+ V.14","WITCH2016","COPPE-COFFEE 1.0")
mods <- unique(all$model)
mods <- mods[mods!="global"]

#national modesl
# nat_mods <- unique(all$model)[grep(pattern =  "*",x = unique(all$model),fixed=TRUE)]

# define global scenarios to look at for budget analysis
scens <- c("NoPolicy_V4","NoPOL_V4", "NPi_V4", "INDCi_V4", "NPi2020_1600_V4","INDC2030i_1600_V4", "NPi2020_1000_V4", "INDC2030i_1000_V4", "NPi2020_400_V4",
            "INDC2030i_400_V4","NPi2020_low_V4","NPi2020_high_V4","INDC2030_low_V4","INDC2030_high_V4")
scens2deg <- c("NPi_V4","INDCi_V4", "NPi2020_1600_V4","NPi2020_1000_V4", "NPi2020_400_V4")

#prepare data
v_emireg <- all
v_emireg$period <- as.numeric(v_emireg$period)
v_emireg <- v_emireg[!is.na(v_emireg$period),]
v_emireg <- v_emireg[!(Scope=="global" & scenario == "INDC_V4"),]

v_plot <- v_emireg
v_plot[model=="*PRIMES_V1"& scenario=="INDC2030_low_V4",]$scenario <- "INDC_V4"

varis <- c("Emissions|CO2","Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Energy","Carbon budget","Carbon budget|Energy and Industry","Carbon budget|Energy")

v_emireg <- v_emireg %>% 
  filter (!Category =="Historical" & variable %in% varis & region %in% regs & !is.na(value) & scenario %in% scens & model %in% mods) %>%
  factor.data.frame() %>% data.table()


#### Analysis of global budgets - comparison with chosen budgets and national scenarios

#assign national scenarios to "corresponding" global scenarios 
v_emireg[v_emireg$scenario == "NPi2020_low_V4",]$scenario <- "NPi2020_1000_V4"
v_emireg[v_emireg$scenario == "NPi2020_high_V4",]$scenario <- "NPi2020_1600_V4"
v_emireg[v_emireg$scenario == "INDC2030_low_V4",]$scenario <- "INDC2030i_1000_V4"
v_emireg[v_emireg$scenario == "INDC2030_high_V4",]$scenario <- "INDC2030i_1600_V4"


#select variables in right point in time
tmp1 <- v_emireg %>% filter(variable %in% c("Carbon budget", "Carbon budget|Energy and Industry", "Carbon budget|Energy"), period == 2100)
tmp2 <- v_emireg %>% filter(variable %in% c("Emissions|CO2",  "Emissions|CO2|Energy and Industrial Processes",  "Emissions|CO2|Energy"),period == 2010)

## #Calculate remaining budgets
tmp1 <- rbind(tmp1,tmp2)  %>%
select(-period, -unit) %>%
  spread(key = region, value = value,fill = 0) %>% 
  mutate( RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` - `CAN` - `TUR` - `IDN` - `MEX` -`SAF`-`AUS`) %>%
  select(model, scenario, variable, Scope, `RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`,`CAN`,`TUR`,`IDN`, `MEX`, `SAF`,`AUS` ) %>%
  arrange(variable, scenario,  model ) %>% gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`,`CAN`,`TUR`,`IDN`, `MEX`, `SAF`,`AUS` ) %>%
  filter(!value==0)
#get rid of values < 0 for RoW
tmp1 <- tmp1[!(tmp1$region == "RoW" & tmp1$value <0),]

ref_budgets <- ref_budgets %>% spread(key = region, value = value) %>%
mutate( RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS`) %>%
  gather(region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`)

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
          col.names = c("MODEL", "SCENARIO", "REGION", "CO2 Energy&Ind 2010",  "CO2 E&I 2010-2100", "Emission Years E&I",
                        "CO2 total 2010",  "CO2 total 2010-2100", "Emission Years CO2 total"))
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
    ggtitle(paste0(" Cumulative CO2 (incl. AFOLU) 2011-2100")) + ylab("Gt CO2") +
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
  geom_text(data=ref_budgets,aes(x="NPi2020_400_V4",y=value*0.97,label=as.character(round(value))),colour=rep(c("#aa0000","#0000aa"),9))+ #
  geom_hline(data=ref_budgets,aes(yintercept=value),colour=rep(c("#aa0000","#0000aa"),9))+  #
    facet_wrap(~region, scales = "free_y") +
    ggtitle(paste0(" Cumulative CO2 Energy and Industry 2011-2100")) + ylab("Gt CO2") +
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
  geom_text(data=ref_budgets,aes(x="NPi2020_400_V4",y=value*0.97,label=as.character(round(value))),colour=rep(c("#aa0000","#0000aa"),9))+ #
  geom_hline(data=ref_budgets,aes(yintercept=value),colour=rep(c("#aa0000","#0000aa"),9))+  #
  facet_wrap(~region, scales = "free_y") +
  ggtitle(paste0(" Cumulative CO2 Energy 2011-2100")) + ylab("Gt CO2") +
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
    ggtitle(paste0("CO2 Energy and Industry (2011-2100 rel. to 2010)")) + ylab("Emission Years") +
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
    ggtitle(paste0("CO2 total (2011-2100 rel. to 2010)")) + ylab("Emission Years") +
  scale_color_manual(values = c(rep("black",10),rep("red",10)))+
  scale_shape_manual(values = rep(seq(1,10),2)) +
  scale_size_manual(values = c(rep(1,10),rep(3,10))) +
    theme(axis.text.x  = element_blank() )
ggsave(file=paste0("plots/","CO2tot_EmissionYears_2deg","_multiregbox.pdf"),
       width=24, height=18, unit="cm", dpi=300, bg = "transparent")

