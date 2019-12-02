#stackedbar waterfall plot
#regs = c("BRA", "CHN", "IND", "RUS", "EU", "JPN", "USA", "ydummy")
#regs_global = c("World","CHN", "IND", "RUS", "EU", "JPN", "USA","BRA")
regs = c("BRA", "CHN", "IND", "RUS", "EU", "JPN", "USA")

#regional color palette, copied from functions/script_RegionalBudgets_FFI.R
#colors_regional=c("IND"=hsv(0,1,1),"CHN"=hsv(0.3,1,1),"BRA"="#85AD00","RUS"=hsv(0.8,1,1),"EU"=hsv(0.1,1,1),"JPN"=hsv(0.5,1,1),"USA"=hsv(0.7,1,1),yaROW="#444444",ydummy="#ffffff",ybWorld="grey")


vars <- c(    "Emissions|CO2|Energy|Supply",
              "Emissions|CO2|Energy|Demand|Industry",
              "Emissions|CO2|Energy|Demand|Residential and Commercial",
              "Emissions|CO2|Energy|Demand|Transportation")

ylab <-  "Emissions [Mt CO2]"
titletag <- "Waterfall_"
file.prefix <- "Waterfall_"

#scensglob = c("NPi1000")
#catsglob <- c("NPi1000","NPi")
catsnat <- c( "NPi400","NPi")

labcat <- c("NPi400","NPi")

natmods <- unique(all[Scope=="national"]$model)
  #setdiff(unique(all$model),grep("\\*",unique(all$model),value = T))
#check if model has problems with Emissions accounting
# check <- all %>% filter(variable %in% c("Emissions|CO2|Energy",vars), model %in% natmods, Category %in% c("2030_high",catsnat)) %>%
#          spread(key=variable,value=value) %>% mutate(check = .[[9]]-.[[10]]-.[[11]]-.[[12]]-.[[13]] ) %>% filter (check < -000) 
# check <- unique(check$model)
# natmods <- setdiff(natmods,check)

#choose reference scenario INDC or NoPOL
reference_cat <- catsnat[2]


for(icat in c(1)){
  for(ireg in c(1)){ #now for NPi400 manually doing ireg in c(1) for BRA and c(5) for EU (and for all scenarios, CHN 2, IND 3, RUS 4, JPN 6, usa 7) instead of c(1,2,3,4,5,6,7)
  tt = c("2010", "2050")
  #if(icat == 2) {catsnat[1] <- "2030_high"}
  for(iper in c(1,2)){
    if(iper == 2) {tt[2] <- "2030"}
#dtg <- filter(all, Category %in% catsglob, region %in% regs_global,model %in% globmods, variable %in% vars, period %in% tt, Scope == "global") %>% group_by(scenario,Category,Baseline,region,variable,period,Scope,unit) %>% summarize(value=mean(value)) %>% ungroup()
#for now, REMIND, (later: global average)

dtn <- filter(all, Category %in% catsnat, region %in% regs[ireg],variable %in% vars, period %in% tt, Scope == "national")  %>% select(-Baseline)  %>% group_by(scenario,Category,region,variable,period,Scope,unit) %>% summarize(value=mean(value)) %>% ungroup()
#for now, filter out duplicate models for same region: *India MARKAL, *IPAC-AIM/technology V1.0, *DNE21+ V.14 (national) (later: average)

#calculate ROW:  global minus explicit global model region values 
# dt = dtg %>% factor.data.frame() %>% select(-scenario,-Baseline,-Scope) %>%
#   spread(region,value=value) %>% mutate(ROW = World - CHN-EU-IND-JPN-RUS-USA-BRA) %>% select(Category,period,unit,variable,ROW) %>% 
#   mutate(value=ROW) %>% select(-ROW)
# dt$region <- "yaROW" # stupid name, such that it is between USA and ydummy
# dt$Scope <- "global"
# dt$scenario <- "global"
# dt$Category <- factor(dt$Category,levels=unique(c(levels(dt$Category),catsnat[1])))
# dt[dt$Category== catsglob[1],]$Category <- catsnat[1]
# dtn <- rbind(dtn,dt)


# defining the stacks
dtn_1 <- filter(dtn, Category==reference_cat, variable %in% vars, period == "2010") %>%
  mutate(bar_position = "1") 

dtn_2 <- filter(dtn, Category==reference_cat, variable %in% vars, period == tt[2]) %>%
  mutate(bar_position = "2") %>% factor.data.frame()

#last stack: ambitious mitigation scenario
dtn_7 <- filter(dtn, Category==catsnat[1], variable %in% vars, period == tt[2]) %>%
  mutate(bar_position = "7") %>% factor.data.frame()

# calculating the difference
dtn_tmp <- filter(dtn, variable %in% vars, period == tt[2])
# dtn_tmp <- dtn_tmp[!names(dtn) %in% c("Category","Baseline")] #drop category and baseline
# dtn_tmp1 <- spread(dtn_tmp, scenario, value=value)
dtn_tmp <- dtn_tmp %>% select(-scenario) #drop category and baseline
dtn_tmp1 <- spread(dtn_tmp,Category, value=value)

# filling gaps by 1) assuming that emissions are zero in low-scenario AND 2) using NoPOL where no INDC_V2
# dtn_tmp1[is.na(dtn_tmp1$NPi2020_low_V2),]$NPi2020_low_V2 <- 0
#dtn_tmp2 <- mutate(dtn_tmp1,diff_to_reference = ifelse(is.na(INDC_V2),NoPOL_V2 - NPi2020_low_V2,INDC_V2 - NPi2020_low_V2))
#or alternatiively without this assumption:
#names(dtn_tmp1)[7] <- "low"
setnames(dtn_tmp1,paste0("",catsnat[1],""),"low")
dtn_tmp2 <- mutate(dtn_tmp1,diff_to_reference = NPi - low)

dtn_3 <- filter(dtn_tmp2, variable == vars[1], region == regs[ireg], period == tt[2]) %>%
  mutate(bar_position = "3")%>%  mutate(value = diff_to_reference) %>% factor.data.frame()

dtn_4 <- filter(dtn_tmp2, variable == vars[2], region == regs[ireg], period == tt[2]) %>%
  mutate(bar_position = "4")%>%  mutate(value = diff_to_reference) %>% factor.data.frame()

dtn_5 <- filter(dtn_tmp2, variable == vars[3], region == regs[ireg], period == tt[2]) %>%
  mutate(bar_position = "5")%>%  mutate(value = diff_to_reference) %>% factor.data.frame()

dtn_6 <- filter(dtn_tmp2, variable == vars[4], region == regs[ireg], period == tt[2]) %>%
  mutate(bar_position = "6")%>%  mutate(value = diff_to_reference) %>% factor.data.frame()


# only region, value (and later variable) needed
reduction <- c("region","value", "variable","bar_position")
# dtn_1_reduced <- dtn_1 %>% group_by(region,bar_position) %>% summarise(value = sum(value))  %>%
#   mutate(variable = "total")
dtn_2_reduced <- dtn_2 %>% group_by(region,bar_position) %>% summarise(value = sum(value)) %>%
  factor.data.frame() %>%  mutate(variable = "total")
dtn_1_stack_reduced <- dtn_1[names(dtn_1) %in% reduction]
dtn_2_stack_reduced <- dtn_2[names(dtn_2) %in% reduction]
dtn_3_reduced <- dtn_3[names(dtn_3) %in% reduction]
dtn_4_reduced <- dtn_4[names(dtn_4) %in% reduction]
dtn_5_reduced <- dtn_5[names(dtn_5) %in% reduction]
dtn_6_reduced <- dtn_6[names(dtn_6) %in% reduction]
# dtn_7_reduced <- dtn_7 %>% group_by(region,bar_position) %>% summarise(value = sum(value)) %>%
#   factor.data.frame() %>%  mutate(variable = "total")
dtn_7_stack_reduced <- dtn_7[names(dtn_7) %in% reduction]

#ydummy region transparent
ydummy3<-data.frame("ydummy",sum(dtn_2_reduced$value)-sum(dtn_3_reduced$value),"total-dummy",3)
names(ydummy3)<-reduction
dtn_3_reduced = rbind(dtn_3_reduced,ydummy3)
ydummy4<-data.frame("ydummy",sum(dtn_3_reduced[dtn_3_reduced$region == "ydummy",]$value)-sum(dtn_4_reduced$value),"total-dummy",4)
names(ydummy4)<-reduction
dtn_4_reduced = rbind(dtn_4_reduced,ydummy4)
ydummy5<-data.frame("ydummy",sum(dtn_4_reduced[dtn_4_reduced$region == "ydummy",]$value)-sum(dtn_5_reduced$value),"total-dummy",5)
names(ydummy5)<-reduction
dtn_5_reduced = rbind(dtn_5_reduced,ydummy5)
ydummy6<-data.frame("ydummy",sum(dtn_5_reduced[dtn_5_reduced$region == "ydummy",]$value)-sum(dtn_6_reduced$value),"total-dummy",6)
names(ydummy6)<-reduction
dtn_6_reduced = rbind(dtn_6_reduced,ydummy6)

dtn_all=rbind(dtn_1_stack_reduced,dtn_2_stack_reduced, dtn_3_reduced, dtn_4_reduced, dtn_5_reduced, dtn_6_reduced, dtn_7_stack_reduced) #dtn_1_reduced,dtn_2_reduced, dtn_7_reduced
dtn_all$alpha <- 0*dtn_all$value
dtn_all[dtn_all$region !="ydummy",]$alpha <- 1
dtn_all <- data.frame(dtn_all)

dtn_all$bar_position <- as.numeric(dtn_all$bar_position)

#data for small reference bars and boxplots
#1,2,7
ref_g <-  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region == regs[ireg], variable %in% "Emissions|CO2|Energy", period %in% tt, Scope == "national") 
ref_g <- ref_g[ref_g$Category==reference_cat | ref_g$period==tt[2],]
ref_g$bar_position <- 0
ref_g[ref_g$period==2010 & ref_g$Category==reference_cat,]$bar_position <- 1
ref_g[ref_g$period==tt[2] & ref_g$Category==reference_cat,]$bar_position <- 2
ref_g[ref_g$period==tt[2] & ref_g$Category==catsnat[1],]$bar_position <- 7
ref_med <- ref_g %>% group_by(scenario,Category,Baseline,region,variable,period,Scope,unit,bar_position) %>% summarize(value=mean(value)) %>% ungroup()
#add differences to all global model values
ref_g  <-  rbind(ref_g %>% select(-scenario,-Category),filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region ==regs[ireg],
# ref_gi  <-  rbind(filter(all, Category %in% catsglob, region %in% regs_global,model %in% globmods, region == "World", 
                    variable %in% vars[1], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=3),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region == regs[ireg], 
                         variable %in% vars[2], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=4),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region == regs[ireg], 
                         variable %in% vars[3], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=5),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region == regs[ireg], 
                         variable %in% vars[4], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=6))

#add mean of differences to global model means
ref_med <- rbind (ref_med %>% select (-scenario,-Category),filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region ==regs[ireg],
                         variable %in% vars[1], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=3)  %>% 
                    filter(!is.na(value)) %>% group_by(Baseline,region,variable,period,Scope,unit,bar_position) %>% summarize(value=mean(value)) %>% ungroup(),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region ==regs[ireg], 
                         variable %in% vars[2], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=4) %>% 
                    filter(!is.na(value)) %>% group_by(Baseline,region,variable,period,Scope,unit,bar_position) %>% summarize(value=mean(value)) %>% ungroup(),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region ==regs[ireg], 
                         variable %in% vars[3], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=5) %>% 
                    filter(!is.na(value)) %>% group_by(Baseline,region,variable,period,Scope,unit,bar_position) %>% summarize(value=mean(value)) %>% ungroup(),
                  filter(all, Category %in% catsnat, region %in% regs,model %in% natmods, region == regs[ireg], 
                         variable %in% vars[4], period %in% tt[2], Scope == "national") %>% select(-scenario) %>% spread(key=Category,value=value) %>% mutate(value = .[[9]] - .[[8]] ) %>% select(-8,-9) %>% mutate(bar_position=6) %>% 
                    filter(!is.na(value)) %>% group_by(Baseline,region,variable,period,Scope,unit,bar_position) %>% summarize(value=mean(value)) %>% ungroup())
#add ydummy to make start position of bar right
ref_med <- ref_med %>% select(-Baseline,-Scope,-unit,-period) %>% mutate(alpha=1) %>% rbind(dtn_all[dtn_all$region == "ydummy",])
ref_med$region <- as.character(ref_med$region)
#correct height of boxplot
ref_g[ref_g$bar_position==3,]$value = ref_g[ref_g$bar_position==3,]$value + ref_med[ref_med$region == "ydummy" & ref_med$bar_position==3,]$value
ref_g[ref_g$bar_position==4,]$value = ref_g[ref_g$bar_position==4,]$value + ref_med[ref_med$region == "ydummy" & ref_med$bar_position==4,]$value
ref_g[ref_g$bar_position==5,]$value = ref_g[ref_g$bar_position==5,]$value + ref_med[ref_med$region == "ydummy" & ref_med$bar_position==5,]$value
ref_g[ref_g$bar_position==6,]$value = ref_g[ref_g$bar_position==6,]$value + ref_med[ref_med$region == "ydummy" & ref_med$bar_position==6,]$value
names(dtn_all)[names(dtn_all)=="region"] <- "Region"
names(ref_med)[names(ref_med)=="region"] <- "Region"
#ref_med[ref_med$Region=="World",]$Region <- "ybWorld"

### waterfall plot - TODO: add 2030 and 2050 in one graph? save plots in environment for grid arrange?
#plot <- list() #paste0("Plot",icat,ireg,iper,sep="")
#p<-
ggplot()+
  geom_bar(data = dtn_all, aes(x = bar_position, y = value , fill = variable ,alpha=alpha),stat='identity',width=0.7)+
  #geom_bar(data = ref_med, aes(x = bar_position+0.5, y = value/1000,alpha=alpha,fill=variable),stat='identity',width=0.15)+
  #geom_boxplot(data = ref_g,aes(group=bar_position,x = bar_position+0.5,y = value/1000),fill = "grey",width=0.04)+
  scale_fill_manual(values=c("total"="#cc0000","Emissions|CO2|Energy|Demand|Residential and Commercial"="#7777ff","Emissions|CO2|Energy|Demand|Industry"="#bb7700",
                             "Emissions|CO2|Energy|Supply"="#993a44","Emissions|CO2|Energy|Demand|Transportation"="#222288","Emissions|CO2|Energy"="transparent","total-dummy"="transparent"),
                    labels=c("Emissions|CO2|Energy|Demand|Transportation"="Transport","Emissions|CO2|Energy|Demand|Industry"="Industry",
                             "Emissions|CO2|Energy|Demand|Residential and Commercial"="Buildings","Emissions|CO2|Energy|Supply"="Supply","total"="Total","total-dummy"=""),
                    name="Sector",guide=F
                    #,labels=c(BRA="Brazil",CHN="China",IND="India",RUS="Russia",EU="EU 28",JPN="Japan",USA="USA",yaROW="RoW",ybWorld="global",ydummy=" ")
                             ) +
  # scale_fill_manual(values=c(BRA="#00BAFF",CHN="#00E539",EU="#B5BF00",IND="#F24200",JPN="#175900",RUS="#D900BC",USA="#8C5400",yaROW="#444444",ydummy="#ffffff"),
                    # labels=c(BRA="Brazil",CHN="China",IND="India",RUS="Russia",EU="EU 28",JPN="Japan",USA="USA",yaROW="RoW",ydummy=" "),
                    # name="Country")+
  scale_x_continuous(breaks=unique(dtn_all$bar_position),minor_breaks = NULL,
                   labels=c(tt[1], paste0(tt[2],"\n",catsnat[2]), "Supply","Industry","Buildings","Transport",paste0(tt[2],"\n",labcat[icat])))+
  ggtitle(regs[ireg])+
  scale_alpha(guide = "none")+#coord_cartesian(ylim=c(0,62))+
  ylab(ylab)+xlab("") + theme_bw()

#plot[[i]]<<-p
# ggsave(filename=paste0(file.prefix,"nat_",labcat[icat],"_",tt[2],"1030.pdf"),width=7, height=3.5)
ggsave(filename=paste0(file.prefix,regs[ireg],labcat[icat],"_",tt[2],"1030.png"),width=5, height=3.5)
# cat(paste("scen",catsnat[1],"time",tt[2],"sum",
# sum(dtn_all[dtn_all$bar_position==7,]$value),"sum without ROW",
# sum(dtn_all[dtn_all$bar_position==7 & dtn_all$Region != "yaROW",]$value),"rel",
# sum(dtn_all[dtn_all$bar_position==7 & dtn_all$Region != "yaROW",]$value)/sum(dtn_all[dtn_all$bar_position==7,]$value),
# "as percent of WB2C",sum(dtn_all[dtn_all$bar_position==7,]$value)/ref_med[ref_med$bar_position==7,]$value,"\n",
# "WB2C",ref_med[ref_med$bar_position==7,]$value))
}
}
}
#write.csv(dtn_all,file="Waterfall_dtn_all.csv",row.names=F,quote=F)
