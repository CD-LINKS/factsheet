
#specify plot scope
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "World")
mods <- unique(all$model)
vars <- "Emissions|CO2"
cats <- c("NoPOL","NPi","INDC", "2030_high", "2030_low", "2020_high", "2020_low", "2020_verylow")
# scens <- c( "NPi", "INDCi", "2020_1800","2030_1800", "2020_1000", "2030_1000", "2020_400",
#             "2030_1000")
#scens2deg <- c("INDC", "2030_high", "2030_low", "2020_verylow")
scens2deg <- c("NPi", "2030_low", "2020_low", "2020_verylow")

# v_co2reg <- v_procDataScen %>% filter (variable == "Emissions|CO2", region %in% regs & !is.na(value))
# v_co2reg_cum <- calcCumulatedDiscount(as.quitte(v_co2reg),discount = 0,fixYear = 2010,nameVar="Emissions|CO2") %>% filter (period==2050)
#
# v_co2fosreg <- v_procDataScen %>% filter (variable == "Emissions|CO2|Energy and Industry" , region %in% regs & !is.na(value))
# v_co2fosreg_cum <- calcCumulatedDiscount(as.quitte(v_co2reg),discount = 0,fixYear = 2010,nameVar="Emissions|CO2|Energy and Industry" ) %>% filter (period==2050)

#calculate emissions and 2050 budgets
v_emireg <- all %>%
    filter (variable %in% vars & region %in% regs & !is.na(value) & Category %in% cats & model %in% mods) %>%
    #mutate(value = value / 1000, unit = "GtCO2/yr") %>%
    factor.data.frame()
v_emireg <- as.data.table(v_emireg)
v_emireg$period <- as.numeric(as.character(v_emireg$period))
v_budgreg <- calcBudget(data = v_emireg,var = vars,new_var = paste0("Budget|",vars))

tmp1 <- filter(v_budgreg, variable=="Budget|Emissions|CO2",period == 2050)
tmp2 <- filter(v_emireg, period == 2010)



### budgets expressed as multiples of 2010 to get rid of baseyear differences
v_emi_cumrel <- rbind(tmp1, tmp2) %>%
    select(-period, -unit) %>%
    spread(key = variable, value = value) %>%
    mutate( CO2rel2010 = 1000* `Budget|Emissions|CO2` / `Emissions|CO2` ) %>%
    select(model, scenario, Category, Scope,region, `Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` ) %>%
        # rename( `CO2 Energy/Ind 2010-2050` = `Emissions|CO2|Energy and Industrial Processes|aggregated`,
        #         `CO2 total 2010-2050` = `Emissions|CO2|aggregated` ) %>%
        arrange(region, scenario, Category, Scope,model )

library(openxlsx)
##write out calculation result
write.csv(v_emi_cumrel, file = "EmissionBudgets.csv", row.names = F,
          col.names = c("MODEL", "SCENARIO", "REGION", "CO2 Energy&Ind 2010",  "CO2 E&I 2010-2050", "Emission Years E&I",
                        "CO2 total 2010",  "CO2 total 2010-2050", "Emission Years CO2 total"))
write.xlsx(v_emi_cumrel, file = "EmissionBudgets.xlsx")



##plotting

theme_set(ggplot2::theme_bw(base_size = 15))

v_plot <-  filter(v_emi_cumrel, Category %in% scens2deg)

v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)

# # CD-LINKS COP Plots
# plotstyle.add(c("NPi") ,c("NPi"),  c( "#882222"), replace = T)
# plotstyle.add(c("INDC") ,c("INDC"),  c( "#885555"), replace = T)
# plotstyle.add(c("2030_high") ,c("50% 2?C"),  c( "#558888"), replace = T)
# plotstyle.add(c("2030_low") ,c("2?C"),  c( "#56B4E9"), replace = T)
# plotstyle.add(c("2020_verylow") ,c("1.5?C"),  c( "#000080"), replace = T)

v_plot$Category =  factor(v_plot$Category, levels = scens2deg, ordered = T)


ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=Category,y=`CO2rel2010`, fill = Category), outlier.size = 0, coef = 3) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=`CO2rel2010`, shape = model),color = 'white',size = 2) +
    geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=`CO2rel2010`, shape = model),color = 'black',size = 3, stroke = 1) +
  #    geom_point(data=v_plot,aes(x=scenario,y=`CO2rel2010` ,shape=model)) +
    ggtitle(paste0("CO2 total (2011-2050 rel. to 2010)")) + ylab("Emission Years") +
    xlab ("") +
    scale_shape_manual(values = 1:nlevels(v_plot$model)) +
    scale_fill_manual(values= plotstyle(scens2deg),
                      #labels=c("INDC", "med2°C","below2°C", "1.5°C"),
                      labels=c("NPi", "NDC1000","NPi1000", "NPi400"),
                      name="") +
    theme( strip.background = element_blank(), plot.background = element_rect(fill     = "transparent",colour = NA ),
           panel.background = element_rect(fill     = "transparent",colour = NA )) +
    scale_x_discrete(labels=c("NPi", "NDC1000","NPi1000", "NPi400")) + #labels=c("INDC", "med2°C","below2°C", "1.5°C")
#    theme(axis.text.y  = element_blank()) +
  facet_grid(  . ~ region) +
  coord_cartesian(ylim = c(0,80)) +
  theme( strip.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA ),
         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))


ggsave(file=paste0("plots/","CO2_EmissionYears_COP_fliped","_multiregbox.png"),
       width=25, height=17, unit="cm", dpi=300) #, bg = "transparent"




### total budget

regs <- c("CHN","USA","IND","EU","RUS","BRA","JPN")

v_emi_cumrel <- rbind(tmp1, tmp2) %>%
  select(-period, -unit) %>%
  spread(key = variable, value = value) %>%
  mutate( CO2rel2010 = 1000* `Budget|Emissions|CO2` / `Emissions|CO2` ) %>%
  select(model, scenario, Category, Scope,region, `Emissions|CO2`,`Budget|Emissions|CO2`,  `CO2rel2010` ) %>%
  # rename( `CO2 Energy/Ind 2010-2050` = `Emissions|CO2|Energy and Industrial Processes|aggregated`,
  #         `CO2 total 2010-2050` = `Emissions|CO2|aggregated` ) %>%
  arrange(region, scenario, Category, Scope,model )




##plotting

theme_set(ggplot2::theme_bw(base_size = 15))

v_plot <-  filter(v_emi_cumrel, Category %in% scens2deg, region %in% regs)

v_plot <- mutate(v_plot, CO2Budget = `Budget|Emissions|CO2` * 1)

v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)

# # CD-LINKS COP Plots
# plotstyle.add(c("NPi") ,c("NPi"),  c( "#882222"), replace = T)
# plotstyle.add(c("INDC") ,c("INDC"),  c( "#885555"), replace = T)
# plotstyle.add(c("2030_high") ,c("50% 2?C"),  c( "#558888"), replace = T)
# plotstyle.add(c("2030_low") ,c("2?C"),  c( "#56B4E9"), replace = T)
# plotstyle.add(c("2020_verylow") ,c("1.5?C"),  c( "#000080"), replace = T)

v_plot$Category =  factor(v_plot$Category, levels = scens2deg, ordered = T)


ggplot() +
  geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=Category,y=CO2Budget, fill = Category), outlier.size = 0, coef = 3) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'white',size = 2) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'black',size = 3, stroke = 1) +
  #    geom_point(data=v_plot,aes(x=scenario,y=`CO2rel2010` ,shape=model)) +
  ggtitle(paste0("CO2 total (2011-2050)")) + ylab("CO2 Budget [GtCO2]") +
  xlab ("") +
  scale_shape_manual(values = 1:nlevels(v_plot$model)) +
  scale_fill_manual(values= plotstyle(scens2deg),
                    #labels=c("INDC", "med2°C","below2°C", "1.5°C"),
                    labels=c("NPi", "NDC1000","NPi1000", "NPi400"),
                    name="") +
  theme( strip.background = element_blank(), plot.background = element_rect(fill     = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA )) +
  scale_x_discrete(labels=c("NPi", "NDC1000","NPi1000", "NPi400")) + #labels=c("INDC", "med2°C","below2°C", "1.5°C")
  #    theme(axis.text.y  = element_blank()) +
  facet_grid(  . ~ region) +
  theme( strip.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA ),
         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))


ggsave(file=paste0("plots/","CO2_EmissionBudgets","_multiregbox.png"),
       width=25, height=17, unit="cm", dpi=300) #, bg = "transparent"

# Only national scenarios
ggplot() +
  #geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=Category,y=CO2Budget, fill = Category), outlier.size = 0, coef = 3) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'white',size = 2) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'black',size = 3, stroke = 1) +
  #    geom_point(data=v_plot,aes(x=scenario,y=`CO2rel2010` ,shape=model)) +
  ggtitle(paste0("CO2 total (2011-2050)")) + ylab("CO2 Budget [GtCO2]") +
  xlab ("") +
  scale_shape_manual(values = 1:nlevels(v_plot$model)) +
  scale_fill_manual(values= plotstyle(scens2deg),
                    #labels=c("INDC", "med2°C","below2°C", "1.5°C"),
                    labels=c("NPi", "NDC1000","NPi1000", "NPi400"),
                    name="") +
  theme( strip.background = element_blank(), plot.background = element_rect(fill     = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA )) +
  scale_x_discrete(labels=c("NPi", "NDC1000","NPi1000", "NPi400")) + #labels=c("INDC", "med2°C","below2°C", "1.5°C")
  #    theme(axis.text.y  = element_blank()) +
  facet_grid(  . ~ region) +
  theme( strip.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA ),
         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))

ggsave(file=paste0("plots/","CO2_EmissionBudgets_national","_multiregbox.png"),
       width=25, height=17, unit="cm", dpi=300) #, bg = "transparent"



