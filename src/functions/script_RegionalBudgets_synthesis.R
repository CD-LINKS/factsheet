
#specify plot scope
regs <- c("BRA","CHN","IND","EU","JPN","USA","RUS", "World")
mods <- unique(all$model)
vars <- "Emissions|CO2"
cats <- c("NoPOL","NPi","INDC", "2030_high", "NDC1000", "2020_high", "NPi1000", "NPi400")
scens2deg <- c("NPi", "NDC1000", "NPi1000", "NPi400")

#calculate emissions and 2050 budgets
v_emireg <- all %>%
    filter (variable %in% vars & region %in% regs & !is.na(value) & Category %in% cats & model %in% mods) %>%
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
        arrange(region, scenario, Category, Scope,model )

##plotting
theme_set(ggplot2::theme_bw(base_size = 15))
v_plot <-  filter(v_emi_cumrel, Category %in% scens2deg)
v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)
v_plot$Category =  factor(v_plot$Category, levels = scens2deg, ordered = T)


ggplot() +
    geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=Category,y=`CO2rel2010`, fill = Category), outlier.size = 0, coef = 3) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=`CO2rel2010`, shape = model),color = 'white',size = 2) +
    geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=`CO2rel2010`, shape = model),color = 'black',size = 3, stroke = 1) +
    ggtitle(paste0("CO2 total (2011-2050 rel. to 2010)")) + ylab("Emission Years") +
    xlab ("") +
    scale_shape_manual(values = 1:nlevels(v_plot$model)) +
    scale_fill_manual(values= plotstyle(scens2deg),
                      labels=c("NPi", "NDC1000","NPi1000", "NPi400"),
                      name="") +
    theme( strip.background = element_blank(), plot.background = element_rect(fill     = "transparent",colour = NA ),
           panel.background = element_rect(fill     = "transparent",colour = NA )) +
    scale_x_discrete(labels=c("NPi", "NDC1000","NPi1000", "NPi400")) + 
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
  arrange(region, scenario, Category, Scope,model )

##plotting
theme_set(ggplot2::theme_bw(base_size = 15))

v_plot <-  filter(v_emi_cumrel, Category %in% scens2deg, region %in% regs)
v_plot <- mutate(v_plot, CO2Budget = `Budget|Emissions|CO2` * 1)
v_plot$region =  factor(v_plot$region, levels = regs, ordered = T)
v_plot$Category =  factor(v_plot$Category, levels = scens2deg, ordered = T)

ggplot() +
  geom_boxplot(data=v_plot[v_plot$Scope=="global",],aes(x=Category,y=CO2Budget, fill = Category), outlier.size = 0, coef = 3) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'white',size = 2) +
  geom_point(data=v_plot[v_plot$Scope=="national",],aes(x=Category,y=CO2Budget, shape = model),color = 'black',size = 3, stroke = 1) +
  ggtitle(paste0("CO2 total (2011-2050)")) + ylab("CO2 Budget [GtCO2]") +
  xlab ("") +
  scale_shape_manual(values = 1:nlevels(v_plot$model)) +
  scale_fill_manual(values= plotstyle(scens2deg),
                    labels=c("NPi", "NDC1000","NPi1000", "NPi400"),
                    name="") +
  theme( strip.background = element_blank(), plot.background = element_rect(fill     = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA )) +
  scale_x_discrete(labels=c("NPi", "NDC1000","NPi1000", "NPi400")) +
  facet_grid(  . ~ region) +
  theme( strip.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA ),
         panel.background = element_rect(fill     = "transparent",colour = NA ),
         axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))

ggsave(file=paste0("plots/","CO2_EmissionBudgets","_multiregbox.png"),
       width=25, height=17, unit="cm", dpi=300) 