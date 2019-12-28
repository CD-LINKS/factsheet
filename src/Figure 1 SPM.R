#country_order <- c('CHN', 'USA', 'IND', 'EU', 'World')
country_order <- c('CHN', 'USA', 'IND', 'EU', 'BRA', 'JPN', 'World')
data_fig_SPM <- filter(d_selec_Kyoto, scenario %in% c('No new policies', 'National policies'), year==2030,
                       region %in% country_order) %>%
                spread(scenario, value) %>%
                mutate(reduction = -1*(`National policies`/`No new policies`-1))
data_fig_SPM$region <- factor(data_fig_SPM$region, levels=country_order)                  
data_fig_SPM_stat <- group_by(data_fig_SPM, region, year, unit) %>% 
                     summarise(median_reduction=median(reduction, na.rm=TRUE),
                               min_reduction=quantile(reduction, .25, na.rm=TRUE),
                               max_reduction=quantile(reduction, .75, na.rm=TRUE))
p <- ggplot(data=data_fig_SPM_stat, aes(x=region, y=median_reduction, fill=region)) +
     geom_bar(stat="identity") +
     geom_errorbar(aes(ymin=min_reduction, ymax=max_reduction), width=.2,position=position_dodge(.9)) +
     scale_fill_manual(name="Country", 
                       #values = c("#FFDB6D", "#D16103", "#52854C", "#4E84C4", "#293352"),
                       values = c("#FFDB6D", "#C4961A", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"), 
                       labels=c("CHN"="China", "USA"="USA", "IND"="India", "EU"="EU", "World", "BRA"="Brazil", "JPN"="Japan")) +
     xlab('Country') +
     ylab('%') +
     theme_bw() + 
     theme(legend.position="bottom") +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1))
plot(p)
ggsave(file="graphs/p9_3_400dpi.jpg", p, dpi=400)
ggsave(file="graphs/p9_3_800dpi.jpg", p, dpi=800)
