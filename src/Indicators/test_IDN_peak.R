tmp<-filter(all_cd_links, region=="RUS", scenario%in%c('2C_50'), variable == "Emissions|Kyoto Gases")
ggplot(data=tmp) + geom_line(aes(x=year, y=value, colour=model)) + theme_bw() + ylim(0, NA) + xlim(2010, 2100)

ggplot(data=filter(d_cd_links_peak_zero_inclhist_stat, region=="IDN", statistic=="median")) +
   geom_line(aes(x=year, y=value, colour=scenario)) +
   theme_bw() + ylim(0, NA) 
  

x<-inner_join(d_cd_links_peak_zero, dx, by=c('scenario', 'model', 'region', 'variable'))
x<-mutate(x, diff=value.x-value.y)
x1 <- filter(x, diff != 0)
x2 <- filter(x, diff==0)