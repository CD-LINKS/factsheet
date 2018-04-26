minmax$ymax=minmax$ymax/1000
minmax$ymin=minmax$ymin/1000
minmax$med=minmax$med/1000
ylim=c(20,80)

# minmax$Category=str_replace_all(minmax$Category,"Carbon budget 1000","Below 2 °C")
# minmax$Category=str_replace_all(minmax$Category,"Carbon budget 400","Below 1.5 °C")
# source("functions/plotstyle.R")

p = ggplot()
p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                          color=Category),size=1.3)}
p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario")
p = p + scale_fill_manual(values=plotstyle(cats),name="Scenario")
if (range & length(unique(dt$Category))==5){
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[1]], stat="identity", aes(x=2030, xend=2030, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[2]], stat="identity", aes(x=2030.5, xend=2030.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[3]], stat="identity", aes(x=2031, xend=2031, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[4]], stat="identity", aes(x=2031.5, xend=2031.5, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
  p = p + geom_segment(data=minmax[period %in% c(2030) & Category %in% unique(minmax$Category)[5]], stat="identity", aes(x=2032, xend=2032, y=ymin, yend=ymax, size=1.5, colour=Category), show.legend=FALSE) 
}
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
p = p + ylab("GtCO2-equiv/year") + xlab("")
p = p + ggplot2::theme_bw() 
p = p + theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18),
              strip.text=element_text(size=18),
              legend.text=element_text(size=18),
              legend.title=element_text(size=18),
              plot.title=element_text(size=18))

ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)
