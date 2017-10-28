plot_lineNationalScens <- function(reg, dt, vars, scensnat, scensglob, out=cfg$outdir, title="", ylab ="", file_pre="",ylim=NA,xlim=c(2005,2050)){
  #select data

  dt$period = as.numeric(dt$period)

  dtn <- filter(dt, region == reg, variable %in% vars, Scope == "national",scenario %in% scensnat) %>%
    factor.data.frame()
  dtg <- filter(dt, region == reg, variable %in% vars, Scope == "global", scenario %in% scensglob) %>%
    factor.data.frame() %>% as.data.table()

  dtn$scenario <- factor(dtn$scenario, levels = scensnat)

  minmax=dtg[,list(ymax=max(value),ymin=min(value)),by=c('region','period','scenario','variable')]
  minmax=minmax[period %in% c(2005, seq (2010, 2050, by= 10))]

  minmax<-minmax[order(region, scenario, period),]



  p = ggplot()
  p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax, fill=scenario),alpha=.2)
  p = p + geom_path(data=dtg,aes(x=period,y=value,group=paste(model,scenario)), color = "grey75", size = 0.3, show.legend = F )
  p = p + geom_path(data=dtn,aes(x=period,y=value,linetype=model, color = scenario, group=paste(model,scenario)), size = 1.5)
  p = p + scale_colour_manual(values=plotstyle(union(scensnat, scensglob)) )
  p = p + scale_linetype_manual(values = c("solid", "dashed"),
    labels =  plotstyle(as.character(unique(dtn$model)), out = "legend"))
    # p = p + scale_colour_manual(values=plotstyle(scensnat) )
  p = p + scale_fill_manual(values=plotstyle(as.character(scensglob)), name = "Global Models" )

#  p = p + geom_path(data=dt[Scope=="national"],aes(x=period,y=value,color=model,group=paste(model,scenario),size=Scope))
  #p = p + ylab(paste("Carbon Price"))#add unit (different for each facet grid plot...)
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  #p = p + theme(strip.text.y=element_text(angle=45))
  p = p + ylab(ylab) + xlab("")
  p = p + ggtitle(title) + ggplot2::theme_bw(base_size = 11)
  p = p + theme(legend.text=element_text(size=16),legend.title=element_text(size=18))
  ggsave(file=paste0(out,"/NatScens_",file_pre,"_",reg,cfg$format),p, width=13, height=11, dpi=240, units  = "cm")
  return(p)

  }
