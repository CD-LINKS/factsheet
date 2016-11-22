theme_set(ggplot2::theme_bw(base_size = 15))

#plot function for boxplots
plot_boxplot <- function(regs, dt, vars, cats, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multicat = F, b.multivar =  F, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T){
  
  dt <- dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period == year & Category %in% cats & region %in% regs]  %>%
    rename(Global = model )
  dtn <- dt[Scope=="national" & variable %in% vars & period == year & Category %in% cats & region %in% regs] %>%
    rename(National = model )
  
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=region,y=value))
  if (b.multicat){
    p = p + facet_wrap(~ Category)
  } else if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National), size = 3)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if (b.multicat)
  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,"_refpol",cfg$format),p, width=9, height=6, dpi=120)
  }  else if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


#plot function for scatter plots


plot_scatter_xcut <- function(reg, dt, vars_to_spread, cats, out=cfg$outdir, title="Title", file_pre="scatter",connect=T,ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T) {
  
  if (length(vars_to_spread) != 2) stop("Scatter plot requires exactly two variables")
  #create strings with axis units for axis labels
  unitx <- paste0("(",unique(dt[variable==vars_to_spread["x"]]$unit),")")
  unity <- paste0("(",unique(dt[variable==vars_to_spread["y"]]$unit),")")
  # remove unit column because it has different entrie for the two vars_to_spread
  # and causes spread to replace some of the existing values with NA
  
  dt <- subset(dt[region %in% reg & Category%in% cats & variable %in% vars_to_spread],select=-unit)
  dt[variable == vars_to_spread["x"] ]$variable = "x"
  dt[variable == vars_to_spread["y"] ]$variable = "y"
  dt <- spread(dt,variable,value)
  
  
  p = ggplot()
  #normal points for global models
  p = p + geom_point(data=dt[Scope=="global"],aes(x=x,y=y,color=Scope,shape=model))
  #big points for national models
  p = p + geom_point(data=dt[Scope=="national"],aes(x=x,y=y,color=Scope,shape=model),size=4,show.legend = FALSE)
  #label for some years
  #optional: connection lines for each model-scenario
  if (connect){p = p + geom_path(data=dt,aes(x=x,y=y,color=Scope,shape=model,linetype=Scope,
                                             group=interaction(scenario,model),size=Scope))}
  if(yearlab){p = p + geom_dl(data=dt[period %in% c(2010,2030,2050) & Scope == "national"],aes(x=x,y=y,label=period),
                              method=list( cex = 0.9, offset=1, hjust=1, vjust = 0))}
  p = p + scale_size_manual(values=c("national"=2, "global"=.2))
  p = p + scale_colour_manual(values=c("national" = "#119966", "global" = "#000000"))
  p = p + scale_linetype_manual(values=c("national"="solid", "global"="dashed"))
  if (length(reg) >1){p = p + facet_wrap( ~ region,ncol=2)}
  p = p + xlab("CO2 Price [US$/tCO2]") + ylab("CO2 Abatement [%]")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  if (ylog){p = p + scale_y_log10(limits=ylim)} #y-axis logarithmic
  if (xlog){p = p + scale_x_log10(limits=xlim)} #x-axis logarithmic
  p = p + theme(legend.position = "bottom") + ggplot2::theme_bw(base_size = 15)
  ggsave(file=paste0(out,"/",file_pre,"_",reg[1],cfg$format),p, width=8, height=6, dpi=120)
  return(p)
}


plot_boxplot2 <- function(regs, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multicat = F, b.multivar =  F, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T){
  
  dt <- dt[ variable %in% vars & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & Category %in% cats & region %in% regs]  %>%
    rename(Global = model )
  dtn <- dt[Scope=="national" & variable %in% vars & Category %in% cats & region %in% regs] %>%
    rename(National = model )
  
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=region,y=value))
  if (b.multicat){
    p = p + facet_wrap(~ Category)
  } else if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National), size = 3)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if (b.multicat)
  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,"_refpol",cfg$format),p, width=9, height=6, dpi=120)
  }  else if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    "))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


plot_boxplot3 <- function(regs, dt, vars, cats, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multivar =  F, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T){
  
  dt <- dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period == year & Category %in% cats & region %in% regs]  %>%
    rename(Global = model )
  dtn <- dt[Scope=="national" & variable %in% vars & period == year & Category %in% cats & region %in% regs] %>%
    rename(National = model )
  
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=Category,y=value))
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  p = p + geom_point(data=dtg,aes(x=Category,y=value,shape=Global))
  p = p + geom_point(data=dtn,aes(x=Category,y=value,colour=National), size = 3)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    p = p + ggtitle(paste0( dtg$region, "--" , as.character(year)))
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}