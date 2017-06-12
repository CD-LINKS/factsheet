theme_set(ggplot2::theme_bw(base_size = 15))

source("functions/plotstyle.R") # load plotstyle() function that provides colors etc. for entities


#plot function for boxplots
plot_boxplot <- function(regs, dt, vars, cats, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multicat = F, b.multivar =  F, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T,globpoints=T){
  
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
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))
  p = p + scale_shape_manual(values=cfg$man_shapes)}
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National), size = 3)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if (b.multicat)
  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    p = p + ggtitle(paste0(dtg$variable[1], " [", dtg$unit[1],"] - ",as.character(year)))
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,"_refpol",cfg$format),p, width=9, height=6, dpi=120)
  }  else if(b.multivar)  {
    p = p + ggtitle(paste0("'",cats[1],"'"," - ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"] - ",cats[1]," - ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(size = 13) ) #hjust = 1, 
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}

# Boxplot with multi-year option
plot_boxplot_yr <- function(regs, dt, vars, cats, years, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                            b.multicat = F, b.multivar =  F, b.multiyear=T, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T,globpoints=T){
  
  dt <- dt[ variable %in% vars & period %in% years & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% cats & region %in% regs]  %>%
    rename(Global = model )
  dtn <- dt[Scope=="national" & variable %in% vars & period %in% years & Category %in% cats & region %in% regs] %>%
    rename(National = model )
  
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=region,y=value),outlier.size = 0)
  if (b.multicat){
    p = p + facet_wrap(~ Category)
  } else if(b.multiyear){
    p = p + facet_wrap(variable~ period, scales="free_x")
  }
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))
  p = p + scale_shape_manual(values=cfg$man_shapes)}
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National), size = 3)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if (b.multicat)
  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    p = p + ggtitle(paste0(dtg$variable[1], " [", dtg$unit[1],"] - ",as.character(year)))
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,"_refpol",cfg$format),p, width=9, height=6, dpi=120)
  }  else if(b.multivar)  {
    p = p + ggtitle(paste0("'",cats[1],"'"," - ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"] - ",cats[1]))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot_",file_pre,cfg$format),p,
           width=8, height=6, dpi=120)
  }
  return(p)
}


#plot function for boxplots
plot_boxplot_multiScenNat <- function(regs, dt, vars, catsnat, catglob, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multivar =  F, var.labels = NA, ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F){


  dt <- dt[ variable %in% vars & period == year & Category %in% union(catsnat, catglob) & region %in% regs & !is.na(value)] %>% factor.data.frame()

  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = union(catsnat, catglob), ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )


  dtg <- dt[Scope=="global" & variable %in% vars & period == year & Category %in% catglob & region %in% regs]  %>%
    rename(Global = model ) %>% factor.data.frame()
  dtn <- dt[Scope=="national" & variable %in% vars & period == year & Category %in% catsnat & region %in% regs] %>%
    rename(National = model ) %>% factor.data.frame()


  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }

  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=region,y=value), coef = 1e4, color = "grey65", size = 1.)
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))}
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National, shape=Category), size = 3,  stroke = 1 )
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")

  p = p + scale_color_manual( values=plotstyle(levels(dtn$National)),
                              labels =  plotstyle(levels(dtn$National), out = "legend") )
  p = p + scale_shape_manual(values=plotstyle(catsnat, out="shape"))

  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)

  if(b.multivar)  {

    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/boxplotMultiReg_MultiNatiScen_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( var.labels[1], " -- ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/boxplotMultiReg_MultiNatiScen_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


#plot function for boxplots - multi-year, one variable
plot_boxplot_multiScenNat_yr <- function(regs, dt, vars, catsnat, catglob, years, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                                      b.multivar =  F, b.multiyear = T, var.labels = NA, ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F){
  
  
  dt <- dt[ variable %in% vars & period %in% years & Category %in% union(catsnat, catglob) & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = union(catsnat, catglob), ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% catglob & region %in% regs]  %>%
    rename(Global = model ) %>% factor.data.frame()
  dtn <- dt[Scope=="national" & variable %in% vars & period %in% years & Category %in% catsnat & region %in% regs] %>%
    rename(National = model ) %>% factor.data.frame()
  
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_boxplot(data=dtg,aes(x=region,y=value), coef = 1e4, color = "grey65", size = 1.)
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  if(b.multiyear){
    p = p + facet_wrap(~ period, scales="fixed")
  }
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))}
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National, shape=Category), size = 3,  stroke = 1 )
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  
  p = p + scale_color_manual( values=plotstyle(levels(dtn$National)),
                              labels =  plotstyle(levels(dtn$National), out = "legend") )
  p = p + scale_shape_manual(values=plotstyle(catsnat, out="shape"))
  
  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)
  
  if(b.multivar)  {
    
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/boxplotMultiReg_MultiNatiScen_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( var.labels[1]))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/boxplotMultiReg_MultiNatiScen_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


#############################################################
####################### plot_stackbar ########################
#############################################################

plot_stackbar <- function(regs, dt, vars, cats, per, out=cfg$outdir, lab="Title", file_pre="stackbar",ylim=NA,ybreaks=NA){
  #dataframe for stack bar plots: use first scenario of each category-model combination, if multiple exists
  # dta <- dt[region %in% regs & Category%in% cats & variable%in% vars[2:length(vars)] & period %in% per]
  dta <-filter(dt, region %in% regs, Category%in% cats, variable%in% vars[2:length(vars)], period %in% per)
  dta <- factor.data.frame(dta)
  for (cat in cats){
    for (mod in unique(dta[dta$Category==cat,]$model)){
      if(length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario))==1){
      } else {
        dta <- dta[!(dta$scenario %in% unique(dta[dta$Category==cat & dta$model == mod,]$scenario)[seq(2,length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario)))] & dta$Category==cat & dta$model == mod),]
      }
    }
  }
  #build data frame for overlaid point plots showing totals for all scenarios in each category-model group
  dtl <- filter(dt, region %in% regs, Category%in% cats, variable%in% vars[1], period %in% per)
      # dt[region==regs & Category%in% cats & variable%in% vars[1] &  period %in% per]
  dtl <- factor.data.frame(dtl)
  p = ggplot() + ggplot2::theme_bw()
  p = p + geom_bar(data=dta,aes(model, value, group = interaction(variable, region, Category), fill = variable), stat="identity", position="stack")
  p = p + geom_point(data=dtl,aes(model, value, group = interaction(variable, region, Category)),shape=5,size=2)
  p = p + geom_point(data=dtl,aes(model, value, group = interaction(variable, region, Category)),shape=5, colour = "#ffffff",size=1)
  p = p + facet_grid(Category ~ region)
#  p = p + scale_x_discrete(breaks=plotstyle(unique(dta$model),out="legend"))

  p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1))
  p = p + ylab(lab) + xlab("Models")
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  p = p + scale_fill_manual(values=plotstyle(vars), labels=plotstyle(vars,out="legend"), name=strsplit(vars[1], "|", fixed=T)[[1]][1])
  ggsave(file=paste0(out,"/",file_pre,"_",per,cfg$format),p, width=7, height=8, dpi=120)
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
  p = p + scale_shape_manual(values=cfg$man_shapes)
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

#for peak year plot
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
  p = p + scale_shape_manual(values=cfg$man_shapes)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if (b.multicat)
  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot2_",file_pre,"_refpol",cfg$format),p, width=9, height=6, dpi=120)
  }  else if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/multireg_boxplot2_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    "))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot2_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


plot_boxplot3 <- function(regs, dt, vars, cats, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                         b.multivar =  F, var.labels = NA, ylim=F,xlim=NA,xlog=F,ylog=F,yearlab=T){
  
  if ("Historical" %in% cats) {
    hist=dt[ variable %in% vars & Category=="Historical" & region %in% regs & !is.na(value)]
    hist$period <- year
    dt=rbind(hist,dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)]) 
    dt %>% factor.data.frame()
  } else {
  dt <- dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  }
  
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
  p = p + geom_point(data=dtg,aes(x=Category,y=value,shape=Global,colour=Global))
  p = p + geom_point(data=dtn,aes(x=Category,y=value,colour=National), size = 3)
  p = p + scale_shape_manual(values=cfg$man_shapes)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if(ylim){p=p+ylim(c(0,NA))}
  if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    p = p + ggtitle(paste0( dtg$region, "--" , as.character(year)))
    ggsave(file=paste0(out,"/multireg_boxplot3_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot3_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}

plot_boxplot4 <- function(regs, dt, vars, cats, year = 2050, out=cfg$outdir, title="Title", file_pre="boxplot",connect=T,
                          b.multivar =  F, var.labels = NA, ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T){
  
  
  if ("Historical" %in% cats) {
    hist=dt[ variable %in% vars & Category=="Historical" & region %in% regs & !is.na(value)]
    hist$period <- year
    dt=rbind(hist,dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)]) 
    dt %>% factor.data.frame()
  } else {
  dt <- dt[ variable %in% vars & period == year & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()}
  
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
    p = p + facet_grid(variable ~ region, scales="free_y")
  }
  p = p + geom_point(data=dtg,aes(x=Category,y=value,shape=Global,colour=Global))
  p = p + geom_point(data=dtn,aes(x=Category,y=value,colour=National), size = 3)
  p = p + scale_shape_manual(values=cfg$man_shapes)
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  p=p+ylim(ylim)
  if(b.multivar)  {
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    p = p + ggtitle(paste0( dtg$region, "--" , as.character(year)))
    ggsave(file=paste0(out,"/multireg_boxplot4_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( dtg$variable[1], " [", dtg$unit[1],"]   --    ", as.character(year)))
    p = p + theme(axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/multireg_boxplot4_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}

#############################################################
####################### plot_pointrange #####################
#############################################################

#plot function for pointrange (instead of boxplot) - multi-year, one variable
plot_pointrange_multiScen_yr <- function(regs, dt, vars, catsnat, catglob, years, out=cfg$outdir, title="Title", file_pre="pointrange",connect=T,
                                         b.multivar =  F, b.multiyear = T, var.labels = NA, ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F){
  
  
  dt <- dt[ variable %in% vars & period %in% years & Category %in% union(catsnat, catglob) & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = union(catsnat, catglob), ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% catglob & region %in% regs]  %>%
    rename(Global = model ) %>% factor.data.frame()
  dtg1 <-dtg[,list(mean=mean(value),min=min(value),max=max(value)),by=c("scenario","Category","Baseline","region","period","Scope","unit","variable")]
  dtn <- dt[Scope=="national" & variable %in% vars & period %in% years & Category %in% catsnat & region %in% regs] %>%
    rename(National = model ) %>% factor.data.frame()
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  p = p + geom_pointrange(data=dtg1,aes(x=region,y=mean,ymin=min,ymax=max,colour=Category),  size = 1.) #color = "grey65",
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  if(b.multiyear){
    p = p + facet_wrap(~ period, scales="fixed")
  }
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))}
  p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National, shape=Category), size = 3,  stroke = 1 )
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  
  p = p + scale_color_manual( values=plotstyle(levels(dtn$National)),
                              labels =  plotstyle(levels(dtn$National), out = "legend") )
  p = p + scale_shape_manual(values=plotstyle(catsnat, out="shape"))
  
  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)
  
  if(b.multivar)  {
    
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiNatiScen_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( var.labels[1]))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiNatiScen_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}


#############################################################
####################### plot_pointrange_global###############
#############################################################

#plot function for pointrange (instead of boxplot) - multi-year, one variable
plot_pointrange_multiScen_glob <- function(regs, dt, vars, cats, years, out=cfg$outdir, title="Title", file_pre="pointrange",connect=T,ylabel="",
                                         b.multivar =  F, b.multiyear = T, var.labels = NA, ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F,nonreg=F,hist=F){
  
  if(hist){dt[Category=="Historical"]$period<-years}
  dt <- dt[ variable %in% vars & period %in% years & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% cats & region %in% regs]  %>%
    rename(Global = model ) %>% factor.data.frame()
  dtg1 <-dtg[,list(mean=mean(value),min=min(value),max=max(value)),by=c("Category","Baseline","region","period","Scope","unit","variable")]
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
  }
  
  p = ggplot()
  if(nonreg){p = p + geom_pointrange(data=dtg1,aes(x=Category,y=mean,ymin=min,ymax=max, colour=Category),stat="identity",position=position_dodge(width=0.5))
  }else{
  p = p + geom_pointrange(data=dtg1,aes(x=region,y=mean,ymin=min,ymax=max, colour=Category),stat="identity",position=position_dodge(width=0.5))
  }
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  if(b.multiyear){
    p = p + facet_wrap(~ period, scales="fixed")
  }
  if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))}
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab(ylabel) + xlab("")
  
  p = p + scale_color_manual( values=plotstyle(cats),
                              labels =  plotstyle(cats, out = "legend") )
  #p = p + scale_shape_manual(values=plotstyle(cats, out="shape"))
  
  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)
  
  if(b.multivar)  {
    
    p = p + theme(axis.text.x  = element_text(size = 14), #angle=90, vjust=0.5, hjust = 1, 
                  plot.title = element_text( size = 18) )
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( var.labels[1]))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) )
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p,
           width=6.5, height=6, dpi=120)
  }
  return(p)
}

