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
plot_pointrange_multiScen_yr <- function(regs, dt, vars, catsnat, catglob, years, out=cfg$outdir, title="Title",plottitle="", file_pre="pointrange",connect=T,noglobrange=F,increasefont=F,
                                         b.multivar =  F, b.multiyear = F, var.labels = NA, ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F,quantiles=T,minprob=0.1,maxprob=0.9){
  
  
  dt <- dt[ variable %in% vars & period %in% years & Category %in% union(catsnat, catglob) & region %in% regs & !is.na(value)] %>% factor.data.frame()
  
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = union(catsnat, catglob), ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% catglob & region %in% regs]  %>%
    rename(Global = model ) %>% factor.data.frame()
  
  #Only for models that have the region in their spatial aggregation
  regions=all[,list(region=unique(region)),by=c("model")]
  dtg=data.table(dtg)
  for(mod in unique(dtg$model)){
    dtg[model==mod]=dtg[model==mod&region %in% regions[model==mod]$region]
  }
  
  if(quantiles){dtg1 <-dtg[,list(mean=median(value),min=quantile(value,prob=minprob),max=quantile(value,prob=maxprob)),by=c("scenario","Category","Baseline","region","period","Scope","unit","variable")]
  }else{dtg1 <-dtg[,list(mean=median(value),min=min(value),max=max(value)),by=c("scenario","Category","Baseline","region","period","Scope","unit","variable")]}
  
  dtn <- dt[Scope=="national" & variable %in% vars & period %in% years & Category %in% catsnat & region %in% regs] %>%
    rename(National = model ) %>% factor.data.frame()
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtg1$variable) <- var.labels
    levels(dtn$variable) <- var.labels
  }
  
  p = ggplot()
  if(noglobrange){
    p = p 
  }else{
    p = p + geom_pointrange(data=dtg1,aes(x=region,y=mean,ymin=min,ymax=max,fill=Category),  size = 3,fatten=8,shape="-",show.legend = F,color = "grey65") }
  if(b.multivar&b.multiyear){
      p = p + facet_grid(variable~period,scales="free_y")
  }else{
    if(b.multivar){
      p = p + facet_wrap(~ variable, scales="free_y")
    }
    if(b.multiyear){
      p = p + facet_wrap(~ period, scales="fixed")
    }
  }
    if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=Global))}
  if(b.multivar&b.multiyear){
    p = p + geom_point(data=dtn,aes(x=region,y=value,colour=Category, shape=National),position=position_dodge(width=0.7),size = 2,  stroke = 1 )
  }else{
    p = p + geom_point(data=dtn,aes(x=region,y=value,colour=National, shape=Category), size = 3.5,  stroke = 1 )
  }
  #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
  p = p + ylab("") + xlab("")
  if(b.multivar&b.multiyear){
    p = p + scale_color_manual(values=plotstyle(catsnat))
    p = p + scale_shape_manual(values=plotstyle(levels(dtn$National),out="shape"))
  }else{
    p = p + scale_color_manual( values=plotstyle(levels(dtn$National)),
                                labels =  plotstyle(levels(dtn$National), out = "legend") )
    p = p + scale_shape_manual(values=plotstyle(catsnat, out="shape"))
  }
  
  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)
  
  if(b.multivar)  {
    p = p + ggtitle(plottitle)
    if(increasefont){
      p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 18),
                    axis.text.y  = element_text(size = 22),
                    axis.title = element_text(size=18),
                    legend.title = element_text(size=22),
                    legend.text = element_text(size=20),
                    strip.text = element_text(size=22),
                    plot.title=element_text(size=22))}else{
                      p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 14),
                  plot.title = element_text( size = 18) )}
    p = p + theme_bw()
    p = p + guides(colour=guide_legend(override.aes=list(size=1)))
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiNatiScen_",file_pre,cfg$format),p, width=9, height=6, dpi=120)
  }  else   {
    p = p + ggtitle(paste0( var.labels[1]))
    if(increasefont){
      p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 18),
                    axis.text.y  = element_text(size = 22),
                    axis.title = element_text(size=18),
                    legend.title = element_text(size=22),
                    legend.text = element_text(size=20),
                    strip.text = element_text(size=22),
                    plot.title=element_text(size=22))}else{
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 11),
                  plot.title = element_text(hjust = 1, size = 13) ) }
    p = p + theme_bw()
    p = p + guides(colour=guide_legend(override.aes=list(size=1)))
    if(b.multiyear){
      if(increasefont){
        p = p + theme(axis.text.x  = element_text(angle=45, vjust=0.5, hjust = 1, size = 18),
                      axis.text.y  = element_text(size = 22),
                      axis.title = element_text(size=18),
                      legend.title = element_text(size=22),
                      legend.text = element_text(size=20),
                      strip.text = element_text(size=22),
                      plot.title=element_text(size=22))}else{
      p = p + theme(axis.text.x=element_text(angle=45,vjust=0.5))}
      ggsave(file=paste0(out,"/pointrangeMultiReg_MultiNatiScen_",file_pre,cfg$format),p,
             width=10, height=6, dpi=120) 
    }else{
      ggsave(file=paste0(out,"/pointrangeMultiReg_MultiNatiScen_",file_pre,cfg$format),p,
                                 width=6.5, height=6, dpi=120)}
  }
  return(p)
}


#############################################################
####################### plot_pointrange_global###############
#############################################################

#plot function for pointrange (instead of boxplot) - multi-year, one variable
plot_pointrange_multiScen_glob <- function(regs, dt, vars, cats, catsnat, years, out=cfg$outdir, title="Title", file_pre="pointrange",connect=T,ylabel="",
                                         b.multivar =  F, b.multiyear = F, b.multicat = F, b.multireg=F,var.labels = NA, modnames=F, mod.labels=NA, 
                                         ylim=NULL,xlim=NULL,xlog=F,ylog=F,yearlab=T,globpoints=F,natpoints=F,nonreg=F,hist=F,nrow,ncol,quantiles=T,minprob=0.1,maxprob=0.9){
  
  if(hist){ dt$period<-as.numeric(dt$period)
            dt[Category=="Historical"]$period<-years}
  dt <- dt[ variable %in% vars & period %in% years & Category %in% cats & region %in% regs & !is.na(value)] %>% factor.data.frame()
  dt$region <- factor(dt$region, levels = regs, ordered = T )
  dt$Category <- factor(dt$Category, levels = cats, ordered = T )
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  dtg <- dt[Scope=="global" & variable %in% vars & period %in% years & Category %in% cats & region %in% regs]  %>%
    factor.data.frame()
  
  #Only for models that have the region in their spatial aggregation
  regions=all[,list(region=unique(region)),by=c("model")]
  dtg=data.table(dtg)
  for(mod in unique(dtg$model)){
    dtg[model==mod]=dtg[model==mod&region %in% regions[model==mod]$region]
  }

  baselines <-dtg[,list(Baseline=unique(Baseline)),by=c("Category","region","period","Scope","unit","variable")]
  if(quantiles){dtg1 <-dtg[,list(mean=median(value),min=quantile(value,prob=minprob),max=quantile(value,prob=maxprob)),by=c("Category","region","period","Scope","unit","variable")]
  }else{
    dtg1 <-dtg[,list(mean=median(value),min=min(value),max=max(value)),by=c("Category","region","period","Scope","unit","variable")]
  }
  dtg1=merge(dtg1,baselines,by=c("Category","region","period","Scope","unit","variable"))
  
  if(natpoints){dtn <- dt[Scope=="national" & variable %in% vars & period %in% years & Category %in% catsnat & region %in% regs] %>%
    factor.data.frame()}
  
  if (b.multivar){
    levels(dtg$variable) <- var.labels
    levels(dtg1$variable) <- var.labels
    if(natpoints){levels(dtn$variable) <- var.labels}
    
  }
  
  # if (modnames){
  #   levels(dtg$model)<-mod.labels  
  # }
  
  p = ggplot()+ ggplot2::theme_bw()
  if(b.multireg){
    p = p + geom_pointrange(data=dtg1,aes(x=Category,y=mean,ymin=min,ymax=max, colour=variable),size=3,fatten=8,shape="-",stat="identity",position=position_dodge(width=0.7))
    p = p + facet_wrap(~region,scales="free_y",nrow=nrow,ncol=ncol)
  }else{
  if(nonreg){p = p + geom_pointrange(data=dtg1,aes(x=Category,y=mean,ymin=min,ymax=max, colour=Category),size=3,fatten=8,shape="-",stat="identity",position=position_dodge(width=0.7))
  }else{
  p = p + geom_pointrange(data=dtg1,aes(x=region,y=mean,ymin=min,ymax=max, colour=Category),size=3,fatten=8,shape="-",stat="identity",position=position_dodge(width=0.7))
  }}
  
  if(b.multivar){
    p = p + facet_wrap(~ variable, scales="free_y")
  }
  if(b.multiyear){
    p = p + facet_wrap(~ period, scales="fixed")
  }
  if(b.multicat){
    p = p + facet_grid(variable ~ Category, scales="free")
  }
  
  if(globpoints&nonreg){p = p + geom_point(data=dtg,aes(x=Category,y=value,shape=model,colour=Category,group=Category),size=3)#,position=position_dodge(width=c(0.5,0.5,0.5))
  }else{
    if(globpoints&b.multivar){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=model,colour=Category,group=interaction(Category,variable)),size=3,position=position_dodge(width=c(0.7,0.7,0.7)))
    }else{
    if(globpoints){  p = p + geom_point(data=dtg,aes(x=region,y=value,shape=model,colour=Category,group=Category),size=3,position=position_dodge(width=c(0.7,0.7,0.7)))
    }}}
  
  if(natpoints&nonreg){p = p + geom_point(data=dtn,aes(x=Category,y=value,shape=model, colour=Category,group=Category,stroke=1.5), size = 3,  position=position_dodge(width=c(0.7,0.7,0.7)))
  }else{
      if(natpoints&b.multivar){p = p + geom_point(data=dtn,aes(x=region,y=value,shape=model, colour=Category, group=interaction(Category,region,variable),stroke=1.5), size = 3,  position=position_dodge(width=c(0.7,0.7,0.7)))
      }else{
      if(natpoints){ p = p + geom_point(data=dtn,aes(x=region,y=value,shape=model, colour=Category,group=interaction(Category,region),stroke=1.5), size = 3,  position=position_dodge(width=c(0.7,0.7,0.7)))
      }}}
    
    #  p = p + ylab(paste0(dtg$variable[1], " [", dtg$unit[1],"]") ) + xlab("")
    p = p + ylab(ylabel) + xlab("")
    if(b.multireg){
     p = p + scale_color_manual(values=plotstyle(vars),labels=plotstyle(vars,out="legend")) 
    }else{
    p = p + scale_color_manual( values=plotstyle(cats),
                              labels =  plotstyle(cats, out = "legend"),name="Scenario" )}
    if(modnames){p = p + scale_shape_manual(values=cfg$man_shapes,labels=mod.labels,name="Model")}else{
      p = p + scale_shape_manual(values=cfg$man_shapes,name="Model")}
  #p = p + scale_shape_manual(values=plotstyle(cats, out="shape"))
  
  if (!is.null(ylim))
    p = p + coord_cartesian(ylim = ylim)
  
  if(b.multicat){
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 18),
                  axis.text.y  = element_text(size = 18),
                  axis.title = element_text(size=18),
                  legend.title = element_text(size=18),
                  legend.text = element_text(size=18),
                  strip.text = element_text(size=18))
    p = p + guides(colour=guide_legend(override.aes=list(size=1)))
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p, width=12, height=8, dpi=120)
  } else {
  if(b.multivar)  {
    
    p = p + theme(axis.text.x  = element_text(size = 18), #angle=90, vjust=0.5, hjust = 1, 
                  axis.text.y  = element_text(size = 18),
                  plot.title = element_text( size = 20),
                  axis.title = element_text(size=18),
                  legend.title = element_text(size=18),
                  legend.text = element_text(size=18),
                  strip.text = element_text(size=18))
    p = p + guides(colour=guide_legend(override.aes=list(size=1)))
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p, width=12, height=8, dpi=120)
  }  else   {
    if(b.multireg){
      p = p + theme(axis.text.x  = element_text(size = 18,angle=90), #angle=90, vjust=0.5, hjust = 1, 
                    axis.text.y  = element_text(size = 18),
                    plot.title = element_text( size = 20),
                    axis.title = element_text(size=18),
                    legend.title = element_text(size=18),
                    legend.text = element_text(size=18),
                    strip.text = element_text(size=18))
      p = p + guides(colour=guide_legend(override.aes=list(size=1)))
      ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p, width=12, height=8, dpi=120)
    }else{
    #p = p + ggtitle(paste0( var.labels[1]))
    p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = 18),
                  axis.text.y  = element_text(size = 18),
                  plot.title = element_text(hjust = 1, size = 20),
                  axis.title = element_text(size=18),
                  legend.title = element_text(size=18),
                  legend.text = element_text(size=18),
                  strip.text = element_text(size=18))
    p = p + guides(colour=guide_legend(override.aes=list(size=1)))
    ggsave(file=paste0(out,"/pointrangeMultiReg_MultiScen_",file_pre,cfg$format),p,
           width=12, height=12, dpi=120)
  }}}
  return(p)
}

#############################################################
####################### plot_stackbar_regions ########################
#############################################################

plot_stackbar_regions <- function(regs, dt, vars, cats, per, out=cfg$outdir, lab="ylab",title=F,Title="Title", file_pre="stackbar",ylim=NA,ybreaks=NULL,hist=F,medvar,med=F,CO2eq=F,quantiles=T,minprob=0.1,maxprob=0.9,colour=F){
  
  if(hist){dt[Category=="Historical"]$period<-per}
  
  if(CO2eq){
    if("Emissions|CH4"%in%vars){
      dt[variable=="Emissions|CH4"]$value<-dt[variable=="Emissions|CH4"]$value*25
      dt[variable=="Emissions|CH4"]$unit<-"MtCO2eq/yr"}
    if("Emissions|N2O"%in%vars){
      dt[variable=="Emissions|N2O"]$value<-dt[variable=="Emissions|N2O"]$value*298/1000
      dt[variable=="Emissions|N2O"]$unit<-"MtCO2eq/yr"}
  }
  
  dta <-filter(dt, region %in% regs, Category%in% cats, variable%in% vars, period %in% per,Scope=="global") #[2:length(vars)]
  dta <- factor.data.frame(dta)
  #dataframe for stack bar plots: use first scenario of each category-model combination, if multiple exists
  for (cat in cats){
    for (mod in unique(dta[dta$Category==cat,]$model)){
      if(length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario))==1){
      } else {
        dta <- dta[!(dta$scenario %in% unique(dta[dta$Category==cat & dta$model == mod,]$scenario)[seq(2,length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario)))] & dta$Category==cat & dta$model == mod),]
      }
    }
  }

  
  #Only for models that have the region in their spatial aggregation
  regions=all[,list(region=unique(region)),by=c("model")]
  dta=data.table(dta)
  for(mod in unique(dta$model)){
    dta[model==mod]=dta[model==mod&region %in% regions[model==mod]$region]
  }

  #Calculate model average per region
  dta=dta[,list(mean(value)),by=c("Category","variable","region","period","Scope","unit")]
  setnames(dta,"V1","value")
  
  #Calculate rest of world 
  dta=spread(dta,region,value,fill=0)
  if(c("Bunkers")%in%regs){dta=dta%>%mutate(RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` - `Bunkers`)}
  else{dta=dta%>%mutate(RoW = `World` - `BRA` - `CHN` - `IND` - `EU` - `JPN` - `USA` - `RUS` )}
  if(c("Bunkers")%in%regs){dta=gather(dta,region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS` , `Bunkers`) }
  else{dta=gather(dta,region,value,`RoW`, `World`, `BRA`, `CHN`, `IND`, `EU`, `JPN`, `USA`, `RUS`) }
  dta=data.table(dta)
  dta <- filter(dta,!region %in% c("World"))
    
  #build data frame for overlaid errorbar showing model range for world total
  dtl <- filter(dt, region %in% c("World"), Category%in% cats, variable%in% vars, period %in% per)
  dtl=data.table(dtl)
  if(quantiles){dtl=dtl[,list(min=quantile(value,prob=minprob),max=quantile(value,prob=maxprob),median=median(value)),
               by=c("Category","variable","region","period","Scope","unit")]
  }else{dtl=dtl[,list(min=min(value),max=max(value),median=median(value)),
                by=c("Category","variable","region","period","Scope","unit")]}
  
  dta$Category <- factor(dta$Category, levels = cats, ordered = T )
  dta$region <- factor(dta$region, levels = regs, ordered = T )
  dtl$Category <- factor(dtl$Category, levels = cats, ordered = T )
  
  p = ggplot() + ggplot2::theme_bw()
  p = p + geom_bar(data=dta,aes(Category, value, group = interaction(variable, region, Category), fill = region), stat="identity", position="stack")
  p = p + geom_errorbar(data=dtl,aes(Category, ymin=min,ymax=max, group = interaction(variable, region, Category)),size=0.3)
  if(med){p = p + geom_point(data=dtl[variable%in%medvar],aes(Category,y=median,group=interaction(variable,region,Category)))}
  p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1,size=18),
                axis.text.y  = element_text(size = 18),
                plot.title = element_text(size = 20),
                axis.title = element_text(size=16),
                legend.title = element_text(size=18),
                legend.text = element_text(size=18))
  p = p + ylab(lab) + xlab("")
  if(title){p = p + ggtitle(Title)}
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if(colour){p = p + scale_fill_manual(values=plotstyle(regs), labels=plotstyle(regs,out="legend"),name="Region")}
  else{p = p + scale_fill_brewer(palette="Spectral")}
  ggsave(file=paste0(out,"/",file_pre,"_",per,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_stackbar_ghg ########################
#############################################################

plot_stackbar_ghg <- function(regs, dt, vars, cats, catsnat, per, out=cfg$outdir, lab="ylab",title=F,Title="Title",file_pre="stackbar",ylim=NA,ybreaks=NA,hist=F,labels=F, var.labels=NA, TotalEmis_var = "Emissions|Kyoto Gases", natpoints, error_bar=F,quantiles=T,minprob=0.1,maxprob=0.9,total=F){
  
  if(hist){dt[Category=="Historical"]$period<-per}
  
  #dta <-filter(dt, region %in% regs, Category%in% cats, variable%in% vars, period %in% per,Scope=="global",!variable=="Emissions|Kyoto Gases") #[2:length(vars)]
  dta <-filter(dt, region %in% regs, Category%in% cats, variable%in% vars, period %in% per,Scope=="global",!variable==TotalEmis_var) #[2:length(vars)]
  dta <- factor.data.frame(dta)
  #dataframe for stack bar plots: use first scenario of each category-model combination, if multiple exists
  for (cat in cats){
    for (mod in unique(dta[dta$Category==cat,]$model)){
      if(length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario))==1){
      } else {
        dta <- dta[!(dta$scenario %in% unique(dta[dta$Category==cat & dta$model == mod,]$scenario)[seq(2,length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario)))] & dta$Category==cat & dta$model == mod),]
      }
    }
  }
  
  dta=data.table(dta)
  dta[variable=="Emissions|CH4"]$value<-dta[variable=="Emissions|CH4"]$value*25
  dta[variable=="Emissions|CH4"]$unit<-"MtCO2eq/yr"
  dta[variable=="Emissions|N2O"]$value<-dta[variable=="Emissions|N2O"]$value*298/1000
  dta[variable=="Emissions|N2O"]$unit<-"MtCO2eq/yr"
  
  # Calculate median, only for models that have the region in their spatial aggregation
  regions=all[,list(region=unique(region)),by=c("model")]
  dta=data.table(dta)
  for(mod in unique(dta[!region=="RoW"]$model)){
    dta[model==mod&!region=="RoW"]=dta[model==mod&region %in% regions[model==mod]$region]
  }
  
  dta=dta[,list(mean(value)),by=c("Category","variable","region","period","Scope","unit")]
  setnames(dta,"V1","value")
  dta <- filter(dta,!region %in% c("World"))
  
  #build data frame for overlaid errorbar showing model range for GHG total
  #dtl <- filter(dt, region %in% regs, Category%in% cats, variable%in% c("Emissions|Kyoto Gases"), period %in% per,Scope=="global")
  dtl <- filter(dt, region %in% regs, Category%in% cats, variable%in% c("Emissions|Kyoto Gases"), period %in% per,Scope=="global")
  dtl=data.table(dtl)
  if(quantiles){dtl=dtl[,list(min=quantile(value,prob=minprob,na.rm=T),max=quantile(value,prob=maxprob,na.rm=T),med=median(value,na.rm=T)),by=c("Category","variable","region","period","Scope","unit")]
  }else{dtl=dtl[,list(min=min(value),max=max(value),med=median(value)),by=c("Category","variable","region","period","Scope","unit")]}

  #if(natpoints){dtn <- filter(dt, region %in% regs, Category%in% catsnat, variable%in% c("Emissions|Kyoto Gases"), period %in% per,Scope=="national")}
  if(natpoints){dtn <- filter(dt, region %in% regs, Category%in% catsnat, variable==TotalEmis_var, period %in% per,Scope=="national")}

  dta$Category <- factor(dta$Category, levels = cats, ordered = T )
  dtl$Category <- factor(dtl$Category, levels = cats, ordered = T )
  if(natpoints){dtn$Category <- factor(dtn$Category, levels = catsnat, ordered = T )}
  dta$variable <- factor(dta$variable, levels = vars, ordered = T)

  p = ggplot() + ggplot2::theme_bw()
  p = p + geom_bar(data=dta,aes(Category, value, group = interaction(variable, region, Category), fill = variable), stat="identity", position="stack")
  if(error_bar){ p = p + geom_errorbar(data=dtl,aes(Category, ymin=min,ymax=max, group = interaction(variable, region, Category)),size=0.3)
  }
  if(natpoints){
  p = p + geom_point(data=dtn,aes(x=Category,y=value,shape=model, group=interaction(Category,region,variable)), size = 3,show.legend = F)
  }
  if(total){
    p = p + geom_point(data=dtl,aes(Category, y=med, group = interaction(variable, region, Category)),size=3)  
  }
  p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1,size=22),
                axis.text.y  = element_text(size = 22),
                plot.title = element_text(size = 24),
                axis.title = element_text(size=22),
                legend.title = element_text(size=24),
                legend.text = element_text(size=22))
  p = p + ylab(lab) + xlab("")
  if(title){p = p + ggtitle(Title)}
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if(labels){p = p + scale_fill_brewer(palette="Spectral",labels=var.labels,name="Variable")}else{p = p + scale_fill_brewer(palette="Spectral",name="Variable")}
  #p = p + scale_fill_manual(values=plotstyle(regs), labels=plotstyle(regs,out="legend"), name=strsplit(regs[1], "|", fixed=T)[[1]][1])
  ggsave(file=paste0(out,"/",file_pre,"_",per,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_stackbar_diff ########################
#############################################################

plot_stackbar_diff <- function(regs, dt, vars, cats, per, out=cfg$outdir, lab="ylab",title=F,Title="Title",file_pre="stackbar",ylim=NA,ybreaks=NA,labels=F, scen.labels=NA,b.multiyear=T){
  
  dta <-filter(dt, region %in% regs, Category%in% cats, variable%in% vars, period %in% per,Scope=="global")
  dta <- factor.data.frame(dta)
  #dataframe for stack bar plots: use first scenario of each category-model combination, if multiple exists
  for (cat in cats){
    for (mod in unique(dta[dta$Category==cat,]$model)){
      if(length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario))==1){
      } else {
        dta <- dta[!(dta$scenario %in% unique(dta[dta$Category==cat & dta$model == mod,]$scenario)[seq(2,length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario)))] & dta$Category==cat & dta$model == mod),]
      }
    }
  }
  
  dta=data.table(dta)
  dta=na.omit(dta)
  # Calculate median, only for models that have the region in their spatial aggregation
  regions=all[,list(region=unique(region)),by=c("model")]
  dta=data.table(dta)
  for(mod in unique(dta[!region=="RoW"]$model)){
    dta[model==mod&!region=="RoW"]=dta[model==mod&region %in% regions[model==mod]$region]
  }
  dta=dta[,list(mean(value)),by=c("Category","variable","region","period","Scope","unit")]
  setnames(dta,"V1","value")
  
  #calculate difference NDC-2C
  dta=spread(dta,Category,value)
  dta=dta%>%mutate(diff=`2°C`-`NDC`)
  dta=data.table(gather(dta,Category,value,c("2°C","NDC","diff")))
  dta=dta[Category%in%c("NDC","diff")]
  
  p = ggplot() + ggplot2::theme_bw()
  p = p + geom_bar(data=dta,aes(x=region, y=value, group = interaction(variable, region), fill = Category), stat="identity", position="stack")
  if(b.multiyear){p = p + facet_wrap(~ period, scales="free_x")}
  p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1,size=22),
                axis.text.y  = element_text(size = 22),
                plot.title = element_text(size = 24),
                axis.title = element_text(size=22),
                legend.title = element_text(size=24),
                legend.text = element_text(size=22))
  p = p + ylab(lab) + xlab("")
  if(title){p = p + ggtitle(Title)}
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if(labels){p = p + scale_fill_brewer(palette="Paired",labels=scen.labels,name="Scenario")}else{p = p + scale_fill_brewer(palette="Paired",name="Scenario")}
  ggsave(file=paste0(out,"/",file_pre,"_",cfg$format),p, width=10, height=8, dpi=120)
  return(p)
}
