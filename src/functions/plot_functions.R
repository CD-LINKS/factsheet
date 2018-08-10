#############################################################
################ Definition of plot functions ###############
#############################################################

theme_set(ggplot2::theme_bw(base_size = 15))
source("functions/plotstyle.R") # load plotstyle() function that provides colors etc. for entities

#############################################################
####################### plot_area ########################
#############################################################

plot_area <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", file_pre="area",ylim=NA,ybreaks=NA,xlim=NA,xbreaks=c(2010,2030,2050),scentext=T){
  #dataframe for area plots: use first scenario of each category-model combination, if multiple exists
  dta <- dt[region==reg & Category%in% cats & variable%in% vars[2:length(vars)]]
  for (cat in cats){
    for (mod in unique(dta[dta$Category==cat,]$model)){
      if(length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario))==1){
      } else {
        dta <- dta[!(dta$scenario %in% unique(dta[dta$Category==cat & dta$model == mod,]$scenario)[seq(2,length(unique(dta[dta$Category==cat & dta$model == mod,]$scenario)))] & dta$Category==cat & dta$model == mod),]
      }
    }
  }
  #build data frame for overlaid line plots showing totals for all scenarios in each category-model group
  dtl <- dt[region==reg & Category%in% cats & variable%in% vars[1]]
  #build data.frame to display respective scenario names in the panels
  scens <- dta[dta$period==2010 & dta$variable == vars[2],]
  if(!all(is.na(xlim))){
    scens$value <- 0.95*max(dtl[period<xlim[2]]$value)
  } else {
    scens$value <- 0.99*max(dtl$value)
  } 
  dta$period=as.numeric(dta$period)
  dtl$period=as.numeric(dtl$period)
  scens$period=as.numeric(scens$period)
  if(dim(scens)[1]==0){scentext=F}#if there is a problem with the scenario name just leave them out (happens if second variable is non-existent in data)
  p = ggplot()
  p = p + geom_area(data=dta,aes(period, value, group = interaction(variable, region, scenario), fill = variable))
  p = p + geom_path(data=dtl,aes(period, value, group = interaction(variable, region, scenario)))
  p = p + geom_path(data=dtl,aes(period, value, group = interaction(variable, region, scenario)), colour = "#ffffff",linetype=2)
  p = p + facet_grid(Category ~ model)
  p = p + ylab(lab) + xlab("year 20xx")
  if (scentext){p = p + geom_text(data=scens,aes(x=period,y=value,label=scenario,angle=90,hjust=1, vjust = 0 ))}
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + scale_x_continuous(limits=xlim,breaks=xbreaks, labels = substr(xbreaks,3,4))#manual x-axis limits
  if (all(is.na(ylim))){p = p + ylim(c(0,max(dtl[period<xlim[2]]$value)))}#change ylim if manual x but not y limits
  } 
  p = p + scale_fill_manual(values=plotstyle(vars), labels=plotstyle(vars,out="legend"), name=strsplit(vars[1], "|", fixed=T)[[1]][1])
  p = p + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_bar ############################
#############################################################

plot_bar <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", title="Title",file_pre="def",ylim=NA,xlim=NA,ref_line=F){
  #select data
  dt <- dt[region==reg & Category %in% cats & variable %in% vars]
  require(directlabels)
  p = ggplot(dt,aes(x=Category, y=value, fill=model))
  p = p + geom_bar(stat="identity",position=position_dodge(width=0.66),width=0.66)
  #p = p + coord_flip()
  p = p + xlab("")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  #   p = p + facet_grid(variable~period, scales="free_y")
  p = p + scale_fill_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

plot_bar_facet <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", title="Title",file_pre="def",ylim=NA,xlim=NA){
  #select data
  dt <- dt[region==reg & Category %in% cats & variable %in% vars]
  
  p = ggplot(dt,aes(x=model, y=value, fill=Category))
  p = p + facet_grid(variable ~ region,scales="free_y")
  p = p + geom_bar(stat="identity",position=position_dodge(width=0.66),width=0.66)
  #p = p + coord_flip()
  p = p + xlab("")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  p = p + scale_fill_manual(values=plotstyle(as.character(unique(dt$Category))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

plot_bar_facet2 <- function(reg, dt, vars, cats, year=2030, out=cfg$outdir, lab="Title", title="Title",file_pre="def",ylim=NA,xlim=NA,b.legend=F,legendorder){
  #select data
  if ("Historical" %in% cats) {
    hist=dt[ variable %in% vars & Category=="Historical" & region %in% reg & !is.na(value)]
    hist$period <- year
    dt=rbind(hist,dt[ variable %in% vars & period==year & Category %in% cats & region %in% reg & !is.na(value)]) 
    dt %>% factor.data.frame()
  } else {
    dt <- dt[region==reg & Category %in% cats & variable %in% vars &period==year]}
  
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  if(b.legend){dt$Category=factor(dt$Category, levels=legendorder)} else {
    dt$Category=factor(dt$Category,levels=rev(levels(dt$Category)))}
  
  p = ggplot(dt,aes(x=Category, y=value, fill=model))
  p = p + facet_grid(variable ~ region,scales="free_y")
  p = p + geom_bar(stat="identity",position=position_dodge(width=0.66),width=0.66)
  #p = p + coord_flip()
  p = p + xlab("")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  p = p + scale_fill_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + ylab(paste(unitsy))
  p = p + ggtitle(lab)
  p = p + ggplot2::theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

plot_bar_facet3 <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", title="Title",file_pre="def",ylim=NA,xlim=NA,ref_line=F){
  #select data
  dt <- dt[region%in%reg & Category %in% cats & variable %in% vars]
  
  p = ggplot(dt,aes(x=model, y=value, fill=Category))
  p = p + facet_grid(variable ~ region,scales="free_y")
  p = p + geom_bar(stat="identity",position=position_dodge(width=0.66),width=0.66)
  #p = p + coord_flip()
  p = p + xlab("")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (ref_line){p = p + geom_hline(data=protocol[variable%in%vars & unit%in%unique(dt$unit)&period%in%unique(dt$period)&region%in%reg],aes(yintercept=value*1.05),colour=c("#aa0000"))}
  if (ref_line){p = p + geom_hline(data=protocol[variable%in%vars& unit%in%unique(dt$unit)&period%in%unique(dt$period)&region%in%reg],aes(yintercept=value*0.95),colour=c("#0000aa"))}
  p = p + scale_fill_manual(values=plotstyle(as.character(unique(dt$Category))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,cfg$format),p, width=9, height=8, dpi=120)
  return(p)
}
#############################################################
####################### plot_boxplot ########################
#############################################################
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
####################### plot_funnel #########################
#############################################################

plot_funnel <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def",ylim=NA,xlim=NA,glob_lines=F){
  #select data
  dt <- dt[region==reg & Category%in% cats & variable%in% vars]
  #create string with y-axis units for axis label
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(model))),by=c('region','variable','Category')]
  dt=merge(dt, models, by=c('region','variable','Category'))
  #   dt$variable <- paste(dt$variable,' [',dt$number,' models]',sep="")
  
  # Extract data from global models only for funnels
  minmax=dt[Scope=="global" ,list(ymax=max(value),ymin=min(value)),by=c('region','period','Category','variable')]
  minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  minmax<-minmax[order(region, Category, period),]
  minmax$period=as.numeric(minmax$period)
  dt$period=as.numeric(dt$period)
  
  dt$variable <- factor(dt$variable, levels = vars, ordered = T )
  
  p = ggplot()
  # Plot funnel for global models
  p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
  #optional: plot individual model-scenario lines
  if (glob_lines){
    p = p + geom_path(data=dt[region==reg & Scope=="global"],aes(x=period,y=value,group = interaction(scenario,model),
                                                                 color=Category,linetype=model),size=.15)}
  # Plot lines for national models
  p = p + geom_path(data=dt[region==reg & Scope=="national"],aes(x=period,y=value,color=Category,linetype=model),size=2,show.legend = FALSE)
  p = p + scale_linetype_manual(values=cfg$man_lines,labels=plotstyle(cats,out="legend"))
  p = p + scale_colour_manual(values=plotstyle(cats),labels=plotstyle(cats,out="legend"))
  p = p + scale_fill_manual(values=plotstyle(cats),labels=plotstyle(cats,out="legend"))
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  p = p + ylab(paste(unitsy))
  p = p + facet_grid(variable ~ region,scales="free_y")
  p = p + ggtitle(title) + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=8, height=8, dpi=120)
  return(p)
}

plot_funnel2 <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def",ylim=NA,xlim=NA,glob_lines=F,range=F,median=F,linetypemanual=T){
  #select data
  dt <- dt[region==reg & Category%in% cats & variable%in% vars]
  #create string with y-axis units for axis label
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(model))),by=c('region','variable','Category')]
  dt=merge(dt, models, by=c('region','variable','Category'))
  #   dt$variable <- paste(dt$variable,' [',dt$number,' models]',sep="")
  
  # Extract data from global models only for funnels
  minmax=dt[Scope=="global" ,list(ymax=max(value),ymin=min(value),med=median(value)),by=c('region','period','Category','variable')]
  minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  minmax<-minmax[order(region, Category, period),]
  minmax$period=as.numeric(minmax$period)
  dt$period=as.numeric(dt$period)
  
  dt$variable <- factor(dt$variable, levels = c("Emissions|Kyoto Gases","Emissions|Kyoto Gases|Excl. AFOLU CO2","Emissions|CO2|AFOLU"))
  UNEP=data.table(`UNEP (2016) range for conditional NDCs`=c("Median","10%-percentile", "90%-percentile"),Category="INDCi",
                  Baseline="NoPolicy",model="UNEP",region="World",
                  period=2030,Scope="global",value=c(53400,49500,54700), 
                  unit="Mt CO2-equiv./yr",variable="Emissions|Kyoto Gases")
  
  
  p = ggplot()
  # Plot funnel for global models
  p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
  #optional: plot individual model-scenario lines
  if (glob_lines){
    p = p + geom_path(data=dt[region==reg & Scope=="global"],aes(x=period,y=value,group = interaction(scenario,model),
                                                                 color=Category,linetype=model),size=0.5)}
  if(median){p = p + geom_path(data=minmax[region==reg],aes(x=period,y=med,group = Category,
                                                            color=Category),size=1.3)}
  # Plot lines for national models
  p = p + geom_path(data=dt[region==reg & Scope=="national"],aes(x=period,y=value,color=Category,linetype=model),size=2,show.legend = FALSE)
  if(linetypemanual){p = p + scale_linetype_manual(values=cfg$man_lines,name="Model")}
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
  p = p + ylab(paste(unitsy)) + xlab("")
  p = p + facet_grid(variable ~ region,scales="free_y")
  #p = p + geom_point(data=UNEP,aes(x=period,y=value,shape=`UNEP (2016) range for conditional NDCs`),size=3)
  #p = p + scale_shape_manual(values=c("10%-percentile"=20,"90%-percentile"=16,"Median"=15))
  p = p + ggtitle(title) + ggplot2::theme_bw() 
  p = p + theme(axis.text=element_text(size=18),
                axis.title=element_text(size=18),
                strip.text=element_text(size=18),
                legend.text=element_text(size=18),
                legend.title=element_text(size=18),
                plot.title=element_text(size=18))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=12, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_line ###########################
#############################################################

# Standard function for plotting any variable that is left (not having its own function).
# In this case, it shows drivers (population and GDP)
plot_line <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def",ylim=NA,xlim=NA){
  #select data
  dt <- dt[region==reg & Category %in% cats & variable %in% vars]
  #create string with y-axis units for axis label
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(model))),by=c('region','variable')]
  dt=merge(dt, models, by=c('region','variable'))
  dt$variable <- paste(dt$variable,' [',dt$number,' models]',sep="")

  minmax=dt[,list(ymax=max(value),ymin=min(value)),by=c('region','period','Category','variable')]
  minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  minmax<-minmax[order(region, Category, period),]

  p = ggplot()
  p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax),alpha=.3,fill='grey')
  p = p + geom_path(data=dt,aes(x=period,y=value,color=model,group=paste(model,scenario),size=Scope))
  p = p + scale_shape_manual(values=cfg$man_shapes)
  p = p + scale_size_manual(values=c("national"=2, "global"=.2))
  p = p + scale_colour_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + geom_path(data=dt[Scope=="national"],aes(x=period,y=value,color=model,group=paste(model,scenario),size=Scope))
  #p = p + ylab(paste("Carbon Price"))#add unit (different for each facet grid plot...)
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  p = p + facet_grid(variable~region, scales="free_y")
  #p = p + theme(strip.text.y=element_text(angle=45))
  p = p + ylab(paste(unitsy))
  p = p + ggtitle(title) + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

# Standard function for plotting lines for national models only
plot_line_nat <- function(reg, dt, vars, scens, out=cfg$outdir, title="Title", file_pre="def",ylim=NA,xlim=NA){
  #select data
  dt <- dt[region==reg & scenario %in% scens & variable %in% vars]
  #create string with y-axis units for axis label
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  p = ggplot()
  p = p + geom_path(data=dt,aes(x=period,y=value,color=scenario,group=paste(model,scenario),linetype=model),size=1)
  p = p + scale_colour_manual(values=plotstyle(scens))
#   p = p + scale_size_manual(values=c("NoPOL"=.2, "NPi"=.5,"NPi2020_low"=.5,"NPi2020_high"=.5,"INDC"=1,"INDC2030_low"=1,"INDC2030_high"=1))
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  p = p + facet_grid(variable~region, scales="free_y")
  #p = p + theme(strip.text.y=element_text(angle=45))
  p = p + ylab(paste(unitsy))
  p = p + ggtitle(title) + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

plot_lines <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def",ylim=NA,xlim=NA){
  #select data
  dt <- dt[region==reg & Category %in% cats & variable %in% vars]
  #create string with y-axis units for axis label
  unitsy <- paste0("(",unique(dt[variable%in%vars]$unit),")    ")
  unitsy <- paste(rev(unitsy),sep='',collapse='')
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(model))),by=c('region','variable')]
  dt=merge(dt, models, by=c('region','variable'))
  dt$variable <- paste(dt$variable,' [',dt$number,' models]',sep="")
  
  # minmax=dt[,list(ymax=max(value),ymin=min(value)),by=c('region','period','Category','variable')]
  # minmax=minmax[!period %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  # minmax<-minmax[order(region, Category, period),]
  
  p = ggplot()
  #p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax),alpha=.3,fill='grey')
  p = p + geom_path(data=dt,aes(x=period,y=value,color=model,group=paste(model,scenario),size=Scope))
  p = p + scale_shape_manual(values=cfg$man_shapes)
  p = p + scale_size_manual(values=c("national"=2, "global"=.2))
  p = p + geom_path(data=dt[Scope=="national"],aes(x=period,y=value,color=model,group=paste(model,scenario),size=Scope))
  #p = p + ylab(paste("Carbon Price"))#add unit (different for each facet grid plot...)
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  p = p + facet_grid(variable~region, scales="free_y")
  #p = p + theme(strip.text.y=element_text(angle=45))
  p = p + ylab(paste(unitsy))
  p = p + ggtitle(title) + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_point ##########################
#############################################################

plot_point <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", title="Title",file_pre="def",ylim=NA,xlim=NA,ref_line=F){
  #select data
  dt <- dt[region==reg & Category %in% cats & variable %in% vars]
  p = ggplot(dt,aes(x=Category, y=value))
  p = p + geom_point(data=dt,aes(x=Category,y=value,color=model,shape=model),size=4)
  p = p + coord_flip()
  p = p + xlab("")
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  #   p = p + facet_grid(variable~period, scales="free_y")
  #FIXME?
  p = p + scale_shape_manual(values=cfg$man_shapes)
  p = p + scale_colour_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw() #+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
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
####################### plot_scatter ########################
#############################################################

plot_scatter <- function(reg, dt, vars_to_spread, cats, out=cfg$outdir, title="Title", file_pre="scatter",connect=T,ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T,yearlabglob=F,enhance=F) {

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
  if(enhance){p = p + geom_point(data=dt[Scope=="global"],aes(x=x,y=y,color=Category,shape=model),size=5)
              }else{
              p = p + geom_point(data=dt[Scope=="global"],aes(x=x,y=y,color=Category,shape=model))}
  #big points for national models
  p = p + geom_point(data=dt[Scope=="national"],aes(x=x,y=y,color=Category,shape=model),size=8,show.legend = FALSE)
  #label for some years
  #optional: connection lines for each model-scenario
  if (connect&enhance){p = p + geom_path(data=dt,aes(x=x,y=y,color=Category,shape=model,linetype=Scope,
                                             group=interaction(scenario,model)),size=3)}
  if (connect&!enhance){p = p + geom_path(data=dt,aes(x=x,y=y,color=Category,shape=model,linetype=Scope,
                                             group=interaction(scenario,model),size=Scope))}
  if(yearlab){p = p + geom_dl(data=dt[period %in% c(2010,2030,2050) & Scope == "national"],aes(x=x,y=y,label=period),
                              method=list( cex = 0.9, offset=1, hjust=1, vjust = 0))}
  if(yearlabglob){p = p + geom_dl(data=dt[period %in% c(2010,2050) & Scope == "global"],aes(x=x,y=y,label=period),
                              method=list( cex = 0.9, offset=1, hjust=1, vjust = 0))}
  p = p + scale_size_manual(values=c("national"=2, "global"=.2))
  p = p + scale_linetype_manual(values=c("national"="solid", "global"="dashed"))
  p = p + scale_colour_manual(values=plotstyle(cats),name="Scenario")
  p = p + scale_shape_manual(values=cfg$man_shapes,name="Model")
  if (length(reg) >1){p = p + facet_wrap( ~ region,ncol=2)}
  p = p + xlab(paste(vars_to_spread["x"],unitx)) + ylab(paste(vars_to_spread["y"],unity))
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  if (ylog){p = p + scale_y_log10(limits=ylim)} #y-axis logarithmic
  if (xlog){p = p + scale_x_log10(limits=xlim)} #x-axis logarithmic
  p = p + theme(legend.position = "bottom")+ ggplot2::theme_bw(base_size = 15)
  p = p + theme(axis.text = element_text(size=20),
                axis.title = element_text(size=20),
                legend.text = element_text(size=18),
                legend.title = element_text(size=18))
  ggsave(file=paste0(out,"/",file_pre,"_",reg[1],cfg$format),p, width=7, height=8, dpi=120)
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

plot_stackbar_ghg <- function(regs, dt, vars, cats, catsnat, per, out=cfg$outdir, lab="ylab",title=F,Title="Title",file_pre="stackbar",ylim=NA,ybreaks=NA,hist=F,labels=F, var.labels=NA, TotalEmis_var = "Emissions|Kyoto Gases", natpoints, error_bar=F,quantiles=T,minprob=0.1,maxprob=0.9){
  
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
  if(quantiles){dtl=dtl[,list(min=quantile(value,prob=minprob),max=quantile(value,prob=maxprob)),by=c("Category","variable","region","period","Scope","unit")]
  }else{dtl=dtl[,list(min=min(value),max=max(value)),by=c("Category","variable","region","period","Scope","unit")]}
  
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
  p = p + theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1,size=18),
                axis.text.y  = element_text(size = 18),
                plot.title = element_text(size = 20),
                axis.title = element_text(size=18),
                legend.title = element_text(size=18),
                legend.text = element_text(size=18))
  p = p + ylab(lab) + xlab("")
  if(title){p = p + ggtitle(Title)}
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if(labels){p = p + scale_fill_brewer(palette="Spectral",labels=var.labels,name="Variable")}else{p = p + scale_fill_brewer(palette="Spectral",name="Variable")}
  #p = p + scale_fill_manual(values=plotstyle(regs), labels=plotstyle(regs,out="legend"), name=strsplit(regs[1], "|", fixed=T)[[1]][1])
  ggsave(file=paste0(out,"/",file_pre,"_",per,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

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
####################### plot_ternary ########################
#############################################################
plot_ternary <- function(reg, dt, vars_to_spread, cats, out=cfg$outdir, lab="Title", file_pre="tern",yearmax=2100){
 if (length(vars_to_spread) != 3) stop("Ternary plot requires exactly 3 variable")

 # remove unit column because it has different entrie for the two vars_to_spread
 # and causes spread to replace some of the existing values with NA
  dtt <- spread(subset(dt[region==reg & Category%in% cats & variable %in% vars_to_spread & period<=yearmax],select=-unit),variable,value)
 setnames(dtt,vars_to_spread["x"],"x")
 setnames(dtt,vars_to_spread["y"],"y")
 setnames(dtt,vars_to_spread["z"],"z")

  ggtern::ggtern(mapping = aes(x = x, y = y, z = z)) +
    geom_path(data = dtt,
              mapping = aes(linetype = Scope, colour = Category, group = scenario))  + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}
