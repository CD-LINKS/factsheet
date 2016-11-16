#############################################################
################ Definition of plot functions ###############
#############################################################


source("functions/plotstyle.R") # load plotstyle() function that provides colors etc. for entities

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
  p = p + scale_shape_manual(values=man_shapes)
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
####################### plot_lines ###########################
#############################################################

# Standard function for plotting any variable that is left (not having its own function).
# In this case, it shows drivers (population and GDP)
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
  p = p + scale_shape_manual(values=man_shapes)
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

  p = ggplot()
  # Plot funnel for global models
  p = p + geom_ribbon(data=minmax,aes(x=period,ymin=ymin,ymax=ymax,fill=Category),alpha=.15)
  #optional: plot individual model-scenario lines
  if (glob_lines){
      p = p + geom_path(data=dt[region==reg & Scope=="global"],aes(x=period,y=value,group = interaction(scenario,model),
                                                                   color=Category,linetype=model),size=.15)}
  # Plot lines for national models
  p = p + geom_path(data=dt[region==reg & Scope=="national"],aes(x=period,y=value,color=Category,linetype=model),size=2,show.legend = FALSE)
  p = p + scale_linetype_manual(values=cfg$man_lines)
  p = p + scale_colour_manual(values=plotstyle(cats))
  p = p + scale_fill_manual(values=plotstyle(cats))
if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  p = p + ylab(paste(unitsy))
  p = p + facet_grid(variable ~ region,scales="free_y")
  p = p + ggtitle(title) + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}


#############################################################
####################### plot_scatter ########################
#############################################################

plot_scatter <- function(reg, dt, vars_to_spread, cats, out=cfg$outdir, title="Title", file_pre="scatter",connect=T,ylim=NA,xlim=NA,xlog=F,ylog=F,yearlab=T) {

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
  p = p + geom_point(data=dt[Scope=="global"],aes(x=x,y=y,color=Category,shape=model))
  #big points for national models
  p = p + geom_point(data=dt[Scope=="national"],aes(x=x,y=y,color=Category,shape=model),size=4,show.legend = FALSE)
  #label for some years
  #optional: connection lines for each model-scenario
  if (connect){p = p + geom_path(data=dt,aes(x=x,y=y,color=Category,shape=model,linetype=Scope,
                                             group=interaction(scenario,model),size=Scope))}
  if(yearlab){p = p + geom_dl(data=dt[period %in% c(2010,2030,2050) & Scope == "national"],aes(x=x,y=y,label=period),
                              method=list( cex = 0.9, offset=1, hjust=1, vjust = 0))}
  p = p + scale_size_manual(values=c("national"=2, "global"=.2))
  p = p + scale_linetype_manual(values=c("national"="solid", "global"="dashed"))
  p = p + scale_colour_manual(values=plotstyle(cats))
  if (length(reg) >1){p = p + facet_wrap( ~ region,ncol=2)}
  p = p + xlab(paste(vars_to_spread["x"],unitx)) + ylab(paste(vars_to_spread["y"],unity))
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  if (ylog){p = p + scale_y_log10(limits=ylim)} #y-axis logarithmic
  if (xlog){p = p + scale_x_log10(limits=xlim)} #x-axis logarithmic
  p = p + theme(legend.position = "bottom") + ggplot2::theme_bw(base_size = 15)
  ggsave(file=paste0(out,"/",file_pre,"_",reg[1],cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}

#############################################################
####################### plot_area ########################
#############################################################

plot_area <- function(reg, dt, vars, cats, out=cfg$outdir, lab="Title", file_pre="area",ylim=NA,ybreaks=NA,xlim=c(2000,2050),xbreaks=c(2010,2030,2050)){
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
  scens$value <- 0.99*max(dtl$value)
  dta$period=as.numeric(dta$period)
  dtl$period=as.numeric(dtl$period)
  scens$period=as.numeric(scens$period)
  
  p = ggplot()
  p = p + geom_area(data=dta,aes(period, value, group = interaction(variable, region, scenario), fill = variable))
  p = p + geom_path(data=dtl,aes(period, value, group = interaction(variable, region, scenario)))
  p = p + geom_path(data=dtl,aes(period, value, group = interaction(variable, region, scenario)), colour = "#ffffff",linetype=2)
  p = p + facet_grid(Category ~ model)
  p = p + ylab(lab) + xlab("")
  p = p + geom_text(data=scens,aes(x=period,y=value,label=scenario,angle=90,hjust=1, vjust = 0 ))
  if (!all(is.na(ylim))){p = p + scale_y_continuous(limits=ylim,breaks=ybreaks)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + scale_x_continuous(limits=xlim,breaks=xbreaks)} #manual x-axis limits
  p = p + scale_fill_manual(values=plotstyle(vars), labels=plotstyle(vars,out="legend"), name=strsplit(vars[1], "|", fixed=T)[[1]][1])
  p = p + ggplot2::theme_bw()
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
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
  if (ref_line){p = p + geom_hline(data=ref_budgets[ref_budgets$region==reg,],aes(yintercept=value),colour=c("#aa0000","#aa0000","#0000aa","#0000aa"))}
#   p = p + facet_grid(variable~period, scales="free_y")
  p = p + scale_fill_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
  p = p + scale_shape_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + scale_colour_manual(values=plotstyle(as.character(unique(dt$model))))
  p = p + ylab(paste(lab))
  p = p + ggplot2::theme_bw() #+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file=paste0(out,"/",file_pre,"_",reg,cfg$format),p, width=7, height=8, dpi=120)
  return(p)
}