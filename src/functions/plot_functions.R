#############################################################
################ Definition of plot functions ###############
#############################################################
library(tidyr)   # spread
library(ggplot2) # ggplot


#############################################################
####################### plot_line ###########################
#############################################################

# Standard function for plotting any variable that is left (not having its own function).
# In this case, it shows drivers (population and GDP)
plot_line <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def"){
  
  dt <- dt[Region==reg & Category%in% cats & Variable%in% vars]
  
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(Model))),by=c('Region','Variable')]
  dt=merge(dt, models, by=c('Region','Variable'))
  dt$Variable <- paste(dt$Variable,' [',dt$number,' models]',sep="")
  
  minmax=dt[,list(ymax=max(value),ymin=min(value)),by=c('Region','Year','Category','Variable')]
  minmax=minmax[!Year %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  minmax<-minmax[order(Region, Category, Year),]

  p = ggplot()
  p = p + geom_line(data=dt,aes(x=Year,y=value,color=Model,group=paste(Model,Scenario),size=Scope)) #& Model=="REMIND 1.5"
  p = p + scale_shape_manual(values=man_shapes)
  p = p + scale_size_manual(values=c("national"=2, "global"=1))
  p = p + geom_ribbon(data=minmax,aes(x=Year,ymin=ymin,ymax=ymax),alpha=.3,fill='grey')
  #p = p + ylab(paste("Carbon Price"))#add unit, unit exists on miles db (different for each facet grid plot...)
  p = p + ylim(0,NA)
  p = p + facet_grid(Variable~Region, scales="free_y")
  #p = p + theme(strip.text.y=element_text(angle=45))
  p = p + ggtitle(title)
  ggsave(file=paste0(out,"/",file_pre,"_",reg,".png"),p, width=7, height=8, dpi=120)
  return(p)
}


#############################################################
####################### plot_funnel #########################
#############################################################

plot_funnel <- function(reg, dt, vars, cats, out=cfg$outdir, title="Title", file_pre="def"){
  
  dt <- dt[Region==reg & Category%in% cats & Variable%in% vars]
  
  # For each variable count models and add to data and variable name
  models=dt[,list(number=length(unique(Model))),by=c('Region','Variable','Category')]
  dt=merge(dt, models, by=c('Region','Variable','Category'))
#   dt$Variable <- paste(dt$Variable,' [',dt$number,' models]',sep="")
  
  # Extract data from global models only for funnels
  minmax=dt[Scope=="global" ,list(ymax=max(value),ymin=min(value)),by=c('Region','Year','Category','Variable')]
  minmax=minmax[!Year %in% c("2015","2025","2035","2045","2055","2065","2075","2085","2095")]
  minmax<-minmax[order(Region, Category, Year),]
  
  p = ggplot()
  # Plot lines for national models
  p = p + geom_line(data=dt[Region==reg & Scope=="national"],aes(x=Year,y=value,color=Scenario,linetype=Model),size=1)
  p = p + scale_linetype_manual(values=cfg$man_lines)
  # Plot funnel for global models
  p = p + geom_ribbon(data=minmax,aes(x=Year,ymin=ymin,ymax=ymax,fill=Category),alpha=.3)
  p = p + ylim(0,NA)
  p = p + facet_grid(Variable ~ Region,scales="free_y")
  p = p + ggtitle(title)
  ggsave(file=paste0(out,"/",file_pre,"_",reg,".png"),p, width=7, height=8, dpi=120)
  return(p)
}


#############################################################
####################### plot_scatter ########################
#############################################################

plot_scatter <- function(reg, dt, vars_to_spread, cats, out=cfg$outdir, title="Title", file_pre="scatter",ylim=NA,xlim=NA,xlog=F,ylog=F) {

  if (length(vars_to_spread) != 2) stop("Scatter plot requires exactly two variables")
  
  # remove Unit column because it has different entrie for the two vars_to_spread 
  # and causes spread to replace some of the existing values with NA
  dt <- spread(subset(dt[Region==reg & Category%in% cats & Variable %in% vars_to_spread],select=-Unit),Variable,value)
  setnames(dt,vars_to_spread["x"],"x")
  setnames(dt,vars_to_spread["y"],"y")
  
  p = ggplot()
  p = p + geom_point(data=dt,aes(x=x,y=y,color=Category,shape=Model))
  p = p + xlab(vars_to_spread["x"]) + ylab(vars_to_spread["y"])
  if (ylog){p = p + scale_y_log10()} #y-axis logarithmic
  if (xlog){p = p + scale_x_log10()} #x-axis logarithmic
  if (!all(is.na(ylim))){p = p + ylim(ylim)} #manual y-axis limits
  if (!all(is.na(xlim))){p = p + xlim(xlim)} #manual x-axis limits
  ggsave(file=paste0(out,"/",file_pre,"_",reg,".png"),p, width=7, height=8, dpi=120)
  return(p)
}


