calcBudget_2015 = function(data,var,new_var){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  
  dat=data[variable %in% var]

  ######2050 budgets
  # Interpolate to get values for each year
  yy=seq(2005,2050)
  dt = dat[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  setnames(dt,"V1","value")
  setnames(dt,"V2","Year")
  
  # Sum to get budget
  budget50=dt[Year %in% c(2016:2050),sum(value/1000,na.rm=TRUE),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  #budget50=budget50[V1<0,V1:=5]
  setnames(budget50,"V1","value")
  
  #correctly specify remaining dimensions
  budget50$variable = new_var
  budget50$unit = 'Gt CO2'
  budget50$period='2050'
  
  #######2100 budgets
  #### To make sure we only use the models with data until 2100, important for this indicator
  check=dat[,list(unique(period)), by=c("model")]
  check=subset(check, subset=V1=="2100")
  dat=subset(dat, subset=model %in% check$model)
  
  # Interpolate to get values for each year
  yy=seq(2005,2100)
  dt = dat[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  setnames(dt,"V1","value")
  setnames(dt,"V2","Year")
  
  # Sum to get budget
  budget100=dt[Year %in% c(2016:2100),sum(value/1000,na.rm=TRUE),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  #budget100=budget100[V1<0,V1:=5]
  setnames(budget100,"V1","value")
  
  #correctly specify remaining dimensions
  budget100$variable = new_var
  budget100$unit = 'Gt CO2'
  budget100$period='2100'
  
  #calculate 2051-2100 budget
  # Sum to get budget
  budget105=dt[Year %in% c(2051:2100),sum(value/1000,na.rm=TRUE),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  #budget105=budget105[V1<0,V1:=5]
  setnames(budget105,"V1","value")
  
  #correctly specify remaining dimensions
  budget105$variable = new_var
  budget105$unit = 'Gt CO2'
  budget105$period='2105'
  
  ######2030 budgets
  # Interpolate to get values for each year
  yy=seq(2005,2030)
  dt = dat[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  setnames(dt,"V1","value")
  setnames(dt,"V2","Year")
  
  # Sum to get budget
  budget30=dt[Year %in% c(2016:2030),sum(value/1000,na.rm=TRUE),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  #budget50=budget50[V1<0,V1:=5]
  setnames(budget30,"V1","value")
  
  #correctly specify remaining dimensions
  budget30$variable = new_var
  budget30$unit = 'Gt CO2'
  budget30$period='2030'
  
  #merge data
  setcolorder(budget30,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  setcolorder(budget50,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  setcolorder(budget100,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  setcolorder(budget105,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data,budget30,budget50, budget100,budget105, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}