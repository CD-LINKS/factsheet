calcBudget = function(data,var,new_var,scens){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }

# First calculate budget per model, then median and range
dat=data[variable %in% var]

# To make sure we only use the models with data until 2100, important for this indicator
check=dat[,list(unique(period)), by=c("model")]
check=subset(check, subset=V1=="2100")
dat=subset(dat, subset=model %in% check$model)

# Interpolate to get values for each year
yy=seq(unique(dat$period)[1],2100)
dt = dat[,list(approx(x=period,y=value,xout=yy)$y,approx(x=period,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','.keep_all','unit','variable')]
setnames(dt,"V1","value")
setnames(dt,"V2","Year")

# Sum to get budget
budget=dt[Year %in% c(2010:2100),sum(value/1000,na.rm=TRUE),by=c('scenario','Category','Baseline','model','region','Scope','.keep_all','unit','variable')]
budget=budget[V1<0,V1:=5]
setnames(budget,"V1","value")

#correctly specify remaining dimensions
budget$variable = new_var
budget$unit = 'Gt CO2'
budget$period='2100'

#merge data
setcolorder(budget,c('scenario','Category','Baseline','model','region','period','Scope','.keep_all','value','unit','variable'))
data <- rbind(data,budget, fill=TRUE)

# convert to data.table / data.frame and return
return(as.data.table(as.data.frame(data)))
}
