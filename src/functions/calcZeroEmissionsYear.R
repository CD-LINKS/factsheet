calcZeroEmissionsYear = function(data,var,new_var){

  dat=data[variable %in% var]
  
  #ZeroEmissionsYear=dat[!duplicated(dat[,list(model,Category,region,variable),with=TRUE]),!c('value','period'),with=FALSE] #"Scope","Baseline","scenario"
  tmp<-dat[period==2100]
  tmp$value=-0.1
  tmp$period=2150
  dat=rbind(dat,tmp)
  ZeroEmissionsYear=dat[value<=0,min(period),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  ZeroEmissionsYear[is.na(V1),]$V1=2105
  names(ZeroEmissionsYear)[names(ZeroEmissionsYear) == "V1"] <- "value"

  #correctly specify remaining dimensions
  ZeroEmissionsYear$variable = new_var
  ZeroEmissionsYear$unit = "Year"
  ZeroEmissionsYear$period=ZeroEmissionsYear$value
  ZeroEmissionsYear$value <- as.integer(ZeroEmissionsYear$value)
  #ZeroEmissionsYear$period <- 2015 #peak year does not belong to a specific year, 2015 is starting point scenarios
  
  #merge data
  setcolorder(ZeroEmissionsYear,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data, ZeroEmissionsYear, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}