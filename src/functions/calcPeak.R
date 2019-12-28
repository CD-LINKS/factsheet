calcPeak = function(data,var,new_var){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  
  dat=data[variable %in% var]
  
  # Calculate peak year
  peak = dat[,list(value=as.numeric(period[which.max(value)])),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  peak = subset(peak, !value=="0"|!value=="2005")

  #correctly specify remaining dimensions
  peak$variable = new_var
  peak$unit = "Year"
  peak$period=peak$value
  
  #merge data
  setcolorder(peak,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data, peak, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}