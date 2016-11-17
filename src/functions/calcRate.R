calcRate = function(data,var){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  
  dat=data[variable %in% var]
  dat$.keep_all<-NULL #FIXME??
  
  # Calculate compound annual growth rate
  # For 2030-2050 (maybe later add multiple periods?)
  rate=spread(dat[period %in% c('2030','2050')],period,value)
  rate=rate %>% mutate(rate=(((`2050`/`2030`)^(1/20))-1)*100)
  rate=gather(rate,period,value, c(`2030`,`2050`,rate))
  rate=data.table(rate)
  rate=rate[period=="rate"]
  
  #correctly specify remaining dimensions
  rate$variable = paste("Rate of Change|",rate$variable)
  rate$unit = "%/Year"
  rate$period="2030-2050"
  
  #merge data
  setcolorder(rate,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data, rate, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}