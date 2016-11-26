calcRate = function(data,var){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  
  dat=data[variable %in% var]

  # Calculate compound annual growth rate
  ### For 2030-2050 (maybe later add multiple periods?)
  rate=spread(dat[period %in% c('2030','2050')],period,value)
  rate=rate %>% mutate(rate=(((`2050`/`2030`)^(1/20))-1)*100)
  rate=gather(rate,period,value, c(`2030`,`2050`,rate))
  rate=data.table(rate)
  rate=rate[period=="rate"]
  
  #correctly specify remaining dimensions
  rate$variable = paste("Rate of Change|",rate$variable)
  rate$unit = "%/Year"
  rate$period="2030-2050"
  
  ### For 2030-2050
  rate2=spread(dat[period %in% c('2050','2100')],period,value)
  rate2=rate2 %>% mutate(rate=(((`2100`/`2050`)^(1/50))-1)*100)
  rate2=gather(rate2,period,value, c(`2050`,`2100`,rate))
  rate2=data.table(rate2)
  rate2=rate2[period=="rate"]
  
  #correctly specify remaining dimensions
  rate2$variable = paste("Rate of Change|",rate2$variable)
  rate2$unit = "%/Year"
  rate2$period="2050-2100"
  
  #merge data
  setcolorder(rate,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  setcolorder(rate2,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data, rate, rate2, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}