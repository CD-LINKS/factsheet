#' Calculate new variables from existing ones
#' 
#' Calculate new variable form existing ones, using a generic formula
#' 
#' 
#' @param data dataframe
#' @param var variable for which to calculate relative value to Baseline
#' @param baseEq1 boolean to select Baseline value as either 1 or 0
#' @param new_var name for newly calculated variable
#' @return dataframe - contains all original data plus the newly derived
#' variable
#' @author Christoph Bertram
#' @examples
#' 
#'   \dontrun{
#' data <- calcRel2Base(data=dat,var="Emissions|CO2",baseEq1=FALSE,new_var="relative Abatement|CO2")
#'   }
#' 
calcRel2Base = function(data,var,baseEq1,new_var,scens){
  if(!(var %in% data$Variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  #go through all possible Baseline scenarios
  for (base_scen in unique(data$Baseline)[!is.na(unique(data$Baseline))]){
    #go through all available policy scenarios for each baseline
    for(pol_scen in unique(data[data$Baseline==base_scen,]$Scenario)){
      #select data 
      tmp <- data[data$Scenario %in% c(base_scen,pol_scen) 
            & data$Variable %in% var,]
    #filter out models that do not have the pol_scen
      tmp <- tmp[tmp$Model%in%unique(tmp[tmp$Scenario ==pol_scen,]$Model),]
   #calculate relative to Baseline value (=100% if policy and baseline scenario have the same value)
   if(isTRUE(baseEq1)){
     res = tmp %>% 
       group_by(Model,Region,Year,Scope) %>% arrange(Scenario == pol_scen) %>% summarize(value = 100* value[2]/value[1]) %>% ungroup()
   #OR calculate relative abatement (=0% if policy and baseline scenario have the same value)
   } else {
    res = tmp %>% 
    group_by(Model,Region,Year,Scope) %>% arrange(Scenario == pol_scen) %>% summarize(value = 100* (value[1] - value[2])/value[1]) %>% ungroup()
   }
   #correctly specify remaining dimensions
    res$Scenario = pol_scen
    res$Category <- scens[scens$Scenario == pol_scen,]$Category
    res$Baseline <- base_scen
    res$Variable = new_var
    res$Unit = '%' 
   #merge data
   data <- rbind(data,res)
   }
  }
  # convert to data.table / data.frame and return
return(as.data.table(as.data.frame(data)))
 
}
