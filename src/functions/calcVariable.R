#' Calculate new variables from existing ones
#'
#' Calculate new variable form existing ones, using a generic formula
#'
#'
#' @param data dataframe
#' @param formula R formula. e.g. as.formula(' result ~ `PE|Coal` *
#' `Consumption in Uganda`/lag(Population) ')
#' @param newUnit unit for the newly calculated variable
#' @return dataframe - contains all original data plus the newly derived
#' variable
#' @author Christoph Bertram, Anselm Schultes
#' @examples
#'
#'   \dontrun{
#' data <- calcVariable(data,as.formula('result ~ `PE|Coal`/`Cap|Electricity|Gas|w/o CCS` ') , newUnit='None')
#'   }
#'
calcVariable = function(data,formula,newUnit='None'){
  #usage: data = calcAddVariable(data,as.formula('result ~ `PE|Coal`/`Cap|Electricity|Gas|w/o CCS` ') , newUnit='None')
  formula = as.formula(formula)
  vars_rhs = all.vars(formula[[3]])
  vars_rhs = vars_rhs[vars_rhs != 'Period'] # period plays a special role here, add it later to the data frame
  if(!(all(vars_rhs %in% data$variable) )){
    stop("Error: The dataframe does not contain all variables found in the formula provided!")
  }
  #filter out variables in forumla
  res <- data[data$variable %in% vars_rhs,]
  #delete units
  res$unit <- NULL
  #convert to wide format
  res <- dcast(res,... ~ variable)
  #convert to years for the calculation:

  #do the actual calculation. split among model,scenario,region (FIXME: Who knows how to group_by for all but some specified column name(s)? Please do tell Anselm. Thanks.), then apply formula to the vector (of values at different time-steps):
  res <- res %>%
    group_by(model,scenario,region) %>%
    mutate_(result = formula[[3]]  ) %>% ungroup()

  #label new unit
  res$unit <- newUnit
  res <- res[, !vars_rhs, with=F] # drop redundant variables
  res$variable = as.character(formula[[2]]) #rename new variable correctly
  names(res)[names(res) == 'result'] = 'value'

  #merge data, convert to dataframe
  # FIXME: add overview function to functions folder
  return(as.data.table(as.data.frame(overwrite(res,data))))
}
