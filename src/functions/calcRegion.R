#' Calculate new variables from existing ones
#'
#' Calculate new variable form existing ones, using a generic formula
#'
#'
#' @param data dataframe
#' @param formula R formula. e.g. ' `NewRegion` ~ `Region_A` + `Region_B` - `Region_C` '
#' @param b.append boolean, result will be appended to data if TRUE, else only new data will be returned
#' '#' @return dataframe - contains all original data plus the newly derived
#' variable
#' @author Gunnar Luderer, based on calcVariable by Anselm Schultes
#' @examples
#'
#'   \dontrun{
#' data <- calcVariable(data, (' `oASIA` ~ `ASIA` - `IND` - `CHN` ') )
#'   }
#'
calcRegion = function(data,formula, b.append = T){
  #usage: data = calcAddVariable(data,as.formula('result ~ `PE|Coal`/`Cap|Electricity|Gas|w/o CCS` ') , newUnit='None')
  formula = as.formula(formula)
  vars_rhs = all.vars(formula[[3]])
  if(!(all(vars_rhs %in% data$region) )){
    stop("Error: The dataframe does not contain all regions found in the formula provided!")
  }

  res <- data %>% filter(region %in% vars_rhs) %>%     #filter out variables in forumla
      dcast(... ~ region) %>%          #convert to wide format
      mutate(region = as.character(formula[[2]]))

  res[is.na(res)] = 0

  res <- res %>% mutate_(value = formula[[3]] )   #evluate formula

  res <- res[, !vars_rhs, with=F] # drop redundant variables

  #merge data, convert to quitte
  if (b.append)
    return(as.data.table(as.data.frame(rbind(data,res))))
  else
    return(res)
}
