#' Calculate new variables from existing ones
#'
#' Calculate new variable form existing ones, using a generic formula
#'
#'
#' @param df dataframe
#' @param vars character vector of variable names for which to calculate value relative to baseyear
#' @param baseyear (numeric) to which the data shall be normalized
#' @return dataframe - contains all original data plus the newly derived
#' variable
#' @author Gunnar Luderer
#' @examples
#'
#'   \dontrun{
#' data.rel2BaseYear <- calcRel2BaseYear(df=data,vars=c("Emissions|CO2", "Population"))
#'   }
#'

calcRel2BaseYear = function(df,vars,baseyear=2010){

    # rename Years as a workaround since mutate cannot deal with numeric column names
    df <-  df %>% filter(variable %in% vars) %>% mutate(period = paste0("y",period))

    df <- spread(df, key = period, value = value)

    # use mutate_ to work with dynamically created baseyear variable name
    df <- mutate_(df, .dots = setNames (paste0("y", baseyear), "baseyear"))

    years = grep("y2",colnames(df), value = TRUE)

    # use mutate_ to dynamically cycle through the different years
    for (yr in years)
    {
        df <- mutate_(df, .dots = setNames( paste0(yr, " / baseyear"), yr ))
    }

    df <- df %>% select(-baseyear) %>%
        gather( key = period, value = value, starts_with("y2"), na.rm=T) %>%
        mutate(period = as.numeric(substr(period,2,5))) %>%
        mutate(variable = paste0(variable,"|rel",baseyear))

    return(df)

}
