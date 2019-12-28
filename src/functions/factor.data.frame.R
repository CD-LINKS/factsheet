# factor.data.frame.R   re-factorise factor and character columns in data frame 
# ----------------------------------------------------------------------------- 
# input:  df - data frame
# output: data frame
# ----------------------------------------------------------------------------- 
#                                                       Michaja Pehl 2015-02-23

factor.data.frame <- function(df) {
    for (c in 1:ncol(df)) 
        if (is.factor(df[[c]]) | typeof(df[[c]]) == "character") {
            df[[c]] <- factor(df[[c]])
            
            if ("" %in% levels(df[[c]]))
                levels(df[[c]])[levels(df[[c]]) == ""] <- NA
        }
    return(df)
}
