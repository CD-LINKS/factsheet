#' Replace rows in a data frame with new values

#'

#' \code{overwrite()} \code{rbind()}s the data frames \code{lhs} and \code{rhs},

#' removing any duplicate lines, which are determined without regard to the

#' columns in \code{except}.

#'

#' @param lhs data frame with values that will replace others

#' @param rhs data frame with values that will be replaced

#' @param except names of columns that will not be considered in determining

#'               which columns to replace; defaults to "value"

#' @return data frame in which rows from rhs have been replaced with rows from

#'         lhs

#' @author Michaja Pehl

#' @examples

#' data <- data.frame(expand.grid(UPPER = LETTERS[1:2],

#'                                lower = letters[24:26]),

#'                    value = 1:6)

#' data

#' data %>%

#'   filter(lower == "y") %>%

#'   mutate(value = value * 10) %>%

#'   overwrite(data)

#'

#' @export

#' @importFrom dplyr '%>%' distinct_

#'

overwrite <- function(lhs, rhs, except = "value") {

  return(

    rbind(lhs, rhs) %>%

      distinct_(.dots = setdiff(colnames(rhs), except), .keep_all = TRUE)

  )

}

