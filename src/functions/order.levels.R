order.levels <-  function (df, ..., drop.extra.levels = TRUE)
{
  if (!is.data.frame(df))
    stop("only works on data frames")
  dots <- list(...)
  dot.names <- names(dots)
  for (column in dot.names) {
    if (!is.factor(getElement(df, column)))
      stop(paste(column, "is not a factor"))
    have.levels <- levels(getElement(df, column))
    want.levels <- getElement(dots, column)
    if (drop.extra.levels)
      want.levels <- intersect(want.levels, have.levels)
    df[, column] <- factor(getElement(df, column), levels = want.levels)
  }
  return(df)
}
