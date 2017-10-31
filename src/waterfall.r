#' Makes a waterfall plot
#'
#' Makes a waterfall plot using ggplot2.  The bars will be plotted in
#' the order specified by the factoring of the 'category' column.
#' Values should represent the positive or negative changes relative
#' to the previous bar.  
#'
#' @param df a dataframe with columns 'category' (an ordered factor),
#' 'value' (numeric), and 'sector' (character)
#' @param offset the spacing between the columns, default = 0.3
#'
#' @examples
#'  raw <- data.frame(category=c("A", "B", "C", "D"),
#'                    value=c(100, -20, 10, 90),
#'                    sector=1)
#'
#'  df1 <- transform(raw, category=factor(category))                
#'  waterfall(df1) + theme_bw() + labs(x="", y="Value")
#'
#'  df2 <- transform(raw, category=factor(category, levels=c("A", "C", "B", "D")))
#'  waterfall(df2) + theme_bw() + labs(x="", y="Value")
#'
#' @return a ggplot2 object
waterfall <- function(df, offset=0.3) {

    library(ggplot2)
    library(scales)
    library(dplyr)

    ## Add the order column to the raw data frame and order appropriately
    df <- df %>% mutate(order=as.numeric(category)) %>% arrange(order)

    ## The last value needs to be negated so that it goes down to
    ## zero.  Throws a warning if the cumulative sum doesn't match.
    last.id <- nrow(df)
    df$value[last.id] <- -df$value[last.id]

    ## Calculate the cumulative sums
    df <- df %>% mutate(cs1=cumsum(value))

    ## Throw a warning if the values don't match zero as expected
    final_value <- tail(df$cs1, 1)
    if (final_value!=0) {
        warning(sprintf("Final value doesn't return to 0.  ", final_value))
    }
                
    ## Calculate the max and mins for each category and sector
    df <- transform(df, min.val=c(0, head(cs1, -1)),
                    max.val=c(head(cs1, -1), 0))    
    df <- df %>% group_by(order, category, sector, value, cs1) %>%
        summarize(min=min(min.val, max.val), max=max(min.val, max.val))

    ## Create the lines data frame to link the bars
    lines <- df %>% group_by(order) %>% summarize(cs=max(cs1))
    lines <- with(lines, data.frame(x=head(order, -1),
                                    xend=tail(order, -1),
                                    y=head(cs, -1),
                                    yend=head(cs, -1)))
                                    
    
    ## Add the offset parameter
    df <- transform(df, offset=offset)

    ## Make the plot    
    gg <- ggplot() +
        geom_segment(data=lines, aes(x=x, y=y, xend=xend, yend=yend), linetype="dashed")  +
            geom_rect(data=df, aes(xmin=order - offset,
                          xmax=order + offset, 
                          ymin=min,
                          ymax=max, fill=sector)) +
                              scale_x_continuous(breaks=unique(df$order), labels=unique(df$category))

    return(gg)
}

