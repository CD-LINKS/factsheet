mipStackedBarDiscreteX <-  function (x, x.dim, stack.dim = "variable", total = NULL, ylab, xlab = "", colour = "default",
          breaks = NULL, title = NULL, name_total = FALSE, width = 0.9, pl.obj = NULL)
{

    pos <- x
    pos$value[pos$value < 0] <- 0
    neg <- x
    neg$value[neg$value > 0] <- 0

    if(is.ggplot(pl.obj) ){
        p <- pl.obj
    } else {
        p <- ggplot()
    }


    p <- p +
        geom_bar(data = pos, aes_string(x = x.dim, y = "value", fill = stack.dim),  stat = "identity", position = "stack", width = width) +
        geom_bar(data = neg, aes_string(x = x.dim, y = "value", fill = stack.dim),  stat = "identity", position = "stack", width = width)
    if (!is.null(total)) {
        if (name_total == TRUE) {
            p <- p + geom_point(data = total, aes_string(x = x.dim, y = "value"), color = "#000000")
            p <- p + scale_linetype_discrete(labels = "Total",
                                             name = "")
        }
        else {
            p <- p + geom_point(data = total, aes_string(x = x.dim, y = "value"), color = "#000000")
        }
    }
    p <- p + xlab(xlab)
    if (!is.null(ylab)) {
        p <- p + ylab(ylab)
    }
    if (!is.null(breaks)) {
        p <- p + scale_x_continuous(breaks = breaks)
    }
    p <- p + theme(axis.ticks = element_blank())
    if (!is.null(title)) {
        p <- p + ggtitle(title)
    }
    if (colour == "default") {
        p <- p + scale_fill_manual(values = plotstyle(as.character(unique(x$variable))),
                                   labels = plotstyle(as.character(unique(x$variable)),
                                                      out = "legend"), name = "")
    }
    else {
        p <- p + scale_fill_manual(values = colour)
    }
    return(p)
}
