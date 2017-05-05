plotstyle <- function(..., out="color", unknown=NULL, plot=FALSE) {
  
  luplot<-list()
  luplot$plotstyle <- read.csv2("./settings/plotstyles.csv",stringsAsFactors = F,row.names=1)
  
  if(is.null(out)) {
    out <- "color"
  } else if(!(out %in% c(names(luplot$plotstyle),"all"))) {
    stop('Unknown style type "',out,'"!')
  }
  
  # returns n levels of gray (in hexadecimal foramt) between lower (default=0: black) and upper (default=1: white) bound
  .makegray <- function(n,from=0,to=1) {
    u <- seq(from,to,length.out=n)
    u <- t(matrix(rep(u,each=3),nrow=3))
    return(rgb(u))  
  }
  
  # make sure that luplot$plotstyle is of type data.frame
  class(luplot$plotstyle) <- "data.frame"
  
  # choose plot colors for given entities
  entity <- c(...)
  if(is.null(entity)) entity <- row.names(luplot$plotstyle)
  
  uq_entity <- unique(entity)
  res <- luplot$plotstyle[uq_entity,]
  row.names(res) <- uq_entity
  
  # count unknown entities
  nna <- sum(!complete.cases(res))

  # replace NA
  if (nna != 0) {
    if (is.null(unknown)) {
      # replace NA in color with shades of gray
      res$color[is.na(res$color)] <- .makegray(nna,from=0.2,to=0.8)
      # replace NA in legens with row names (= entitiy name)
      res$legend[is.na(res$legend)] <- row.names(res[is.na(res$legend),])
    } else {
      if (out=="all") {
        if (!all(names(unknown) %in% names(luplot$plotstyle))) {
          stop("There are elements in names(unknown) that are not in names(plotstyle)!")
        }
        for (n in names(unknown)){
          res[[n]][is.na(res[[n]])] <- as.character((unknown[[n]][1:nna]))
        }
      } else if(!(out %in% names(unknown))) {
        stop('Style type "',out,'" is not existing in argument "unknown"!')
      } else {
        res[[out]][is.na(res[[out]])] <- as.character(unknown[[out]][1:nna])
      }
    }
  }
  
  if (plot) {
    require(ggplot2, quietly = TRUE)
    df<-data.frame(x=uq_entity,c=res$color)
    df$x<-factor(df$x,levels=rev(uq_entity)) # prevent ggplot from sorting it alphabetical by giving order explicitly here
  
    ncol    <- 30 # color bars per page
    pagemax <- ceiling(length(res$color)/ncol) # number of pages
    for (page in 1:pagemax) {
      # start and end index for respective page
      from <- (page-1)*ncol+1
      to   <- min(page*ncol,length(res$color))
      # create data frame
      x <- rownames(res)[from:to]
      c <- res$color[from:to]
      df<-data.frame(x=x, c=c)
      # prevent ggplot from sorting it alphabetically by giving order explicitly here
      # using original order of rownames. Reversing it because the bar plot reverses it again
      # To yield the correct mapping between colors and labels the colors have to be also reversed
      df$x<-factor(df$x,levels=rev(x)) 
      # create bar plot
      p1 <- ggplot(data=df, aes(x=x)) + geom_bar(stat="bin",fill=rev(df$c)) + coord_flip() + 
               theme(axis.title.x = element_blank(),axis.title.y = element_blank()) + 
               labs(title=paste0("Color bars (plot ",page," of ",pagemax,")"))

      print(p1)
    }
    
  }
 
  res <- res[entity,]
  
  # select the output data from res according to "out"
  if (out!="all") {
    res <- res[[out]]
    names(res) <- entity
  }
  return(res)
}