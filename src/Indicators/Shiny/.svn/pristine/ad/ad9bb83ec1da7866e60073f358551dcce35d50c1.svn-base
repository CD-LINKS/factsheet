#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. http://shiny.rstudio.com/
#

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(tools)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)
library(data.table)
library(knitr)
library(tidyr)
library(colourpicker)
library(shiny)
# library(Cairo)

source('./WJlib.R')

colorset <- function(df,input) {
  cset <- c(input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8, input$c9, input$c10, input$c11, input$c12)
  
  if (input$color!="None") {
    if (length(levels(df[,input$color])) > length(cset)) {
      cset <- c(cset, rainbow(length(levels(df[,input$color]))-length(cset)))
    }
    return(cset)
  }
  if (input$fill!="None") {
    if (length(levels(df[,input$fill])) > length(cset)) {
      cset <- c(cset, rainbow(length(levels(df[,input$fill]))-length(cset)))
    }
    return(cset)
  }
  
  
}

stringconvert <- function(convstr,df,input) {
  
  text_list <- unlist(strsplit(convstr, "%"))
  if (length(text_list)>1) {
    convstr = ""
    for (i in 1:length(text_list)){
      # Should add that this works for all subsets just in case
      if (text_list[i] %in% colnames(df)){
        uname = unique(df[,text_list[i]])
        convstr = paste(convstr, uname, sep ="", collapse = ', ')  
      } else {
        convstr = paste(convstr, text_list[i], sep ="")
      }
    }
  } 
  
  return(convstr)
}

plot_build_wj <- function(df, input){
  
  if(nrow(df)==0){return(NULL)}
  chartopt = input$chart
  if(chartopt == "Bar") {df[,input$x_var] <-factor(df[,input$x_var])}
  # if(input$linetype!="None"){df[,input$linetype] <-factor(df[,input$linetype])}
  # if(input$fill!="None"){df[,input$fill] <-factor(df[,input$fill])}
  # if(input$shape!="None"){df[,input$shape] <-factor(df[,input$shape])}
  # if(input$color!="None"){df[,input$color] <-factor(df[,input$color])}
  # 
  
  colset = paste(deparse(colorset(df, input)), sep="", collapse="")

  G1_text = "G1 = ggplot(data=df)\n"
  
  G1_text = paste(G1_text, "G1 = G1 + aes(x=",input$x_var,", y=",input$y_var,")\n", sep="")
  if(input$ylabpercent){G1_text = paste(G1_text, "G1 = G1 + scale_y_continuous(labels=scales::percent)\n", sep="")}
  
  scalecolor = paste("G1 = G1 + scale_color_manual(values=",colset,")\n", sep="")
  scalefill = paste("G1 = G1 + scale_fill_manual(values=",colset,")\n", sep="")
  scaleshape = paste("G1 = G1 + scale_shape(solid=FALSE)\n", sep="")
  
  aesline = if(input$linetype!="None"){paste("G1 = G1 + aes(linetype=factor(",input$linetype,"))\n", sep="")}else{""}
  aesfill = if(input$fill!="None"){paste("G1 = G1 + aes(fill=factor(",input$fill,"))\n", sep="")}else{""}
  aesshape = if(input$shape!="None"){paste("G1 = G1 + aes(shape=factor(",input$shape,"))\n", sep="")}else{""}
  aescolor = if(input$color!="None"){paste("G1 = G1 + aes(color=factor(",input$color,"))\n", sep="")}else{""}
  plttext <- switch(chartopt,
          # "Line","Point","Bar","Ribbon","Area","Boxplot","linesummary","geom_smooth", "None"
         Line = paste(aesline,aescolor,scalecolor, 
                      "G1 = G1 + geom_line(size=",input$size,", alpha=",input$alpha,")\n", sep=""),
         Point = paste(aesshape,scaleshape, aescolor, 
                       "G1 = G1 + geom_point(size=",input$size,", alpha=",input$alpha,")\n", sep=""),
         Bar = paste(aesfill,scalefill, aescolor, scalecolor,
                       "G1 = G1 + geom_bar(stat='identity',size=",input$size,", alpha=",input$alpha,", width=",input$size,",",
                       "position=", if(input$dodgewidth==0){"'stack'"}else{paste("position_dodge(",input$dodgewidth,")", sep="")},")\n", sep=""),
         Area = paste(aesfill,scalefill, aescolor, scalecolor, aesline,
                       "G1 = G1 + geom_area(alpha=",input$alpha,")\n", sep=""),
         Ribbon = paste(aesfill,scalefill,
                      "G1 = G1 + geom_ribbon(stat='summary',fun.ymin=min,fun.ymax=max,colour=NA,alpha=",input$alpha,")\n", sep=""),
         Boxplot = paste(aescolor,scalecolor, aesshape, scaleshape,
                        "G1 = G1 + geom_boxplot(outlier.shape=46)\n",
                        "G1 = G1 + geom_point(size=", input$size,")\n",sep=""),
         linesummary = paste(aesfill,scalefill,aescolor, scalecolor,
                        "G1 = G1 + geom_linerange(stat='summary',fun.ymin=min,fun.ymax=max,alpha=",input$alpha,",size=",input$size,")\n", sep=""),
         geom_smooth = paste(aesline,aescolor,scalecolor, 
                      "G1 = G1 + geom_smooth(size=",input$size,", alpha=",input$alpha,")\n", sep=""),
         )
  G1_text = paste(G1_text,plttext, sep="")
  
  G1_text = paste(G1_text, "G1 = G1 + ", input$themeopt,"(base_size=14+", input$TextSize,")\n", sep="")
  G1_text = paste(G1_text, "G1 = G1 + theme(legend.position='",input$LegendPosition,"',legend.box='vertical',legend.box.just='left')\n", sep="")  
  if(input$xlabrotate){
    G1_text = paste(G1_text, "G1 = G1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust =0.5))\n")
  }
  
  if(input$title!=""){G1_text <- paste(G1_text, "G1 = G1 + ggtitle('",stringconvert(convstr = input$title, df, input) ,"')\n", sep="")} 
  if(input$xlab!=""){G1_text <- paste(G1_text, "G1 = G1 + xlab('",stringconvert(convstr = input$xlab, df, input) ,"')\n", sep="")} 
  if(input$ylab!=""){G1_text <- paste(G1_text, "G1 = G1 + ylab('",stringconvert(convstr = input$ylab, df, input) ,"')\n", sep="")} 
  if(input$xman){G1_text <- paste(G1_text, "G1 = G1 + xlim(", input$xmin, ",",input$xmax,")\n", sep="")}
  if(input$yman){G1_text <- paste(G1_text, "G1 = G1 + ylim(", input$ymin, ",",if(input$ymin<input$ymax){input$ymax}else{NA},")\n", sep="")}
  
  if (!is.null(input$facet)) {
    if(input$facet!="None"){
      coltxt = paste("ncol=", input$ncol, sep="")
      scltxt = paste("scales='", input$scales, "'",sep="")
      frmtxt = paste("'",if(input$facet2=="None"){""}else{input$facet2}," ~ ",input$facet,"'", sep="")  
      if (input$facet_grid) {
        G1_text <- paste(G1_text, "G1 = G1 + facet_grid(as.formula(",frmtxt,"),",scltxt,")\n", sep="") 
      }else{
        G1_text <- paste(G1_text, "G1 = G1 + facet_wrap(as.formula(",frmtxt,"),",coltxt,",",scltxt,")\n", sep="")
      }
    }
  }
  
  return(G1_text)
}
  
  


