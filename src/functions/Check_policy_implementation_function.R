# first run main_xCut_wp2.R which creates data frame 'all'

library(gridExtra)
library(ggplot2)
library(scales)
library(grid)
library(data.table)
library(plyr)
library(stats)
library(data.table)
library(xtable)

# prepare functions
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10,11,12,13,14,15)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

ShowGHGReductionRelToScenario <- function(SECTOR, select_region, Scen1, Scen2, year_subset1, year_subset2, create_graphs)
{
 #pre: 
  # Sector = {economy wide, energy supply, industry, residential and commercial, transportation, AFOLU}
  GHG = "ALL"
  #SECTOR = "economy wide"
  #select_region = "World"
  
  # define scenario names, reductions from Scen1 relative to Scen2 are calculated
  # use the 'Category' names from the 'all' dataframe
  #Scen1 = "NPi"
  #Scen2 = "NoPOL"
  #Scen3 = "INDC"
  #year_subset1 = 2030
  #year_subset2 = 2010
  
    # const
  AIM = "AIM/CGE"
  COPPE = "COPPE-COFFEE 1.0"
  DNE = "DNE21+ V.14"
  GEM_E3 = "GEM-E3"
  IMAGE = "IMAGE 3.0"
  POLES = "POLES CDL"
  REMIND = "REMIND-MAgPIE 1.7-3.0"
  MESSAGE = "MESSAGEix-GLOBIOM_1.0"
  WITCH = "WITCH2016"

  GWP_CH4 = 25 #data in CD_LINKS output is in Mt CH4
  GWP_N2O = 298/1000 #data in CD_LINKS output is in kt N2O
  
  # Emissions|CO2|Energy|Demand|Residential and Commercial
  # Emissions|CH4|Energy|Demand|Residential and Commercial
  if(SECTOR == "economy wide") { GHG_var1 <- "Emissions|CO2"; GWP1 <- 1; print("Emissions|CO2")
                                 GHG_var2 <- "Emissions|CH4";GWP2 <- GWP_CH4; print("Emissions|CH4")
                                 GHG_var3 <- "Emissions|N2O"; GWP3 <- GWP_N2O; print("Emissions|N2O")
                                 GHG_var4 <- "Emissions|F-Gases"; GWP4 <- 1; print("Emissions|F-gases")
  } else if (SECTOR == "energy supply")  { GHG_var1 <- "Emissions|CO2|Energy|Supply"; GWP1 <- 1; ; print("Emissions|CO2|Energy|Supply")
                                           GHG_var2 <- "Emissions|CH4|Energy|Supply"; GWP2 <- GWP_CH4; ; print("Emissions|CH4|Energy|Supply")
                                           GHG_var3 <- "Emissions|N2O|Energy"; GWP3 <- GWP_N2O; ; print("Emissions|N2O|Energy")
                                           GHG_var4 <- ""; GWP4 <- 0;
  } else if (SECTOR == "industry") { GHG_var1 <- "Emissions|CO2|Energy|Demand|Industry"; GWP1 <- 1; 
                                     GHG_var2 <- "Emissions|CH4|Energy|Demand|Industry"; GWP2 <- GWP_CH4;
                                     GHG_var3 <- "Emissions|CO2|Industrial Processes"; GWP3 <- 1;
                                     GHG_var4 <- ""; GWP4 < -0;
  } else if (SECTOR == "residential and commercial") { GHG_var1 <- "Emissions|CO2|Energy|Demand|Residential and Commercial"; GWP1 <- 1;
                                                       GHG_var2 <- "Emissions|CH4|Energy|Demand|Residential and Commercial"; GWP2 <- GWP_CH4;
                                                       GHG_var3 <- ""; GWP3 <- 0;
                                                       GHG_var4 <- "" ; GWP4 <- 0;
  } else if (SECTOR == "transportation") { GHG_var1 <- "Emissions|CO2|Energy|Demand|Transportation"; GWP1 <- 1;
                                           GHG_var2 <- "Emissions|CH4|Energy|Demand|Transportation"; GWP2 <- GWP_CH4;
                                           GHG_var3 <- ""; GWP3 <- 0;
                                           GHG_var4 <- ""; GWP4 <- 0;
  } else if (SECTOR == "AFOLU") { GHG_var1 <- "Emissions|CO2|AFOLU"; GWP1 <- 1;
                                  GHG_var2 <- "Emissions|CH4|AFOLU"; GWP2 <- GWP_CH4;
                                  GHG_var3 <- "Emissions|N2O|AFOLU"; GWP3 <- GWP_N2O;
                                  GHG_var4 <- ""; GWP4 <-0;
  } else {
  }

    
  title_GHG_plot1 <- switch(GHG, 
                  "ALL"   = "Total GHG emissions",
                  "CO2"   = "CO2 emissions",
                  "CH4"   = "CH4 emissions", 
                  "N2O"   = "N2O emissions",
                  "FGases"= "F-gas emissions")
  
  title_GHG_plot2 <- switch(SECTOR, 
                           "economy wide"               = " economy wide",
                           "energy supply"              = " energy supply sector",
                           "industry"                   = " industry sector",
                           "residential and commercial" = " buildings sector", 
                           "transportation"             = " transport sector",
                           "AFOLU"                      = " AFOLU sector")
  title_GHG_plot <- paste(title_GHG_plot1, title_GHG_plot2, ":", select_region ,"\n", Scen1, " ", year_subset1, "relative to ", Scen2, year_subset2)
    
    
  #prepare data
  data_plot <- all
  data_plot$period <- suppressWarnings(as.numeric(data_plot$period))
  data_plot <- data_plot[!is.na(data_plot$period),]
    
  # alternative to add GHG emissions in one go
  dt1 <- data.table(data_plot)
  #dt2 <- subset(dt1, period == 2030 & region==select_region & Scope == "global" & (Category==Scen2 | Category==Scen1) & (variable==GHG_var1 | variable==GHG_var2 | variable==GHG_var3) | variable==GHG_var4)
  #dt2 <- subset(dt1, period == year_subset & region==select_region & Scope == "global" & (Category==Scen2 | Category==Scen1) & (variable==GHG_var1 | variable==GHG_var2 | variable==GHG_var3 | variable==GHG_var4))
  dt2_1 <- subset(dt1, period == year_subset1 & region==select_region & Scope == "global" & Category==Scen1 & (variable==GHG_var1 | variable==GHG_var2 | variable==GHG_var3 | variable==GHG_var4))
  dt2_2 <- subset(dt1, period == year_subset2 & region==select_region & Scope == "global" & Category==Scen2 & (variable==GHG_var1 | variable==GHG_var2 | variable==GHG_var3 | variable==GHG_var4))
  dt2 <- rbind(dt2_1, dt2_2)
  dt3 <- dt2
  # translate GHG emissions into CO2 equivalent mass
  dt3[variable == GHG_var1, "value"] <- dt3[variable == GHG_var1, "value"] * GWP1
  dt3[variable == GHG_var2, "value"] <- dt3[variable == GHG_var2, "value"] * GWP2
  dt3[variable == GHG_var3, "value"] <- dt3[variable == GHG_var3, "value"] * GWP3
  dt3[variable == GHG_var4, "value"] <- dt3[variable == GHG_var4, "value"] * GWP4
  dt3$unit <- "Mt CO2eq"
  dt4 <- dt3[,list(total = sum(value), freq = .N), by = c("model", "Category")]
  
  # GEM-E3 does not have a NoPol scenario, so add fictious one
  if (Scen1 == "NoPOL") 
  { dt4_gem_e3 <- data.frame(GEM_E3, "NoPOL", 0.0001, 1)
    names(dt4_gem_e3) <- c("model", "Category", "total", "freq")
    dt4 <- rbind(dt4, dt4_gem_e3)
  }
  if (Scen2 == "NoPOL") 
  { dt4_gem_e3 <- data.frame(GEM_E3, "NoPOL", 99999999, 1)
  names(dt4_gem_e3) <- c("model", "Category", "total", "freq")
  dt4 <- rbind(dt4, dt4_gem_e3)
  }  
  
  # determine reduction against NoPolicy scenario
  #dt5 <- dt4[Category == Scen1, period == year_subset1, "total" ]/dt4[Category == Scen2, period == year_subset1, "total"]-1
# THIS NEEDS TO BE CHANGED  
  z <- merge(subset(dt4, Category == Scen1),subset(dt4, Category == Scen2), by.x='model', by.y='model')[,c(1,3,6)]
  colnames(z)[2] <- Scen1
  colnames(z)[3] <- Scen2
  #colnames(z)[4] <- scen3
  z <- transform(z, reduction = get(Scen1)/get(Scen2)-1)
  
  x_coord_text=0.7
  y_coord_text=0.1
  #y_min = min(0,roundUpNice(min(z$NoPOL)))
  #y_max = roundUpNice(max(z$NoPOL))
  #y_min=min(0,min(z$NoPOL))
  #y_max=max(z$NoPOL)
  y_max <- switch(SECTOR, 
                  "economy wide"               = 75000,
                  "energy supply"              = 30000,
                  "industry"                   = 15000,
                  "residential and commercial" =  6000, 
                  "transportation"             = 11000,
                  "AFOLU"                      = 15000)
  
  
  #y_max = 6000
  y_txt = "Gt"
print(paste("ymin: ",y_min))
print(paste("ymax: ",y_max))

  # plot data
  data_plot1_1 <- subset(dt4, model==AIM)
  red_1 = round(-1*100*z[z$model==AIM,"reduction"],digits=1)
  ann_1 = grobTree(textGrob(paste("reduction =",red_1,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot1<-ggplot(data_plot1_1, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="AIM/CGE")+annotation_custom(ann_1)+xlab("scenario")+ylab(y_txt)

  data_plot1_2 <- subset(dt4, model==COPPE)
  red_2 = round(-1*100*z[z$model==COPPE,"reduction"],digits=1)
  ann_2 = grobTree(textGrob(paste("reduction =",red_2,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot2<-ggplot(data_plot1_2, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="COPPE-COFFEE")+annotation_custom(ann_2)+xlab("scenario")+ylab(y_txt)       

  data_plot1_3 <- subset(dt4, model==DNE)
  red_3 = round(-1*100*z[z$model==DNE,"reduction"],digits=1)
  ann_3 = grobTree(textGrob(paste("reduction =",red_3,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot3<-ggplot(data_plot1_3, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="DNE21")+annotation_custom(ann_3)+xlab("scenario")+ylab(y_txt)      

  data_plot1_4 <- subset(dt4, model==GEM_E3)
  red_4 = round(-1*100*z[z$model==GEM_E3,"reduction"],digits=1)
  ann_4 = grobTree(textGrob(paste("reduction =",red_4,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot4<-ggplot(data_plot1_4, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="GEM-E3")+annotation_custom(ann_4)+xlab("scenario")+ylab(y_txt)      

  data_plot1_5 <- subset(dt4, model==IMAGE)
  red_5 = round(-1*100*z[z$model==IMAGE,"reduction"],digits=1)
  ann_5 = grobTree(textGrob(paste("reduction =",red_5,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot5<-ggplot(data_plot1_5, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="IMAGE")+annotation_custom(ann_5)+xlab("scenario")+ylab(y_txt)      

  data_plot1_6 <- subset(dt4, model==REMIND)
  red_6 = round(-1*100*z[z$model==REMIND,"reduction"],digits=1)
  ann_6 = grobTree(textGrob(paste("reduction =",red_6,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot6<-ggplot(data_plot1_6, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="REMIND")+annotation_custom(ann_6)+xlab("scenario")+ylab(y_txt)      

  data_plot1_7 <- subset(dt4, model==MESSAGE)
  red_7 = round(-1*100*z[z$model==MESSAGE,"reduction"],digits=1)
  ann_7 = grobTree(textGrob(paste("reduction =",red_7,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot7<-ggplot(data_plot1_7, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="MESSAGE")+annotation_custom(ann_7)+xlab("scenario")+ylab(y_txt)      

  data_plot1_8 <- subset(dt4, model==WITCH)
  red_8 = round(-1*100*z[z$model==WITCH,"reduction"],digits=1)
  ann_8 = grobTree(textGrob(paste("reduction =",red_8,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot8<-ggplot(data_plot1_8, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="WITCH")+annotation_custom(ann_8)+xlab("scenario")+ylab(y_txt)      

  data_plot1_9 <- subset(dt4, model==POLES)
  red_9 = round(-1*100*z[z$model==POLES,"reduction"],digits=1)
  ann_9 = grobTree(textGrob(paste("reduction =",red_9,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="red",fontsize=5,fontface="bold")))
  myplot9<-ggplot(data_plot1_9, aes(x=Category, y=total))+ylim(c(0,y_max))+geom_bar(stat="identity")+labs(title="POLES")+annotation_custom(ann_9)+xlab("scenario")+ylab(y_txt)      

    #g <- grid.arrange(myplot1, myplot2, myplot3, myplot4, myplot5, myplot6, myplot7, myplot8, ncol=4, main=textGrob("CO2 emissions", gp=gpar(fontsize=15,font=8)))
  if (create_graphs == 1)
  { g <- grid.arrange(myplot1, myplot3, myplot4, myplot5, myplot6, myplot7, myplot8, myplot9, ncol=4, top=title_GHG_plot)
    plot(g)
    #fname = paste(GHG, "_", Sys.time(), ".pdf")
    fname_pdf = paste(select_region, GHG, SECTOR, Scen1, year_subset1, Scen2, year_subset2, Sys.Date(),"_", gsub(":", "_", strftime(Sys.time(), format="%H:%M:%S")), ".pdf")
    ggsave(paste(output_dir, fname_pdf), g)
    print(paste("file: ", fname_pdf))
  }
  
  #output to formatted table
  z_table <- xtable(z)
  fname_html = paste(select_region, GHG, SECTOR, Sys.Date(),"_", gsub(":", "_", strftime(Sys.time(), format="%H:%M:%S")), ".html")
  print.xtable(z_table, type="html", file=paste(output_dir,fname_html))
  
  return(z)
  
}

