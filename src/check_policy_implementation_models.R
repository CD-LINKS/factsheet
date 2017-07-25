# first run main_xCut_wp2.R which creates data frame 'all'

library(gridExtra)
library(ggplot2)
library(scales)
library(grid)
library(data.table)
# alternative, a little easier
# AIM/CGE
# DNE21+ V.14
# IMAGE 3.0
# MESSAGEix-GLOBIOM_1.0
# REMIND-MAgPIE 1.7-3.0
# WITCH2016

# const
AIM = "AIM/CGE"
COPPE = "COPPE-COFFEE 1.0"
DNE = "DNE21+ V.14"
GEM_E3 = "GEM-E3_V1"
IMAGE = "IMAGE 3.0"
REMIND = "REMIND-MAgPIE 1.7-3.0"
MESSAGE = "MESSAGEix-GLOBIOM_1.0"
WITCH = "WITCH2016"

NPi = "NPi_V2"
NoPol = "NoPolicy_V2"

# prepare functions
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#prepare data
data_plot1 <- all
data_plot1$period <- as.numeric(data_plot1$period)
data_plot1 <- data_plot1[!is.na(data_plot1$period),]
#plot1 <- subset(plot1, variable=="Emissions|CO2")
data_plot1 <- subset(data_plot1, variable=="Emissions|CO2" & period == 2030 & (scenario=="NoPolicy_V3" | scenario=="NPi_V3") & region=="World")

z1 = subset(data_plot1, Category=="NPi")
z2 = subset(data_plot1, Category=="NoPOL")
z <- merge(z1,z2,by.x='model', by.y='model')[,c(1,8,17)]
setnames(z, "value.x", "NPi")
setnames(z, "value.y", "NoPolicy")
z <- transform(z, reduction = NPi/NoPolicy-1)

x_coord_text=0.7
y_coord_text=0.1
y_max = roundUpNice(max(z2["value"]))
y_txt = "Gt"

# plot data
data_plot1_1 <- subset(data_plot1, model==AIM)
red_1 = round(-1*100*x[x[,"model"]==AIM,]["reduction"],digits=1)
ann_1 = grobTree(textGrob(paste("red. to NoPol =",red_1,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot1<-ggplot(data_plot1_1, aes(Category, value))+geom_bar(stat="identity")+labs(title="AIM/CGE")+annotation_custom(ann_1)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)

data_plot1_2 <- subset(data_plot1, model==COPPE)
red_2 = round(-1*100*x[x[,"model"]==COPPE,]["reduction"],digits=1)
ann_2 = grobTree(textGrob(paste("red. to NoPol =",red_2,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot2<-ggplot(data_plot1_2, aes(Category, value))+geom_bar(stat="identity")+labs(title="COPPE-COFFEE")+annotation_custom(ann_2)+xlab("scenario")+ylim(0,y_max)+ylab(y_txt)       

data_plot1_3 <- subset(data_plot1, model==DNE)
red_3 = round(-1*100*x[x[,"model"]==DNE,]["reduction"],digits=1)
ann_3 = grobTree(textGrob(paste("red. to NoPol =",red_3,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot3<-ggplot(data_plot1_3, aes(Category, value))+geom_bar(stat="identity")+labs(title="DNE21")+annotation_custom(ann_3)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

data_plot1_4 <- subset(data_plot1, model==GEM_E3)
red_4 = round(-1*100*x[x[,"model"]==GEM_E3,]["reduction"],digits=1)
ann_4 = grobTree(textGrob(paste("red. to NoPol =",red_4,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot4<-ggplot(data_plot1_4, aes(Category, value))+geom_bar(stat="identity")+labs(title="GEM-E3")+annotation_custom(ann_4)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

data_plot1_5 <- subset(data_plot1, model==IMAGE)
red_5 = round(-1*100*x[x[,"model"]==IMAGE,]["reduction"],digits=1)
ann_5 = grobTree(textGrob(paste("red. rel NoPol =",red_5,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot5<-ggplot(data_plot1_5, aes(Category, value))+geom_bar(stat="identity")+labs(title="IMAGE")+annotation_custom(ann_5)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

data_plot1_6 <- subset(data_plot1, model==REMIND)
red_6 = round(-1*100*x[x[,"model"]==REMIND,]["reduction"],digits=1)
ann_6 = grobTree(textGrob(paste("red. rel NoPol =",red_6,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot6<-ggplot(data_plot1_6, aes(Category, value))+geom_bar(stat="identity")+labs(title="REMIND")+annotation_custom(ann_6)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

data_plot1_7 <- subset(data_plot1, model==MESSAGE)
red_7 = round(-1*100*x[x[,"model"]==MESSAGE,]["reduction"],digits=1)
ann_7 = grobTree(textGrob(paste("red. rel NoPol =",red_7,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot7<-ggplot(data_plot1_7, aes(Category, value))+geom_bar(stat="identity")+labs(title="MESSAGE")+annotation_custom(ann_7)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

data_plot1_8 <- subset(data_plot1, model==WITCH)
red_8 = round(-1*100*x[x[,"model"]==WITCH,]["reduction"],digits=1)
ann_8 = grobTree(textGrob(paste("red. rel NoPol =",red_8,"%"),x=x_coord_text, y=y_coord_text,hjust=0,rot=90,gp=gpar(col="yellow",fontsize=10,fontface="bold")))
myplot8<-ggplot(data_plot1_8, aes(Category, value))+geom_bar(stat="identity")+labs(title="WITCH")+annotation_custom(ann_8)+ylim(0,y_max)+xlab("scenario")+ylab(y_txt)      

#g <- grid.arrange(myplot1, myplot2, myplot3, myplot4, myplot5, myplot6, myplot7, myplot8, ncol=4, main=textGrob("CO2 emissions", gp=gpar(fontsize=15,font=8)))
g <- grid.arrange(myplot1, myplot2, myplot3, myplot4, myplot5, myplot6, myplot7, myplot8, ncol=4, top="CO2 emissions")
plot(g)
ggsave('CO2.pdf', g)