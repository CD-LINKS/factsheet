# library
library(treemap)
library(d3treeR)
 
# https://www.r-bloggers.com/simple-steps-to-create-treemap-in-r/
# https://www.r-graph-gallery.com/237-interactive-treemap.html

# dataset
group <- c(rep("Stock listed companies",2),rep("Private owned companies",2),rep("Other companies top Transparency Benchmark",2))
#subgroup <- paste("subgroup" , c(1,2,1,2,1,2), sep="-")
subgroup <- c("71 reported", "not-reported", "20 reported", "76 not-reported", "27 reported", "not-reported")
value <- c(71,3,20,76,27,0)
data <- data.frame(group,subgroup,value)
 
# basic treemap
p <- treemap(data,
            index=c("group","subgroup"),
            vSize="value",
            type="index",
            #palette = "Set2",
            #palette = "Blues",
            palette = c("#0080FF", "#FFFFFF", "#00CC00", "#FFFFFF", "#37004D", "#FFFFFF"),
            bg.labels=c("white"),
            align.labels=list(
              c("center", "center"), 
              c("right", "bottom")
            )  
          )            
 
# make it interactive ("rootname" becomes the title of the plot):
inter <- d3tree2( p ,  rootname = "General" )
