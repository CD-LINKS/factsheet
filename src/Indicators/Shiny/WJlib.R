
library(reshape2);
library(ggplot2);
library(plyr);
library(plyr)
library(dplyr)
library(rmarkdown)
library(data.table)
library(knitr)
#library(tidyr)
library(ggplot2)
library(grid)

# Add perhaps, but test first:
# https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart

find_col_year <- function(df, yr) {
  # Finds 
  clnr=c(0,0)
  for (i in 1970:2100) { # finding first year occurrence.
    clnr[2] = match(paste("X", yr, sep=""), colnames(df), nomatch = 0)
    clnr[2] = match(yr, colnames(df), nomatch = 0)
    if (clnr != 0) {break}
  }
  
  return(clnr)
}

# lm_eqn <- function(df, var, var2){
#   m <- lm(var ~ var2, df);
#   eq <- substitute(italic(var) == a + b %.% italic(var2)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }

scenario_range <- function(df, yr) {  

  scens <- levels(df$Scenario)
  
  yr_data <- subset(df, Year==yr)

  yr_data1 <- subset(yr_data, Scenario==scens[1])
  yr_data1$Year <- yr_data1$Year + 2
  yr_data2 <- subset(yr_data, Scenario==scens[2])
  yr_data2$Year <- yr_data2$Year + 4
  yr_data3 <- subset(yr_data, Scenario==scens[3])
  yr_data3$Year <- yr_data3$Year + 6
  yr_data4 <- subset(yr_data, Scenario==scens[4])
  yr_data4$Year <- yr_data4$Year + 8
  yr_data5 <- subset(yr_data, Scenario==scens[5])
  yr_data5$Year <- yr_data5$Year + 10
  
  yr_data <- rbind(yr_data1, yr_data2, yr_data3, yr_data4, yr_data5)
  
    
  return(yr_data)
}



my_dataread <- function(file_list, name_list = FALSE) {
  
  filecount = length(file_list)
  
  for(i in 1:filecount){
   
    f <- file_list[i]
    fext <- tolower(file_ext(file_list[i])[1])
    if(fext == "out" |fext == "scn"|fext == "dat") {image_file<-TRUE}else{image_file<-FALSE}

    if (fext == "rda") { 
      DATAtmp <- readRDS(f)
    } else if (fext == "out" |fext == "scn") {
      DATAtmp <- read.mym2r(f)
    } else {
      #DATAtmp <- read.csv(f, sep=",", dec=".")
      DATAtmp <- read.csv(f, sep=";", dec=".")
      if(ncol(DATAtmp)==1) {
        #DATAtmp <- read.csv(f, sep=",", dec=".")
        DATAtmp <- read.csv(f, sep=";", dec=".")
      }
    }
    
    DATAtmp <- data_cleaner(DATAtmp)
    
    if(filecount>1){
      if (name_list!=FALSE){
        DATAtmp$file_name <- name_list[i]
      } else {
        DATAtmp$file_name <- f
      }
      # Moving new filename column to front
      DATAtmp <- DATAtmp[,c(ncol(DATAtmp),1:(ncol(DATAtmp)-1))]
      if(i==1){
        DATA <- DATAtmp
      } else {
        if(image_file) {
          DATA <- rbind.fill(DATA, DATAtmp) #allow nonoverlapping column names
        } else {
          DATA <- rbind(DATA, DATAtmp)
        }
      }
    } else {
      DATA <- DATAtmp
    }
    
    if(image_file){
      DATA$file_name <- NULL
    }
    
    if("value" %in% colnames(DATA)) { #moving value to the back
      col_idx <- grep("value", names(DATA))
      DATA <- DATA[, c((1:ncol(DATA))[-col_idx],col_idx)]
    }
    
  }
  return(DATA)
}

plot_data_wj <- function(df,input,scaling="None", yr=2010, scalingby="", Facet="None"){
  
  colcount <- ncol(df) - 2
  cln <- colcount + 1
  plot_levels <- colnames(df)
  
  for (i in 1:length(plot_levels)) {
    # Double brackets for input needed because it is a reactivevalues class
    # Doing year after scaling
    if(!(plot_levels[i] == "Year")) { # 
      if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
      if(!("All" %in% input[[plot_levels[i]]])){df[,plot_levels[i]] <- factor(df[,plot_levels[i]], levels=input[[plot_levels[i]]])}
    }   
  }
  
  df <- droplevels(df) # necedfary because empty levels might be left over after subsetting.
  df$Year = as.integer(as.character(df$Year))
  
  if (scalingby != "") { # & scaling == "Absolute") {
    rowvar <- unlist(strsplit(scalingby, ";"))[2]
    col <- unlist(strsplit(scalingby, ";"))[1]
    scale <- df[df[,col]==rowvar & df$Year == yr,]
    scale = scale$value # will be multiple values in case of facets (or wrong input)
  }
  
  if(scaling == "Absolute"){
    if ("index" %in% colnames(df) & "value" %in% colnames(df)){
      df$index <- NULL
      cln = cln - 1
      colcount = colcount - 1
    }
    df <- spread(df, Year, value)
    if(scalingby != "" & Facet != "None") {
      rows <- unique(df[,Facet])
      for (i in 1:length(rows)) {
        dfsub = subset(df, df[,Facet]==rows[i])
        scale <- dfsub[dfsub[,col]==rowvar,]
        scale = scale[,as.character(yr)]
        dfsub[,cln:ncol(dfsub)] <- dfsub[,cln:ncol(dfsub)] - dfsub[,as.character(yr)] + scale
        df <- rbind(dfsub, subset(df, df[,Facet]!=rows[i]))
      }
    } else {
      df[,cln:ncol(df)] <- df[,cln:ncol(df)] - df[,as.character(yr)] + scale
    }
    
    df <- melt(df, id.vars=1:colcount, variable.name="Year")
    df <- na.omit(df)
    df$Year <- as.numeric(substr(df$Year, 1, stop=100))
  }
  if(scaling == "Relative"){
    if ("index" %in% colnames(df) & "value" %in% colnames(df)){
      df$index <- NULL
      cln = cln - 1
      colcount = colcount - 1
    }
    df <- spread(df, Year, value)
    
    if(scalingby != "" & Facet != "None") {
      rows <- unique(df[,Facet])
      for (i in 1:length(rows)) {
        dfsub = subset(df, df[,Facet]==rows[i])
        scale <- dfsub[dfsub[,col]==rowvar,]
        scale = scale[,as.character(yr)]
        dfsub[,cln:ncol(dfsub)] <- scale * dfsub[,cln:ncol(dfsub)]/dfsub[,as.character(yr)] 
        df <- rbind(dfsub, subset(df, df[,Facet]!=rows[i]))
      }
    } else {
      df[,cln:ncol(df)] <- df[,cln:ncol(df)]/df[,as.character(yr)]
    }
    
    
    df <- melt(df, id.vars=1:colcount, variable.name="Year")
    df <- na.omit(df)
    df$Year <- as.numeric(substr(df$Year, 1, stop=100))
  }
  
  for (i in 1:length(plot_levels)) {
    # Double brackets for input needed because it is a reactivevalues cladf
    # Doing year after scaling
    if(plot_levels[i] == "Year") { # 
      if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
    }   
  }
  
  return(df)
}

data_cleaner <- function(df){
  
  #Multiple claenup of names/headers units, etc.
  #Also tries to detect years in column names to convert to long format
  #Creates AgMIP item & variable columns if detected
  
  #Renaming some columns
  df <- plyr::rename(df, c("region"="Region", "model"="Model", "variable"="Variable", "scenario"="Scenario", "year"="Year", "unit"="Unit", "item"="Item"), warn_missing=FALSE)
  df <- plyr::rename(df, c("Index"="index", "Value"="value"), warn_missing=FALSE)
  df <- plyr::rename(df, c("REGION"="Region", "MODEL"="Model", "VARIABLE"="Variable", "SCENARIO"="Scenario", "YEAR"="Year", "UNIT"="Unit", "ITEM"="Item"), warn_missing=FALSE)
  df <- plyr::rename(df, c("INDEX"="index", "INDEX"="value"), warn_missing=FALSE)
  
  #converting years if found
  if (!("Year" %in% colnames(df))){
    if ("X2010" %in% colnames(df)|"X2005" %in% colnames(df)|"X2015" %in% colnames(df)|"X2000" %in% colnames(df)|"X1970" %in% colnames(df)) {
      for (i in 1970:2100) { # finding first year occurrence.
        clnr = match(paste("X", i, sep=""), colnames(df), nomatch = 0)
        if (clnr != 0) {break}
      }
      df <- melt(df, id=1:clnr-1, variable.name = "Year")
      df$Year = as.numeric(substr(df$Year, 2, stop=100))      
    }
  }
  
  # Just to be sure, not sure if all of the below is necessary.
  if("value" %in% colnames(df)) {
    df$value <- as.numeric(substr(df$value, 1, stop=100))
  }
  if("index" %in% colnames(df)){
    df$index <- as.numeric(df$index)
  }
  if("Year" %in% colnames(df)) {
    df$Year = as.integer(as.character(df$Year))
  }
  
  #Making agmip columsn ig "AGMIP| etc is detected
  if("Variable" %in% colnames(df)) {
    if (!("Item" %in% colnames(df)) & substr(df$Variable[1],1,5) == "AGMIP") {
      df$Variable <- gsub("AGMIP|", "", df$Variable, fixed=TRUE)
      df$Item <- gsub("\\|.*", "", df$Variable) # Everything after | removed
      df$Variable <- gsub(".*\\|", "", df$Variable)
    }
  }
  # if ("Unit" %in% colnames(df) & "Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
  #   df$Variable_AgMIP <- factor(paste(df$Variable ,"_", df$Item, "_", df$Unit, sep=""))
  # }
  if ("Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
    # To do, make uppercase
    df$Variable = gsub("^Area$", "AREA", df$Variable)
    df$Variable = gsub("^Prod$", "PROD", df$Variable)
    df$Variable = gsub("^Feed$", "FEED", df$Variable)
    df$Variable = gsub("^food$", "FOOD", df$Variable)
    df$Variable = gsub("^Food$", "FOOD", df$Variable)
    # df$Variable = gsub("Area", "AREA", df$Region)
    # df$Variable = gsub("Area", "AREA", df$Region)
  }
  
  #some general renaming
  # if("Region" %in% colnames(df)){
  #   df$Region = gsub("R5.2", "", df$Region)
  #   df$Region = gsub("Global", "WLD", df$Region)
  #   df$Region = gsub("^World$", "WLD", df$Region)
  #   df$Region = gsub("Total", "WLD", df$Region)
  # }
  if("Unit" %in% colnames(df)){
    df$Unit = gsub("Mt CO2e", "MtCO2e",df$Unit)
    df$Unit = gsub("kcal/cap/day", "kcal/cap/d",df$Unit)
    df$Unit = gsub("1000 Ha", "1000 ha",df$Unit)
  }
  if("Item" %in% colnames(df)){
    df$Item = gsub("wheat", "Wheat", df$Item)
    df$Item = gsub("barley", "Barley", df$Item)
    df$Item = gsub("grain maize", "Maize", df$Item)
    df$Item = gsub("maize", "Maize", df$Item)
    df$Item = gsub("oats", "Oats", df$Item)
    df$Item = gsub("other cereals", "Cereals", df$Item)
    df$Item = gsub("groundnuts", "Groundnuts", df$Item)
    df$Item = gsub("paddy rice", "Rice", df$Item)
    df$Item = gsub("rice", "Rice", df$Item)
    df$Item = gsub("rye", "Rye", df$Item)
    df$Item = gsub("millet", "Millet", df$Item)
    df$Item = gsub("sorghum", "Sorghum", df$Item)
    df$Item = gsub("Soyabeans", "Soybeans", df$Item)
    df$Item = gsub("^Soybean$", "Soybeans", df$Item)
    df$Item = gsub("^soybean$", "Soybeans", df$Item)
    df$Item = gsub("^soybeans$", "Soybeans", df$Item)
    df$Item = gsub("Palm oil  /", "Palm oil", df$Item)
    df$Item = gsub("oil palm fruit", "Palm oil", df$Item)
    df$Item = gsub("groundnut$", "Groundnuts", df$Item)
    df$Item = gsub("raw cotton", "Seed cotton", df$Item)
    df$Item = gsub("^cotton", "Seed cotton", df$Item)
    df$Item = gsub("oilpalm", "Palm oil", df$Item)
    df$Item = gsub("^potatoes$", "Potatoes", df$Item)
    df$Item = gsub("^potato$", "Potatoes", df$Item)
    df$Item = gsub("sweet potato and yam", "Sweet potatoes", df$Item)
    df$Item = gsub("Sweet Potatoes", "Sweet potatoes", df$Item)
    df$Item = gsub("rapeseed", "Rape seed", df$Item)
    df$Item = gsub("rape and mustardseed", "Rape seed", df$Item)
    df$Item = gsub("sunflower seed", "Sunflower seed", df$Item)
    df$Item = gsub("sesame seed", "Sesame seed", df$Item)
    df$Item = gsub("sunflower$", "Sunflower seed", df$Item)
    df$Item = gsub("other oilseeds", "Other oilcrops", df$Item)
    df$Item = gsub("cassava", "Cassava", df$Item)
    df$Item = gsub("sugarbeet", "Sugar beet", df$Item)
    df$Item = gsub("sugar beet", "Sugar beet", df$Item)
    df$Item = gsub("sugarcane", "Sugar cane", df$Item)
    df$Item = gsub("sugar cane", "Sugar cane", df$Item)
    df$Item = gsub("cocoa beans", "Cocoa beans", df$Item)
    df$Item = gsub("citrus fruits", "Citrus fruit", df$Item)
    df$Item = gsub("^tea$", "Tea", df$Item)
    df$Item = gsub("coconuts", "Coconuts", df$Item)
    df$Item = gsub("olives", "Olives", df$Item)
    df$Item = gsub("tobacco", "Tobacco", df$Item)
    df$Item = gsub("natural rubber", "Rubber", df$Item)
    df$Item = gsub("plantains", "Plantains", df$Item)
    df$Item = gsub("bananas", "Bananas", df$Item)
    df$Item = gsub("other crops", "Other crops", df$Item)
    df$Item = gsub("dried pulses", "Pulses", df$Item)
    df$Item = gsub("coffee, green", "Coffee green", df$Item)
    df$Item = gsub("other vegetables", "Other vegetables", df$Item)
    df$Item = gsub("other fruits", "Other fruit", df$Item)
    df$Item = gsub("other fibres\ crops", "Other fibre crops", df$Item)
    df$Item = gsub("other fibres crops", "Other fibre crops", df$Item)
    df$Item = gsub("other roots and tubers", "Other roots and tubers", df$Item)
    #df$Item = gsub("Cereals", "Cereal", df$Item)
  }
  
  df = na.omit(df)
  
  #This makes sure that all non-numeric columns are factorized.
  df <- df %>% mutate_if(is.character,as.factor)
  
  if("value" %in% colnames(df)) {
    df$value <- as.numeric(df$value)
    col_idx <- grep("value", names(df))
    #moves value to the back
    df <- df[, c((1:ncol(df))[-col_idx],col_idx)]
  }
  
  df <- df[!duplicated(df),]
  
  return(df)
}

mapping_aggregation <- function(DATA){
  
  df <- "
Variable,PerVariable
YILD,AREA
YIRF,ARRF
YIIR,ARIR
YEXO,AREA
YENDO,AREA
YSTRUC,AREA
YENDOSTRUC,AREA
CropInt,LAND
IrrPerc,LAND
CropInt_IR,LAND_IR
CropInt_RF,LAND_RF
IrrPerc,LAND
YILD_ATT,AREA
YILD_POT,AREA
YIIR_ATT,ARIR
YIIR_POT,ARIR
YIRF_ATT,ARRF
YIRF_POT,ARRF"
 
  map_ag <- read.delim(textConnection(df), header = TRUE, sep = ",")
  
  #currently works for AgMIP only.
  
  df_ex <- DATA

  # First loop calculate totals per variable
  for (i in 1:dim(map_ag)[1]) {
    mapvar <- toString(map_ag$Variable[i])
    pervar <- toString(map_ag$PerVariable[i])
    
    if (mapvar %in% DATA$Variable) {
      
      ss <- subset(DATA, Variable == mapvar | Variable == pervar)
      ncol = length(colnames(ss))-2
      ss <- spread(ss, Variable, value)
      
      ss <- mapping_crops(ss, "AgMIP","Crop")
      sscer <- subset(ss, Item=="WHT" | Item=="CGR" | Item=="RIC")
      if(nrow(sscer)>0){
        sscer$Item <- "Cereals"
        ss <- rbind(sscer,ss)
      }
      sscrp <- subset(ss, Item=="OCR" |Item=="OSD" |Item=="SGC" |Item=="VFN" |Item=="WHT" | Item=="CGR" | Item=="RIC")
      if(nrow(sscrp)>0){
        sscrp$Item <- "CRP"
        ss <- rbind(sscrp,ss)
      }
      ss <- mapping_regions(ss, "AgMIP.13","Country")
      
      ss[,mapvar] <- ss[,mapvar] * ss[,pervar]
      ss <- aggregate(as.formula(paste("cbind(", mapvar,",",pervar,")~.", sep="")), data=ss, sum)
      ss[,mapvar] <- ss[,mapvar] / ss[,pervar]
      
      ss[,pervar] <- NULL

      ss <- melt(ss, id=1:ncol, variable.name = "Variable")  
      
      df_ex <- subset(df_ex, Variable != mapvar)
      
      # print(summary(ss))
      # ss_mapvar <- subset(DATA, Variable == mapvar)
      # ss_pervar <- subset(DATA, Variable == toString(map_ag$PerVariable[i]))
      # ss_mapvar$value <- ss_mapvar$value * ss_pervar$value
      # DATA <- rbind(subset(DATA, Variable != mapvar), ss)
      
      # This is wrong, only gets triggered when YILD & AREA is in there
      if (i==1) {
        df_ag = ss
      } else {
        df_ag = rbind(df_ag, ss)
      }
    }
  }
  
  # set divdes by zero to zero (assuming there are no nas comming in)
  df_ag[is.na(df_ag)] <- 0
  # df_ag <- rbind(df_ag, subset(DATA, !(Variable %in% map_ag$Variable | Variable %in% map_ag$PerVariable)))
  
  df_ex <- mapping_crops(df_ex, "AgMIP","Crop")
  df_excer <- subset(df_ex, Item=="WHT" | Item=="CGR" | Item=="RIC")
  if(nrow(df_excer)>0){
    df_excer$Item <- "Cereals"
    df_ex <- rbind(df_excer,df_ex)
  }
  df_excrp <- subset(df_ex, Item=="OCR"|Item=="OSD"|Item=="SGC"|Item=="VFN" |Item=="WHT"|Item=="CGR"|Item=="RIC")
  if(nrow(df_excrp)>0){
    df_excrp$Item <- "CRP"
    df_ex <- rbind(df_excrp,df_ex)
  }
  df_ex <- mapping_regions(df_ex, "AgMIP.13", "Country")
  df_ex <- aggregate(value~., data=df_ex, sum)
  
  # DATA <- aggregate(value~., data=DATA, sum)
  #  # summing similar data
  # 
  # # Seccond for calculation after summing
  # for (i in 1:dim(map_ag)[2]) {
  #   mapvar <- toString(map_ag$Variable[i])
  #   pervar <- toString(map_ag$PerVariable[i])
  #   ss_mapvar <- subset(DATA, Variable == mapvar)
  #   ss_pervar <- subset(DATA, Variable == pervar)
  #   ss_mapvar$value <- ss_mapvar$value / ss_pervar$value
  #   
  #   ss <- subset(DATA, Variable == mapvar | Variable == pervar)
  #   # 
  #   ss <- spread(ss, Variable, value)
  #   DATA[,mapvar] <- DATA[,mapvar] / DATA[,pervar]
  #   ncol = length(colnames(ss))-2
  #   ss <- melt(ss, id=1:ncol, "Variable")  
  #   # 
  #   DATA <- rbind(subset(DATA, Variable != mapvar), ss)
  # }
  # 
  # return(DATA)
  return(rbind(df_ag, df_ex))
  
}

mapping_crops <- function(df, to, from){
  
  map_table <- "
Crop,Image,AgMIP
CRP,CRP,CRP
Rice,Rice,RIC
Maize,Maize,CGR
Wheat,Temperate cereals,WHT
Barley,Temperate cereals,CGR
Oats,Temperate cereals,CGR
Rye,Temperate cereals,CGR
Sorgum,Tropical cereals,CGR
Sorghum,Tropical cereals,CGR
Millet,Tropical cereals,CGR
Beans,Pulses,VFN
Pulses,Pulses,VFN
Potatoes,Roots & tubers,VFN
Oilcrops,Oil crops,OSD
Other oilcrops,Oil crops,OSD
Soybeans,Oil crops,OSD
Other cereals, ,CGR
Sweet potatoes, ,VFN
Cassava, ,VFN
Other roots, ,VFN
Other roots and tubers, ,VFN
Plantains, ,VFN
Sugar beet, ,SGC
Sugar cane, ,SGC
Pulses, ,VFN
Vegetables, ,VFN
Bananas, ,VFN
Citrus fruit, ,VFN
Other fruit, ,VFN
Other vegetable crops, ,VFN
Other vegetables, ,VFN
Oilcrops nes, ,OSD
Rape seed, ,OSD
Palm oil, ,OSD
Groundnuts, ,OSD
Sunflower seed, ,OSD
Sesame seed, ,OSD
Coconuts, ,OSD
Olives, ,OSD
Cocoa beans, ,OCR
Coffee green, ,OCR
Other crops, ,OCR
Tea, ,OCR
Tobacco, ,OCR
Seed cotton, ,PFB
Hard fibres, ,PFB
Other fibre crops, ,PFB

"
  map_table <- read.delim(textConnection(map_table), header = TRUE, sep = ",")
  
  df_map <- subset(df, Item %in% map_table$Crop)
  df <- subset(df, !(Item %in% map_table$Crop))
  
  df_map$Item <- map_table$AgMIP[match(df_map$Item, map_table$Crop)]
  
  # for (i in 1:dim(map_table)[1]) {
  #   print(i)
  #   df$Region <- gsub(toString(map_table[i,from]), toString(map_table[i,to]), df$Region)
  # }
  
  return(rbind(df, df_map))
}


mapping_regions <- function(df, to, from, jd = TRUE) {
  
  region_map <- read.csv("/mnt/public/zeistvw/ontwapps/IMAGE/users/zeistvw/R Data Processing Scripts/All regions mapping.csv",sep=",", dec=".")
  
  df_map <- subset(df, Region %in% region_map$Country)
  df <- subset(df, !(Region %in% region_map$Country))
  df_map$Region <- region_map$AgMIP.13[match(df_map$Region, region_map$Country)]
  df <- rbind(df_map, df)
  
  # incase of image regions map those as well
  df_map <- subset(df, Region %in% region_map$IMAGE.region.26)
  df <- subset(df, !(Region %in% region_map$IMAGE.region.26))
  df_map$Region <- region_map$AgMIP.13[match(df_map$Region, region_map$IMAGE.region.26)]
  df <- rbind(df_map, df)
  
  # For mueller o.a. regions map those as well
  # df_map <- subset(df, Region %in% region_map$Other)
  # df <- subset(df, !(Region %in% region_map$Other))
  # df_map$Region <- region_map$AgMIP.13[match(df_map$Region, region_map$Other)]
  # df <- rbind(df_map, df)
  
  if (jd) {
    df_map <- subset(df, Region %in% region_map$AgMIP.13)
    df <- subset(df, !(Region %in% region_map$AgMIP.13))
    df_map$Region <- region_map$Doelman2018[match(df_map$Region, region_map$AgMIP.13)]
    df <- rbind(df_map, df)
    
    # in case leftover map the FAO baseline aggregate regions
    df_map <- subset(df, Region %in% region_map$FAO_Baseline)
    df <- subset(df, !(Region %in% region_map$FAO_Baseline))
    df_map$Region <- region_map$Doelman2018[match(df_map$Region, region_map$FAO_Baseline)]
    df <- rbind(df_map, df)
    
    # in case leftover map the FAO baseline 2018 aggregate regions
    df_map <- subset(df, Region %in% region_map$FAO_Baseline_2018)
    df <- subset(df, !(Region %in% region_map$FAO_Baseline_2018))
    df_map$Region <- region_map$Doelman2018[match(df_map$Region, region_map$FAO_Baseline_2018)]
    df <- rbind(df_map, df)
  }
  
   # for (i in 1:dim(map_table)[1]) {
  #   print(i)
  #   df$Region <- gsub(toString(map_table[i,from]), toString(map_table[i,to]), df$Region)
  # }
  return(df)
}


add_regions <- function(df, to, from, jd = TRUE) {
  
  map_table <- read.csv("/mnt/public/zeistvw/ontwapps/IMAGE/users/zeistvw/R Data Processing Scripts/All regions mapping.csv",sep=",", dec=".")
  df <- map_table[[to]][match(df, map_table[[from]])]
  return(df)
}


## mym read functions stolen/adapted from David Gernaat:
read.mym2r.dims = function(path.to.mym.file){
  # utility function to extract only the dimensions from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  # extract the dimensions
  dims = numeric(0)
  if (grepl(pattern='[[]+.+[]]+',oneline)){
    dimstring = substr(x=oneline, start=regexpr('[',oneline,fixed=TRUE)[1]+1, stop=regexpr(']',oneline,fixed=TRUE)[1]-1)
    dims      = as.numeric(strsplit(dimstring,',')[[1]])
  }
  return(dims)
}

read.mym2r.varname = function(path.to.mym.file){
  # utility function to extract only the varname from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  varname = NA
  # extract variable name used in MyM
  if (grepl(pattern='\\s.+[[(]',oneline)) 
    varname = substr(x=oneline, start=regexpr('\\s\\S+[[(]',oneline)[1]+1, stop=regexpr('[[(]',oneline)[1]-1)
  varname <- gsub('real ','',varname)
  return(varname)
}

read.mym2r.dimnames = function(path.to.mym.file){
  # utility function to extract only the dimensions from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1) # they should be only on the first line after a comment field
  close(con)
  
  # extract the dimensions
  dims = ""
  
  if(!grepl('!', oneline)) {
    dims = paste('Dim', read.mym2r.dims(path.to.mym.file), sep="")
    return(dims)
  }
  
  if (grepl(pattern='[[]+.+[]]+',oneline)){
    dimstring = substr(x=oneline, start=regexpr('[',oneline,fixed=TRUE)[1]+1, stop=regexpr(']',oneline,fixed=TRUE)[1]-1)
    dims      = as.character(strsplit(dimstring,',')[[1]])
  }
  return(dims)
}

read.mym2r = function(path.to.mym.file, yearheader='year', yearsrun=NULL){
  # input:  path to MyM output file
  # output: R dataframe in 'long' format (many rows, each dimension as a 'factor' in its column, and only one column with actual values)
  
  varname = read.mym2r.varname(path.to.mym.file)
  dims    = read.mym2r.dims(path.to.mym.file)
  dimnames = read.mym2r.dimnames(path.to.mym.file)
  
  for (d in 1:length(dimnames)) {
    if (dimnames[d] == "27"){dimnames[d]="NRT"}
  }
  #dimnames_long = lapply(dimnames,lookup.mym.dimnameslong)
  
  # read all the data
  line      = readLines(path.to.mym.file, warn = FALSE)
  
  # skip any commented lines at the start
  while (1 < length(line)){
    if (grepl(pattern='[:blank:]*[!]',line[1])){
      line = line[2:length(line)]   # this might not be very efficient but it is clear: we discard the first line if it is only a comment
    } else {
      break
    }
  }
  
  # extract whether there is a time dimension or not
  has.time.dim = grepl(pattern='(t)',line[1],fixed=TRUE)
  
  # convert all lines to one long vector of numbers, including years
  string = paste0(line, collapse = ',')
  string = strsplit(string, split='=', fixed=TRUE) # now a list of 1 header and 1 content string
  content = string[[1]][2]
  # remove [ ] ;
  content = gsub('[','',content, fixed=TRUE)  
  content = gsub(']','',content, fixed=TRUE)  
  content = gsub(';','',content, fixed=TRUE)  
  # convert to vector of values
  content = strsplit(content, '[[:space:],]')[[1]]
  content = content[content != '']  # remove empty strings
  content = as.numeric(content)
  
  # create a dataframe that exactly matches the format of the MyM file
     # note: last MyM iterator comes first with rev(dims)
  if (length(dims)>=1) {
    dim.dummies = rev(dimnames)
  }
  
  #ret = expand.grid( lapply(rev(dims), FUN=function(x){return(1:x)}) )
  ret = expand.grid(mapply(lookup.mym.dimlabels, rev(dimnames), dim=rev(dims)))
  
  if (has.time.dim && length(dims)>=1){
    
    # add one column of numbers for each year
    ncols  = length(content)/(nrow(ret)+1)   # +1 for the year numbers themselves
    m      = matrix(content, nrow=nrow(ret)+1, ncol=ncols)
    years  = m[1,]
    values = m[2:nrow(m),]
    ret    = cbind(ret, values)
    
    colnames(ret) = c(dim.dummies,years)
    
    ret = reshape2::melt(ret, id.vars=dim.dummies, value_name = 'value', variable.name = yearheader)      # not needed if you wanted wide format output, but default is long format
    ret = ret[c((length(dims)+1):1, ncol(ret))]                                                           # rearrange columns to match the order in MyM
    
  } else if (has.time.dim && length(dims)==0){
    
    m   = matrix(content, ncol = 2, byrow = TRUE)
    ret = data.frame(m)
    colnames(ret) = c(yearheader,'value')
    
  } else {  # timeless
    ret$value     = content                                                # join the values from the MyM file to the prepared dataframe
    colnames(ret) = c(dim.dummies,'value')            
    ret           = ret[c(length(dims):1, ncol(ret))]                  # rearrange columns to match the order in MyM
  }
  
  # additional info:
  ret$Varname = varname
  if (has.time.dim) ret[yearheader] = as.numeric(as.character(ret[[yearheader]]))  # make sure years are numeric
  return(ret)
}


lookup.mym.dimlabels = function(dimname, dim){
  # retrieve default dimlabels, based on varname.
  
  dimlabels = switch(dimname, 
                 'IPCC_REGIONS'=c('OECD90','REF','ASIA','ALM','World'),
                 'NA'=c('non_dairy cattle','dairy cattle','pigs','sheep & goats','poultry'),
                 'NAC'=c('food crops','grass & fodder','biofuel crops'),
                 'NACT'=c('food crops','grass & fodder','biofuel crops','Total'),
                 'NAP'=c('beef','milk','pork','mutton & goat meat','poultry & eggs'),
                 'NAPO'=c('beef','milk','pork','mutton & goat meat','poultry & eggs','Other'),
                 'NAPT'=c('beef','milk','pork','mutton & goat meat','poultry & eggs','Total'),
                 'NAS'=c('cattle for meat','pigs for meat','sheep & goat for meat'),
                 'NAT'=c('non_dairy cattle','dairy cattle','pigs','sheep & goats','poultry','Total'),
                 'NBC'=c('sugar cane','maize','woody biofuels','non woody biofuels'),
                 'NBCO'=c('sugar cane','maize','woody biofuels','non woody biofuels','Other'),
                 'NBCOT'=c('sugar cane','maize','woody biofuels','non woody biofuels','Other','All'),
                 'NBCT'=c('sugar cane','maize','woody biofuels','non woody biofuels','All'),
                 'NBFC'=c('wheat (spring)','wheat (winter)','rice','maize (temperate)','maize (tropical)','millet (temperate)','millet (tropical)','sorghum (temperate)','sorghum (tropical)','beans (temperate)','beans (tropical)','cassava','potato','groundnut','sesame','soya bean','sunflower (temperate)','sunflower (tropical)','sugar cane','legume I','legume II','grass III','grass IV','wood species group I','wood species group II'),
                 'NFBC'=c('temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops','sugar cane','maize','woody biofuels','non woody biofuels'),
                 'NFC'=c('temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops'),
                 'NFCO'=c('temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops','Other'),
                 'NFCT'=c('temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops','All'),
                 'NFERT'=c('synthetic N-fertilizer','synthetic PK-fertilizer','synthetic NPK','manure  N-fertilizer','manure  PK-fertilizer','manure  NPK-fertilizer'),
                 'NFERT_22'=c('synthetic N-fertilizer','synthetic PK-fertilizer','manure  NPK-fertilizer'),
                 'NFERTT'=c('synthetic N-fertilizer','synthetic PK-fertilizer','synthetic NPK','manure  N-fertilizer','manure  PK-fertilizer','manure  NPK-fertilizer','Total'),
                 'NFERTT_22'=c('synthetic N-fertilizer','synthetic PK-fertilizer','manure  NPK-fertilizer','All'),
                 'NFFBC'=c('grass & fodder','temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops','sugar cane','maize','woody biofuels','non woody biofuels'),
                 'NFFC'=c('grass & fodder','temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops'),
                 'NFOC'=c('food crops','animal products'),
                 'NFOCT'=c('food crops','animal products','Total'),
                 'NFP'=c('food crops','animal products','residues','scavenging','grass & fodder'),
                 'NFP_22'=c('food crops','animal products','residues','grass & fodder'),
                 'NFPT'=c('food crops','animal products','residues','scavenging','grass & fodder','Total'),
                 'NFPT_22'=c('food crops','animal products','residues','grass & fodder','Total'),
                 'NFRAC'=c('grass rainfed','temperate cereals','rice','maize','tropical cereals','pulses','roots & tubers','oil crops','biofuel sugar cane','biofuel maize','woody biofuels','non-woody biofuels','irrigated temperate cereals','irrigated rice','irrigated maize','irrigated tropical cereals','irrigated pulses','irrigated roots & tubers','irrigated oil crops'),
                 'NGS'=c('intensive grazing system','extensive grazing system'),
                 'NGST'=c('intensive grazing system','extensive grazing system','Total'),
                 'NTC'=c('pulpw. & particles','sawlogs, veneer, other'),
                 'NTCT'=c('pulpw. & particles','sawlogs, veneer, other','total'),
                 'NUFP'=c('food','feed','other use','stock change'),
                 'NUFPT'=c('food','feed','other use','stock change','Total'),
                 'NWC'=c('pulpw. & particles','sawlogs, veneer, other','fuelw. & charcoal'),
                 'NWCT'=c('pulpw. & particles','sawlogs, veneer, other','fuelw. & charcoal','All'),
                 'NYIELD'=c('decreasing yield','stable yield','increasing yield'),
                 'NYIELDT'=c('decreasing yield','stable yield','increasing yield','total'),
                 'NDEF'=c('just agriculture','wood and agriculture'),
                 'NDEFT'=c('just agriculture','wood and agriculture','total'),
                 'NFAC'=c('0 - 20','20 - 40','40 - 60','60 - 80','80 - 100','100 - 120','120 - 240','> 140'),
                 'NFHT'=c('clear-cut','selective cut','wood plantation','additional deforestation'),
                 'NFHTEXT'=c('clear-cut','selective cut','wood plantation','clear cut for wood plantation','clear cut forced'),
                 'NFHTEXTT'=c('clear-cut','selective cut','wood plantation','clear cut for wood plantation','clear cut forced','total'),
                 'NFHTT'=c('clear-cut','selective cut','wood plantation','additional deforestation','total'),
                 'NFMS'=c('regrowth forest','natural forest'),
                 'NFMST'=c('regrowth forest','natural forest','total'),
                 'NFST'=c('mature forest (t > rot. cyclus)','regrowth forest (t<=rot. cyclus)'),
                 'NFSTT'=c('mature forest (t > rot. cyclus)','regrowth forest (t<=rot. cyclus)','total'),
                 'NFT'=c('primary','clear-cut','selective cut','wood plantation','additional deforestation'),
                 'NFTT'=c('primary','clear-cut','selective cut','wood plantation','additional deforestation','total'),
                 'NLBP'=c('stems','branches','leaves','roots'),
                 'NLBPT'=c('stems','branches','leaves','roots','total'),
                 'NLDBP'=c('living biomass','death biomass'),
                 'NLDBPT'=c('living biomass','death biomass','just agriculture'),
                 'NRARID'=c('hyperarid','arid','semi-arid','dry Sub-humid','moist Sub-humid','humid'),
                 'NWPT'=c('abandoned land','clear-cut','forced clear cut'),
                 'NWPTT'=c('abandoned land','clear-cut','forced clear cut','total'),
                 'NWTF'=c('timberwood','fuelwood'),
                 'NWTFT'=c('timberwood','fuelwood','total'),
                 'NBP'=c('stems','branches','leaves','roots','litter','humus','charcoal','wood (short lifetime)','wood (long lifetime)'),
                 'NBPA'=c('living biomass','death biomass','biomass in timber'),
                 'NBIOF'=c('net ecosystem production','carbon fixation (NEP) by regrowing vegetation','deforestation','emissions from burning of traditional biomass','flux from timber pool (short lifetime)','flux from timber pool (long lifetime)','net flux'),
                 'NBIOFA'=c('full grown vegetation','regrowing vegetation','deforestation','net flux'),
                 'NCF'=c('energy','industry','deforestation+burning trad.biofuels+timber decay','emission carbon plantations','emission biofuel crops','regrowing vegetation','nat. Veg. (NEP excl.regrowth, biofuels, CPs)','uptake carbon plantations','uptake biofuel crops','oceans','net flux'),
                 'NCF10'=c('energy','industry','deforestation','emissions crops','emissions from burning of traditional biomass','regrowing vegetation','natural vegetaton','uptake crops','oceans','net flux'),
                 'NCF11'=c('energy','industry','deforestation','emissions crops','emissions from burning of traditional biomass','regrowing vegetation','natural vegetaton','uptake crops','uptake biofuels','oceans','net flux'),
                 'NCF7'=c('energy','industry','deforestation','regrowing vegetation','full grown vegetation','oceans','net flux'),
                 'NSRESP'=c('litter','humus','charcoal'),
                 'NSRESPT'=c('litter','humus','charcoal','total'),
                 'NRF'=c('CO2','CH4','N2O','Chloride','HFC','PFC & SF6','Halon','Trop. Ozon','Strat. Ozon','Strat. water vapour','Sulphate direct forcing','Sulphate indirect forcing','Organic Carbon','Black Carbon','Biomass burning','NOx','total'),
                 'NRF2'=c('positive contributor','negative contributor'),
                 'NRFNEG'=c('Strat. Ozon','Sulphate direct forcing','Black Carbon','Biomass burning'),
                 'NRFPOS'=c('CO2','CH4','N2O','Chloride','HFC','PFC & SF6','Halon','Trop. Ozon','Strat. water vapour','Black Carbon'),
                 'NRFT'=c('CO2','CH4','N2O','Chloride','HFC','PFC & SF6','Halon','Trop. Ozon','Strat. Ozon','Strat. water vapour','Sulphate direct forcing','Sulphate indirect forcing','Organic Carbon','Black Carbon','Biomass burning','NOx'),
                 'NSEAL'=c('thermal expansion of ocean','small glaciers','Greenland ice sheet','Antarctic ice sheet'),
                 'NSEALT'=c('thermal expansion of ocean','small glaciers','Greenland ice sheet','Antarctic ice sheet','total'),
                 'NTEMP'=c('lowest free atmospheric layer','effective surface temperature','surface Atlantic ocean','surface Pacific ocean','land surface','atmospheric surface layer over land'),
                 'NTEMPT'=c('lowest free atmospheric layer','effective surface temperature','surface Atlantic ocean','surface Pacific ocean','land surface','atmospheric surface layer over land','total'),
                 'NICH4'=c('iron & steel','chemicals'),
                 'NICH4_2011T'=c('iron & steel','chemicals','bulk chemicals','total'),
                 'NICH4T'=c('iron & steel','chemicals','total'),
                 'NICO'=c('iron & steel','aluminium'),
                 'NICO_2011T'=c('iron & steel','paper','bulk chemicals','total'),
                 'NICO2'=c('cement','feed stocks'),
                 'NICO2T'=c('cement','feed stocks','total'),
                 'NICOT'=c('iron & steel','aluminium','total'),
                 'NIN2O'=c('adipic acid','nitric acid'),
                 'NIN2O_2011T'=c('adipic acid','nitric acid','chemicals','total'),
                 'NIN2OT'=c('adipic acid','nitric acid','total'),
                 'NINOX'=c('cement','nitric acid','iron & steel'),
                 'NINOX_2011T'=c('iron & steel','chemicals','cement','paper','adipic acid','total'),
                 'NINOXT'=c('cement','nitric acid','iron & steel','total'),
                 'NISOX'=c('cement','copper','chemicals','lead & zinc','iron & steel'),
                 'NISOX_2011T'=c('iron & steel','copper','cement','chemicals','lead & zinc','paper','total'),
                 'NISOXT'=c('cement','copper','chemicals','lead & zinc','iron & steel','total'),
                 'NIVOC'=c('iron & steel','chemicals','solvents','miscelaneous'),
                 'NIVOC_2011T'=c('iron & steel','bulk chemicals','paper','chemicals','total'),
                 'NIVOCT'=c('iron & steel','chemicals','solvents','miscelaneous','total'),
                 'NLBCOC'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning'),
                 'NLBCOCT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','total'),
                 'NLCH4'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','landfills','sewage','wetland rice','animals','animal waste'),
                 'NLCH4T'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','landfills','sewage','wetland rice','animals','animal waste','Total'),
                 'NLCO'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning'),
                 'NLCO2'=c('biomass burning','burning of traditional biomass','timber pool (short lifetime)','timber pool (long lifetime)','carbon release by regrowing vegetation (-NEP)'),
                 'NLCO2T'=c('biomass burning','burning of traditional biomass','timber pool (short lifetime)','timber pool (long lifetime)','carbon release by regrowing vegetation (-NEP)','Total'),
                 'NLCOT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','Total'),
                 'NLN2O'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','land clearing','fertilizer','animal waste stables','animal waste grazing','manure application','indirect fertilizer','domestic sewage','crop residues','biological N-fixation'),
                 'NLN2OT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','land clearing','fertilizer','animal waste stables','animal waste grazing','manure application','indirect fertilizer','domestic sewage','crop residues','biological N-fixation','Total'),
                 'NLNH3'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','crops/decomposition of crops','animal waste stables','animal waste grazing','manure application','synthetic fertilizers'),
                 'NLNH3T'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','crops/decomposition of crops','animal waste stables','animal waste grazing','manure application','synthetic fertilizers','total'),
                 'NLNOX'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','fertilizer','animal waste stables','animal waste grazing','manure application','crop residues'),
                 'NLNOXT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','fertilizer','animal waste stables','animal waste grazing','manure application','crop residues','Total'),
                 'NLSOX'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning'),
                 'NLSOXT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','Total'),
                 'NLVOC'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning'),
                 'NLVOCT'=c('biomass burning','fuelwood burning','agricultural waste burning','savanna burning','Total'),
                 'NNCH4'=c('wetlands','methane hydrates','termites','wild animals','wild fires','volcanoes','oceans'),
                 'NNCH4T'=c('wetlands','methane hydrates','termites','wild animals','wild fires','volcanoes','oceans','total'),
                 'NNCO'=c('plants','wildfires','oceans'),
                 'NNCO2'=c(''),
                 'NNCO2T'=c(''),
                 'NNCOT'=c('plants','wildfires','oceans','total'),
                 'NNN2O'=c('natural soils','oceans'),
                 'NNN2OT'=c('natural soils','oceans','total'),
                 'NNNH3'=c('natural soils','oceans'),
                 'NNNH3T'=c('natural soils','oceans','total'),
                 'NNNOX'=c('lightning','natural soils'),
                 'NNNOXT'=c('lightning','natural soils','total'),
                 'NNSOX'=c('natural sources'),
                 'NNSOXT'=c('natural sources','total'),
                 'NNVOC'=c('natural sources'),
                 'NNVOCT'=c('natural sources','total'),
                 'FOSSIL'=c('coal','oil','natural gas'),
                 'FOSSILT'=c('coal','oil','natural gas','all'),
                 'NCOST'=c('Without all taxes','Without carbon tax','All costs included'),
                 'NDET'=c('activity level','Inter sectoral change','intra sectoral change','conservation','fuel switch','energy intensity','energy demand'),
                 'NEC4'=c('coal','heavy oil','light oil','natural gas'),
                 'NEC4T'=c('coal','heavy oil','light oil','natural gas','all'),
                 'NEC5'=c('coal','heavy oil','light oil','natural gas','modern biofuels'),
                 'NEC5T'=c('coal','heavy oil','light oil','natural gas','modern biofuels','all'),
                 'NEC8_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','non-thermal electricity','hydropower'),
                 'NEC8T_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','non-thermal electricity','hydropower','all'),
                 'NEC9'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','nuclear','solar / wind','hydropower'),
                 'NEC9T'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','nuclear','solar / wind','hydropower','all'),
                 'NECCAP'=c('fossil based electricity','fossil based electricity (CCS)','biomass based electricity','biomass based electricity (CCS)','nuclear','solar / wind','hydropower'),
                 'NECCAP_22'=c('thermal electricity','nuclear','solar / wind','hydropower'),
                 'NECCAPT'=c('fossil based electricity','fossil based electricity (CCS)','biomass based electricity','biomass based electricity (CCS)','nuclear','solar / wind','hydropower','all'),
                 'NECCAPT_22'=c('thermal electricity','nuclear','solar / wind','hydropower','all'),
                 'NECP'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','nuclear','solar / wind','hydropower'),
                 'NECP_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','non-thermal electricity','hydropower'),
                 'NECP3'=c('modern biofuels','hydropower','solar / wind'),
                 'NECP3T'=c('modern biofuels','hydropower','solar / wind','all'),
                 'NECP4'=c('modern biofuels','traditional biofuels','non-thermal electricity','hydropower'),
                 'NECP4T'=c('modern biofuels','traditional biofuels','non-thermal electricity','hydropower','all'),
                 'NECPT'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','nuclear','solar / wind','hydropower','all'),
                 'NECPT_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','non-thermal electricity','hydropower','all'),
                 'NECS'=c('coal','oil','natural gas','hydrogen','modern biofuels','heat','traditional biofuels','electricity'),
                 'NECS_22'=c('coal','oil','natural gas','traditional biofuels','electricity'),
                 'NECS8_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','electricity','secondairy heat'),
                 'NECS8T_22'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','electricity','secondairy heat','all'),
                 'NECS9'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','hydrogen','secondairy heat','electricity'),
                 'NECS9T'=c('coal','heavy oil','light oil','natural gas','modern biofuels','traditional biofuels','hydrogen','secondairy heat','electricity','all'),
                 'NECST_22'=c('coal','oil','natural gas','traditional biofuels','electricity','all'),
                 'NECTR'=c('coal','oil','natural gas','modern biofuels'),
                 'NEF'=c('heat','electricity'),
                 'NEFT'=c('heat','electricity','all'),
                 'NI'=c('fossil','modern biofuels','electricity','savings'),
                 'NIT'=c('fossil','modern biofuels','electricity','savings','all'),
                 'RAINS_NECT'=c('coal','heavy oil','light oil','light oil 2','natural gas','traditional biofuels','electricity','heat','hydropower','nuclear','solar / wind','all'),
                 'NCHLOR'=c('CFC-11','CFC-12','CFC-113','CFC-114','CFC-115','CCl4','MCF (CH3CC13)','CH3CL','HCFC-22','HCFC-123','HCFC-124','HCFC-141b','HCFC-142b','HCFC-225'),
                 'NEQ'=c('CO2','CH4','N2O','HFC','PFCs & SF6'),
                 'NEQT'=c('CO2','CH4','N2O','HFC','PFCs & SF6','Total'),
                 'NHALON'=c('Halon-1211','Halon-1301','CH3Br'),
                 'NHFC'=c('HFC-23','HFC-32','HFC-43-10','HFC-125','HFC-134a','HFC-143a','HFC-152a','HFC-227ea','HFC-236fa','HFC-245ca'),
                 'NPFC'=c('CF4','C2F6','SF6'),
                 'NARV'=c('protected bioreserves','valuable bioreserves'),
                 'NARVO'=c('protected bioreserves','valuable bioreserves','other'),
                 'NARVOT'=c('protected bioreserves','valuable bioreserves','other','total'),
                 'NARVT'=c('protected bioreserves','valuable bioreserves','total'),
                 'NFA'=c('mature forest','regrowth forest (Abandoning)','regrowth forest (Timber)','carbon plantations'),
                 'NFAT'=c('mature forest','regrowth forest (Abandoning)','regrowth forest (Timber)','carbon plantations','total'),
                 'NFCAREA'=c('rainfed','irrigated'),
                 'NFCAREAT'=c('rainfed','irrigated','total'),
                 'NLCAT'=c('agricultural land','forest','other','total'),
                 'NLCT'=c('agricultural land','extensive grassland','carbon plantations','regrowth forest (Abandoning)','regrowth forest (Timber)','biofuel','ice','tundra','wooded tundra','boreal forest','cool conifer','temp. mixed forest','temp.deciduous forest','warm mixed','grassland/steppe','hot desert','scrubland','savanna','tropical woodland','tropical forest'),
                 'NLCT_22'=c('agricultural land','extensive grassland','carbon plantations','regrowth forest (Abandoning)','regrowth forest (Timber)','ice','tundra','wooded tundra','boreal forest','cool conifer','temp. mixed forest','temp.deciduous forest','warm mixed','grassland/steppe','hot desert','scrubland','savanna','tropical woodland','tropical forest'),
                 'NLCTT'=c('agricultural land','extensive grassland','carbon plantations','regrowth forest (Abandoning)','regrowth forest (Timber)','biofuel','ice','tundra','wooded tundra','boreal forest','cool conifer','temp. mixed forest','temp.deciduous forest','warm mixed','grassland/steppe','hot desert','scrubland','savanna','tropical woodland','tropical forest','total'),
                 'NLCTT_22'=c('agricultural land','extensive grassland','carbon plantations','regrowth forest (Abandoning)','regrowth forest (Timber)','ice','tundra','wooded tundra','boreal forest','cool conifer','temp. mixed forest','temp.deciduous forest','warm mixed','grassland/steppe','hot desert','scrubland','savanna','tropical woodland','tropical forest','total'),
                 'NLNDCOV'=c('total','cropland','pasture','forest','other land','managed forest','energy crops','other arable land','forest area harvested for wood','forest area never managed','afforestation deforestation','builtup','irrigated cropland','irrigated energy crops','other natural land'),
                 'NVEGC'=c('change in potential vegetation without adaption','change in potential vegetation with adaption','no change in potential vegetation'),
                 'NVEGCT'=c('change in potential vegetation without adaption','change in potential vegetation with adaption','no change in potential vegetation','total'),
                 'NAEM'=c('energy','industry','land use'),
                 'NAEMT'=c('energy','industry','land use','Total'),
                 'NEM'=c('energy','industry','land use','natural'),
                 'NEMT'=c('energy','industry','land use','natural','Total'),
                 'NF'=c('maintenance','feed obtainance','lactation','pregnancy'),
                 'NLFTCH4'=c('chemical lifetime','atmospheric lifetime'),
                 'NM'=c('January','February','March','April','May','June','July','August','September','October','November','December'),
                 'NMT'=c('January','February','March','April','May','June','July','August','September','October','November','December','mean'),
                 'NVUICL'=c('< 0.05','0.05 <= Index< 0.10','0.10 <= Index< 0.15','0.15 <= Index< 0.20','0.20 <= Index< 0.25','0.25 <= Index< 0.30','0.30 <= Index< 0.35','0.35 <= Index< 0.40','0.40 <= Index< 0.45','0.45 <= Index< 0.50','0.50 <= Index< 0.55','0.55 <= Index< 0.60','0.60 <= Index< 0.65','0.65 <= Index< 0.70','0.70 <= Index< 0.75','0.75 <= Index< 0.80','0.80 <= Index< 0.85','0.85 <= Index< 0.90','0.90 <= Index< 0.95','0.95 <= Index<= 1.00'),
                 'NAC2T'=c('food crops','grass & fodder','Total'),
                 'NAEMOECD'=c('energy','land use','industry'),
                 'NAPDT'=c('ACI = Acute Respiratory Infections,children <5 yr','LC = Lung Cancer','CPD = Cardio-Pulmonary Disease.','Total'),
                 'NAPYR'=c('2000','2030'),
                 'NBDL'=c('Nitrogen deposition','Infrastructure','Fragmentation','Forestry','Climate change','Pasture area','Woody biofuels','Crop Area','MSA Remaining'),
                 'NBDL6'=c('Climate change','Fragmentation','Infrastructure','Forestry','Agriculture','MSA Remaining'),
                 'NBDLT'=c('Total','Nitrogen deposition','Infrastructure','Fragmentation','Forestry','Climate change','Pasture area','Woody biofuels','Crop Area','MSA Remaining'),
                 'NBIOMES'=c('Tropical grassland & savanah','Temperate grassland & steppe','Tropical rain forest','Tropical dry forest','Mediterranean forest, woodland & shrub','Temperate broadleaf & mixed forest','Temperate coniferous forest','Boreal forest','Desert','Tundra','Polar','No Biome distinction'),
                 'NDIFFYR'=c('2030','2050'),
                 'NDRT'=c('Grey','Green','Total'),
                 'NECP3OECD'=c('Coal','Oil','Nat. gas'),
                 'NECP9'=c('Coal','Heavy oil','Light Oil','Nat. gas','Mod. Biofuels','Trad. Biofuels','Nuclear','Solar/Wind','Hydro'),
                 'NECP9T'=c('Coal','Heavy oil','Light Oil','Nat. gas','Mod. Biofuels','Trad. Biofuels','Nuclear','Solar/Wind','Hydro','Total'),
                 'NECS9OECD'=c('Coal','Heavy oil','Light Oil','Nat. gas','Mod. Biofuels','Trad. Biofuels','Hydrogen','Sec. Heat','Electricity'),
                 'NECS9OECDT'=c('Coal','Heavy oil','Light Oil','Nat. gas','Mod. Biofuels','Trad. Biofuels','Hydrogen','Sec. Heat','Electricity','Total'),
                 'NEQ3'=c('CO2','CH4','N2O'),
                 'NEQ3REV'=c('N2O','CH4','CO2'),
                 'NEQREV'=c('PFCs & SF6','HFC','N2O','CH4','CO2'),
                 'NNEXPT'=c('nonpoint_N_export','nonpoint_N_export_agriculture','nonpoint_N_export_nature','sewage_N_export','Total'),
                 'NNIN'=c('N-fertilizer','N-manure','N-deposition','N-fixation'),
                 'NNOUT'=c('Crop export','NH3 violatilization','Other Losses'),
                 'NOR'=c('Proved recoverable reserves','Estimated additional reserves','Additional resources','Unconventional reserves','Unconventional resources (excl. add. occurences)'),
                 'NR25T'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Greenland','World'),
                 'NROECD'=c('US + Can','Western Europe','Japan','Oceania','Central Europe','Former Soviet Union','East Asia','South East Asia','South Asia','Middle East','Africa','Latin America'),
                 'NROECD13'=c('NAM','EUR','JPK','ANZ','BRA','RUS','SOA','CHN','MEA','OAS','ECA','OLC','AFR'),
                 'NROECD13T'=c('NAM','EUR','JPK','ANZ','BRA','RUS','SOA','CHN','MEA','OAS','ECA','OLC','AFR','World'),
                 'NROECD3'=c('OECD','BRIC','Rest of world'),
                 'NROECD3T'=c('OECD','BRIC','Rest of world','World'),
                 'NROECD6'=c('OECD','BRA','RUS','SOA','CHN','Rest of world'),
                 'NROECDAGGR4'=c('OECD','Transition Economies','Developing Countries','NON-OECD'),
                 'NROECDAGGRT'=c('OECD','Transition Economies','Developing Countries','World'),
                 'NROECDAM'=c('NAM','BRA','OLC'),
                 'NROECDT'=c('US + Can','Western Europe','Japan','Oceania','Central Europe','Former Soviet Union','East Asia','South East Asia','South Asia','Middle East','Africa','Latin America','World'),
                 'NRQFA'=c('Forest area equal to 1970','Natural forest area'),
                 'NS10'=c('Industry','Transport','Residential','Services','Agriculture','Bunker Oil','Non-energy','Hydrogen','Electricity','Other'),
                 'NS10REV'=c('Other','Electricity','Hydrogen','Non-energy','Bunker Oil','Agriculture','Services','Residential','Transport','Industry'),
                 'NS5'=c('Industry','Transport','Residential','Services','Other'),
                 'NS5T'=c('Industry','Transport','Residential','Services','Other','Total'),
                 'NSC_1'=c('Baseline','Case 3','Case 4','Case 5'),
                 'NSC_2'=c('Baseline','Case 3','Case 4','Case 5','Case 6'),
                 'NSC_3'=c('Case 3','Case 5'),
                 'NSC_4'=c('Baseline','Case 7','Case 8','Case 9'),
                 'NSC_5'=c('Baseline','Case 9','Case 10','Case 11'),
                 'NSC_6'=c('Baseline','Case 9','Case 12'),
                 'NSC_7'=c('Baseline','Case 5'),
                 'NSC2'=c('OECD','IEA'),
                 'NSC4'=c('OECD','OS','GO','IEA'),
                 'NWSS'=c('Severe','Medium','Low','No'),
                 'NYR'=c('Potential','1700','1750','1800','1850','1900','1950','2000','2050'),
                 'NAGE'=c('0-5 year','5-10 year','10-15 year','15-20 year','20-25 year','25-30 year','30-35 year','35-40 year','40-45 year','45-50 year','50-55 year','55-60 year','60-65 year','65-70 year','70-75 year','75-80 year','80-85 year','85-90 year','90-95 year','95-100 year'),
                 'NAGE21'=c('0-1 year','1-5 year','5-10 year','10-15 year','15-20 year','20-25 year','25-30 year','30-35 year','35-40 year','40-45 year','45-50 year','50-55 year','55-60 year','60-65 year','65-70 year','70-75 year','75-80 year','80-85 year','85-90 year','90-95 year','95-100 year'),
                 'NDIS'=c('TBC','HIV/AIDS','Diarrhoeal','Measles (mea)','Malaria (mal)','Respiratory infections','Maternal','Perinatal','Nutritional deficiencies','Other communicable','Malignant neoplasm','Trachea, bronchus, lung cancers','Diabetes mellitus','Neuropsychiatric','Cardio Vascular diseases (CVD)','Respiratory','Digestive','Other non-communicable','Injuries'),
                 'NEDUC'=c('No','Primary','Secundairy','Tertairy'),
                 'NEXP'=c('High income','hinc & blood pressure (bp)','hinc & nicotine (nic)','hinc & blood pressure & nicotine','Low income','linc & malnutrition(mnut)','linc & malaria (mal)','linc & malwater (mwat)','linc & mnut & mal','linc & mnut & mwat','linc & mal & mwat','linc & mnut & mal & mwat'),
                 'NFER'=c('15-20 year','20-25 year','25-30 year','30-35 year','35-40 year','40-45 year','45-50 year'),
                 'NP'=c('rural','urban'),
                 'NPAR'=c('1 child','2 children','3 children','4 or more children'),
                 'NSEX'=c('male','female'),
                 'NSEXT'=c('male','female','total'),
                 'NUR'=c('Urban','Rural'),
                 'NURT'=c('Urban','Rural','total'),
                 'NR'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa'),
                 'NR13'=c('Canada','USA','Latin America','Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','India + S. Asia','China + C.P. Asia','East Asia','Oceania','Japan'),
                 'NR13_4_2'=c('Canada','USA','Latin America','Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','India + S. Asia','China + C.P. Asia','East Asia','Oceania','Japan','OECD90','REF','ASIA','ALM','Annex-1','non-Annex1'),
                 'NR13_4_2T'=c('Canada','USA','Latin America','Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','India + S. Asia','China + C.P. Asia','East Asia','Oceania','Japan','OECD90','REF','ASIA','ALM','Annex-1','non-Annex1','World'),
                 'NR13T'=c('Canada','USA','Latin America','Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','India + S. Asia','China + C.P. Asia','East Asia','Oceania','Japan','World'),
                 'NR17'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan'),
                 'NR17_4_2'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','OECD90','REF','ASIA','ALM','Annex-1','non-Annex1'),
                 'NR17_4_2T'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','OECD90','REF','ASIA','ALM','Annex-1','non-Annex1','World'),
                 'NR17T'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','World'),
                 'NR18'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','Greenland'),
                 'NR18O'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','Greenland','Other'),
                 'NR18OT'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','Greenland','Other','World'),
                 'NR18T'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','Greenland','World'),
                 'NR24'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania'),
                 'NR24T'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','World'),
                 'NR27'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','dummy'),
                 'NR27G'=c('Canada','USA','Mexico','Central America','South America','Brazil','Northern Africa','Western Africa','Central Africa','Eastern Africa','Southern Africa','Republic of South Africa','OECD Europe','Central Europe','Eastern Europe','Turkey','Russia','Stans','Middle East','South Asia','India','China','Koreas','South East Asia','Indonesia','Japan','Oceania'),
                 'NR27T'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','dummy','World'),
                 'NR45'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit'),
                 'NR45G'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit','Greenland'),
                 'NR45GO'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit','Greenland','Other'),
                 'NR45GOT'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit','Greenland','Other','World'),
                 'NR45GT'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit','Greenland','World'),
                 'NR45T'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India +','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Austria','Belg.+Lux.','Denmark','Finland','France','Germany','Greece','Ireland','Italy','Netherlands','Portugal','Spain','Sweden','United Kingdom','Bulg.+Roem.','Czech Republic','Hungary','Poland','Slovakia','Slovenia','Est+Lat+Lit','World'),
                 'NRANNEX'=c('Annex-1','non-Annex1'),
                 'NRC'=c('Afghanistan','Albania','Algeria','American Samoa','Andorra','Angola','Azerbaijan','Argentina','Australia','Austria','Bahamas','Bahrain','Bangladesh','Armenia','Barbados','Belgium','Bermuda','Bhutan','Bolivia','Bosnia and Herzegovina','Botswana','Brazil','Belize','Solomon Islands','Virgin Islands, British','Brunei Darussalam','Bulgaria','Myanmar','Burundi','Belarus','Cambodia','Cameroon','Canada','Cape Verde','Cayman Islands','Central African Republic','Sri Lanka','Chad','Chile','China','Taiwan, Province of China','Colombia','Comoros','Congo','Congo, the Democratic Republic of the','Cook Islands','Costa Rica','Croatia','Cuba','Cyprus','Czech Republic','Benin','Denmark','Dominica','Dominican Republic','Ecuador','El Salvador','Equatorial Guinea','Ethiopia','Eritrea','Estonia','Faroe Islands','Falklands Islands (Malvinas)','Fiji','Finland','France','French Guiana','French Polynesia','Djibouti','Gabon','Georgia','Gambia','Germany','Ghana','Gibraltar','Kiribati','Greece','Greenland','Grenada','Guadeloupe','Guatemala','Guinea','Guyana','Haiti','Holy See (Vatican City State)','Honduras','Hong Kong','Hungary','Iceland','India','Indonesia','Iran, Islamic Republic of','Iraq','Ireland','Israel','Italy','C�te d~Ivoire','Jamaica','Japan','Kazakstan','Jordan','Kenya','Korea, Democratic People~s Republic of','Korea, Republic of','Kuwait','Kyrgyzstan','Lao People~s Democratic Republic','Lebanon','Lesotho','Latvia','Liberia','Libyan Arab Jamahiriya','Liechtenstein','Lithuania','Luxembourg','Macau','Madagascar','Malawi','Malaysia','Maldives','Mali','Malta','Martinique','Mauritania','Mauritius','Mexico','Monaco','Mongolia','Moldova, Republic of','Montserrat','Morocco','Mozambique','Oman','Namibia','Nauru','Nepal','Netherlands','Netherlands Antilles','Aruba','New Caledonia','Vanuatu','New Zealand','Nicaragua','Niger','Nigeria','Niue','Norway','Northern Mariana Islands','Micronesia, Federated States of','Marshall Islands','Palau','Pakistan','Panama','Papua New Guinea','Paraguay','Peru','Philippines','Pitcairn','Poland','Portugal','Guinea-Bissau','East Timor','Puerto Rico','Qatar','R�union','Romania','Russian Federation','Rwanda','Saint Helena','Saint Kitts and Nevis ','Anguilla','Saint Lucia','Saint Pierre and Miquelon','Saint Vincent and the Grenadines','San Marino','Sao Tome and Principe','Saudi Arabia','Senegal','Seychelles','Sierra Leone','Singapore','Slovakia','Viet Nam','Slovenia','Somalia','South Africa','Zimbabwe','Spain','Western Sahara','Sudan','Suriname','Swaziland','Sweden','Switzerland','Syrian Arab Republic','Tajikistan','Thailand','Togo','Tokelau','Tonga','Trinidad and Tobago','United Arab Emirates','Tunisia','Turkey','Turkmenistan','Turks and Caicos Islands','Tuvalu','Uganda','Ukraine','Macedonia, the former Yugoslav Republic of','Egypt','United Kingdom','Tanzania, United Republic of','United States','Virgin Islands, U.S.','Burkina Faso','Uruguay','Uzbekistan','Venezuela','Wallis and Futuna','Samoa','Yemen','Yugoslavia','Zambia'),
                 'NRG'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','Greenland'),
                 'NRGO'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','Greenland','Other'),
                 'NRGOT'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','Greenland','Other','World'),
                 'NRGT'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','Greenland','World'),
                 'NRIPCC'=c('OECD90','REF','ASIA','ALM'),
                 'NRIPCCT'=c('OECD90','REF','ASIA','ALM','World'),
                 'NROECD29'=c('Brazil','Oceania','China +','Japan','Korea','Southeastern Asia','Indonesia +','Rest of S. Asia','India','Rest South America','Canada','USA','Mexico','Rest Central America','Ukraine +','Russia +','Asia-Stan','Middle East','Turkey','Northern Africa','Western Africa','Rest of S. Africa','Eastern Africa','South Africa','EU15','EU countries non OECD','Hung.,Pol.,Cz.Rep., Slov','EFTA','Rest of Central Europe'),
                 'NRT'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','World'),
                 'REGIONS'=c('Canada','USA','Central America','South America','Northern Africa','Western Africa','Eastern Africa','Southern Africa','OECD Europe','Eastern Europe','Former USSR','Middle East','South Asia','East asia','South East Asia','Oceania','Japan','World'),
                 'REGIONS27T'=c('Canada','USA','Mexico','Rest Central America','Brazil','Rest South America','Northern Africa','Western Africa','Eastern Africa','South Africa','OECD Europe','Eastern Europe','Turkey','Ukraine +','Asia-Stan','Russia +','Middle East','India','Korea','China +','South East Asia','Indonesia +','Japan','Oceania','Rest of S. Asia','Rest of S. Africa','dummy','World'),
                 'NES'=c('industry','transport','residential','services','other','energy transformation','power generation','losses/leakages','bunkers'),
                 'NESI'=c('industry','transport','residential','services','other','energy transformation','power generation','losses/leakages','bunkers','industry-process'),
                 'NESIT'=c('industry','transport','residential','services','other','energy transformation','power generation','losses/leakages','bunkers','industry-process','total'),
                 'NESREV'=c('bunkers','losses/leakages','power generation','energy transformation','other','services','residential','transport','industry'),
                 'NEST'=c('industry','transport','residential','services','other','energy transformation','power generation','losses/leakages','bunkers','total'),
                 'NS'=c('industry','transport','residential','services','other'),
                 'NSB'=c('transport','other'),
                 'NSBT'=c('transport','other','total'),
                 'NSE'=c('agriculture','industry','services'),
                 'NSET'=c('layer 40','industry','services','total'),
                 'NSG'=c('education','health','research & development','manufactors'),
                 'NSGT'=c('education','health','research & development','manufactors','total'),
                 'NSP'=c('agriculture','materials','energy','manufactors','services'),
                 'NSPT'=c('agriculture','materials','energy','manufactors','services','total'),
                 'NST'=c('industry','transport','residential','services','other','total'),
                 'NTRIP'=c('domestic','industry','power'),
                 'NTRIPT'=c('domestic','industry','power','total'),
                 'RAINS_NS'=c('industry','transport','domestic','electricity','energy system','non-energy use','bunkers'),
                 'NALL'=c(''),
                 'NC'=c(''),
                 'NCW'=c(''),
                 'NHEMIS'=c('northern','southern'),
                 'NHEMIST'=c('northern','southern','global'),
                 'NLATBELT'=c('latitude1','latitude2','latitude3','latitude4','latitude5','latitude6','latitude7','latitude8','latitude9','latitude10','latitude11','latitude12','latitude13','latitude14','latitude15','latitude16','latitude17','latitude18'),
                 'NLATI'=c('90oN - 60oN','60oN - 30oN','30oN -  0o','0o  - 30oS','30oS - 60oS','60oS - 90oS'),
                 'NLATIT'=c('90oN - 60oN','60oN - 30oN','30oN -  0o','0o  - 30oS','30oS - 60oS','60oS - 90oS','global'),
                 'NOCLAY'=c('layer 1','layer 2','layer 3','layer 4','layer 5','layer 6','layer 7','layer 8','layer 9','layer 10','layer 11','layer 12','layer 13','layer 14','layer 15','layer 16','layer 17','layer 18','layer 19','layer 20','layer 21','layer 22','layer 23','layer 24','layer 25','layer 26','layer 27','layer 28','layer 29','layer 30','layer 31','layer 32','layer 33','layer 34','layer 35','layer 36','layer 37','layer 38','layer 39','layer 40'),
                 'NWS'=c('domestic','industrial','agricultural'),
                 NULL )
  
    if (is.null(dimlabels) | length(dimlabels)!=dim) {
      dimlabels = paste(dimname, 1:dim,sep="_")
    }
    return(dimlabels)
}


lookup.mym.dimnameslong = function(dimname){
  # retrieve default dimlabels, based on varname.
  
  longname = switch(dimname, 
                 'IPCC_REGIONS'='IPCC Regions incl. Total',
                 'NA'='Animal types',
                 'NAC'='Agricultural crop classes',
                 'NACT'='Agricultural crop classes + Total',
                 'NAP'='Animal products',
                 'NAPO'='Animal products + Other',
                 'NAPT'='Animal products incl. Total',
                 'NAS'='Animal types with slaughter rates',
                 'NAT'='Animal types + total',
                 'NBC'='Biofuel Crops',
                 'NBCO'='Biofuel crops + Other',
                 'NBCOT'='Biofuel crops + Other incl. Total',
                 'NBCT'='Biofuel Crops',
                 'NBFC'='Basic food crops',
                 'NFBC'='Food & fodder & biofuel crops',
                 'NFC'='Aggregated Foodcrops',
                 'NFCO'='Aggregated food crops + Other incl. Total',
                 'NFCT'='Aggregated Foodcrops incl Total',
                 'NFERT'='Fertilizer types (v2.4)',
                 'NFERT_22'='Fertilizer types',
                 'NFERTT'='Fertilizer types (v2.4) incl total',
                 'NFERTT_22'='Fertilizer types incl total',
                 'NFFBC'='Food & fodder & biofuel crops',
                 'NFFC'='Food & fodder crops',
                 'NFOC'='Food categories',
                 'NFOCT'='Food categories incl. Total',
                 'NFP'='Products used for feeds  (v2.4)',
                 'NFP_22'='Products used for feeds',
                 'NFPT'='Products used for feeds + total (v2.4)',
                 'NFPT_22'='Products used for feeds + total',
                 'NFRAC'='foodcrops',
                 'NGS'='Grazing systems',
                 'NGST'='Grazing systems incl Total',
                 'NTC'='Timber products',
                 'NTCT'='number of timber product classes',
                 'NUFP'='Types of use of food products',
                 'NUFPT'='Types of use of food products + Total',
                 'NWC'='Wood products',
                 'NWCT'='Wood products including Total',
                 'NYIELD'='Yield changes',
                 'NYIELDT'='Yield changes + total',
                 'NDEF'='Conversion cause',
                 'NDEFT'='Conversion cause incl total',
                 'NFAC'='number of forest age classes',
                 'NFHT'='Forestry types, no primary',
                 'NFHTEXT'='Forestry types extended',
                 'NFHTEXTT'='Forestry types + total',
                 'NFHTT'='Forestrytypes + total',
                 'NFMS'='Forest stages',
                 'NFMST'='Forest stages + total',
                 'NFST'='Forest status',
                 'NFSTT'='Forest status + total',
                 'NFT'='Forestry types',
                 'NFTT'='ForestryTypes incl total',
                 'NLBP'='Living biomass pool',
                 'NLBPT'='Living biomass pool + total',
                 'NLDBP'='Biomass pools',
                 'NLDBPT'='Biomass pools incl total',
                 'NRARID'='Aridity zones',
                 'NWPT'='Starting types woodplantations',
                 'NWPTT'='Starting types woodplantations + total',
                 'NWTF'='Wood product classes',
                 'NWTFT'='Wood product classes + total',
                 'NBP'='Biomass pools',
                 'NBPA'='Aggregated biomass pools',
                 'NBIOF'='Carbon fluxes between terrestrial biosphere and atmosphere',
                 'NBIOFA'='Landcover type fluxes',
                 'NCF'='Carbon fluxes (from feb 2010)',
                 'NCF10'='Carbon fluxes( v between 2.2 and 2.4)',
                 'NCF11'='Carbon fluxes(v 2.4 tot feb 2010?)',
                 'NCF7'='Carbon Fluxes (v22)',
                 'NSRESP'='Respiration fluxes',
                 'NSRESPT'='Respiration fluxes + total',
                 'NRF'='contributors to radiative forcing',
                 'NRF2'='positve/negatve contributors to radiatve forcing',
                 'NRFNEG'='negative contributors to radiative forcing',
                 'NRFPOS'='positive contributors to radiative forcing',
                 'NRFT'='contributors to radiative forcing incl total',
                 'NSEAL'='Sources of sea level rise',
                 'NSEALT'='Sources of sea level rise + total',
                 'NTEMP'='Temperature types',
                 'NTEMPT'='Temperature types + total',
                 'NICH4'='CH4 emission from Industry sources 2010',
                 'NICH4_2011T'='CH4 emission from Industry sources',
                 'NICH4T'='CH4 emission from Industry sources 2010',
                 'NICO'='CO  emission from Industry sources 2010',
                 'NICO_2011T'='CO emission from Industry sources incl total',
                 'NICO2'='CO2 emission from Industry sources',
                 'NICO2T'='CO2 emission from Industry sources incl. total',
                 'NICOT'='CO  emission from Industry sources incl. total 2010',
                 'NIN2O'='N2O emission from Industry sources 20101',
                 'NIN2O_2011T'='N2O emission from Industry sources incl. total',
                 'NIN2OT'='N2O emission from Industry sources incl. total 2010',
                 'NINOX'='NOx  emission from Industry sources 2010',
                 'NINOX_2011T'='NOx  emission from Industry sources incl. total',
                 'NINOXT'='NOx  emission from Industry sources incl. total 2010',
                 'NISOX'='SOx  emission from Industry sources 2010',
                 'NISOX_2011T'='SOx  emission from Industry sources incl. total',
                 'NISOXT'='SOx  emission from Industry sources incl. total 2010',
                 'NIVOC'='VOC emission from Industry sources 2010',
                 'NIVOC_2011T'='VOC emission from Industry sources incl. total',
                 'NIVOCT'='VOC emission from Industry sources incl. total 2010',
                 'NLBCOC'='Emission sources of BC and OC from land use',
                 'NLBCOCT'='Emission sources of BC and OC from land use + total',
                 'NLCH4'='CH4 emission from  landuse',
                 'NLCH4T'='CH4 emission from landuse incl. Total',
                 'NLCO'='CO  emission from  landuse',
                 'NLCO2'='CO2 emission from  landuse',
                 'NLCO2T'='CO2 emission from  landuse incl. Total',
                 'NLCOT'='CO  emission from landuse incl. Total',
                 'NLN2O'='N2O emission from landuse',
                 'NLN2OT'='N2O emission from landuse incl. Total',
                 'NLNH3'='Emission sources of NH3 from land use',
                 'NLNH3T'='Emission sources of NH3 from land use + total',
                 'NLNOX'='NOx  emission from landuse',
                 'NLNOXT'='NOx  emission from  landuse incl. Total',
                 'NLSOX'='SOx  emission from  landuse',
                 'NLSOXT'='SOx  emission from  landuse incl. Total',
                 'NLVOC'='VOC emission from  landuse',
                 'NLVOCT'='VOC emission from landuse incl. Total',
                 'NNCH4'='CH4 emission from natural sources',
                 'NNCH4T'='CH4 emission from natural sources incl. Total',
                 'NNCO'='CO  emission from natural sources',
                 'NNCO2'='CO2 emission from natural sources',
                 'NNCO2T'='CO2 emission from natural sources incl. Total',
                 'NNCOT'='CO  emission from natural sources incl. Total',
                 'NNN2O'='N2O emission from natural sources',
                 'NNN2OT'='N2O emission from natural sources incl. Total',
                 'NNNH3'='Emission sources of NH3 from natural sources',
                 'NNNH3T'='Emission sources of NH3 from natural sources + total',
                 'NNNOX'='NOx  emission from natural sources',
                 'NNNOXT'='NOx  emission from natural sources incl. Total',
                 'NNSOX'='SOx  emission from natural sources',
                 'NNSOXT'='SOx  emission from natural sources incl. Total',
                 'NNVOC'='VOC emission from natural sources',
                 'NNVOCT'='VOC emission from natural sources incl. Total',
                 'FOSSIL'='Energy carriers fossil',
                 'FOSSILT'='Energy carriers fossil  incl. Total',
                 'NCOST'='costs for energy options',
                 'NDET'='Energy determinants',
                 'NEC4'='Energy carriers (oil in HLF , LLF)',
                 'NEC4T'='Energy carriers (oil in HLF , LLF) + all',
                 'NEC5'='Energy carriers related to emissions',
                 'NEC5T'='Energy carriers related to emissions + all',
                 'NEC8_22'='Total secondary energy carriers',
                 'NEC8T_22'='Total secondary energy carriers + all',
                 'NEC9'='Total secondary energy carriers (2.3, vh NEC8)',
                 'NEC9T'='Total secondary energy carriers (2.3, vh NEC8) + all',
                 'NECCAP'='Some energy carriers (2.3)',
                 'NECCAP_22'='Some energy carriers',
                 'NECCAPT'='Some energy carriers (2.3) + all',
                 'NECCAPT_22'='Some energy carriers incl total',
                 'NECP'='Primary energy carriers (2.3)',
                 'NECP_22'='Primary energy carriers',
                 'NECP3'='Some primary energy carriers',
                 'NECP3T'='Some primary energy carriers incl total',
                 'NECP4'='Some primary energy carriers',
                 'NECP4T'='Some primary energy carriers + all',
                 'NECPT'='Primary energy carriers (2.3) + all',
                 'NECPT_22'='Primary energy carriers + all',
                 'NECS'='Secondary energy carriers (limited set) (2.3)',
                 'NECS_22'='Secondary energy carriers (limited set)',
                 'NECS8_22'='Secondary energy carriers',
                 'NECS8T_22'='Secondary energy carriers incl total',
                 'NECS9'='Secondary energy carriers  (2.3, vh NECS8)',
                 'NECS9T'='Secondary energy carriers  (2.3, vh NECS8) incl total',
                 'NECST_22'='Secondary energy carriers  (limited set) incl. Total',
                 'NECTR'='Energy carriers Trade',
                 'NEF'='Energy function',
                 'NEFT'='Energy functions + total',
                 'NI'='Energy investments',
                 'NIT'='Energy investments incl total',
                 'RAINS_NECT'='Energy carriers tbv RAINS conversie incl all',
                 'NCHLOR'='Chlorinated compounds (I.e. CFCs)',
                 'NEQ'='Contributors to equivalent CO2 concentration',
                 'NEQT'='Contributors to equivalent CO2 concentration + Total',
                 'NHALON'='Halons',
                 'NHFC'='Hydrofluorocarbons',
                 'NPFC'='Perfluorocarbons',
                 'NARV'='Areas considered for changes in vegetation',
                 'NARVO'='Areas considered for changes in vegetation +other',
                 'NARVOT'='Areas considered for changes in vegetation + other + total',
                 'NARVT'='Cumulative areas for changes in vegetation',
                 'NFA'='Forest areas',
                 'NFAT'='Forest areas incl. Total',
                 'NFCAREA'='Food crop areas',
                 'NFCAREAT'='Food crop areas + Total',
                 'NLCAT'='Aggregated landcover type',
                 'NLCT'='Land covertypes (size=20, IMAGE2.3)',
                 'NLCT_22'='Landcovertype indeling met 19 klassen (IMAGE 2.2)',
                 'NLCTT'='Land covertypes incl. Total  (size=20+, IMAGE2.3)',
                 'NLCTT_22'='Landcovertype indeling met 19 klassen,plus total, (IMAGE 2.2)',
                 'NLNDCOV'='Landcover types',
                 'NVEGC'='Vegetation changes',
                 'NVEGCT'='Vegetation changes + total',
                 'NAEM'='Anthropogenic emission categories',
                 'NAEMT'='Anthropogenic emission categories',
                 'NEM'='Emission categories',
                 'NEMT'='Emission categories + Total',
                 'NF'='Feed functions',
                 'NLFTCH4'='Lifetimes of methane',
                 'NM'='Months',
                 'NMT'='Months including mean',
                 'NVUICL'='Vulnerability Index categories',
                 'NAC2T'='area crop type  plus total',
                 'NAEMOECD'='hussled NAEM (emission sources)',
                 'NAPDT'='Air pollution diseases incl total',
                 'NAPYR'='Air pollution output years',
                 'NBDL'='Bio diversity losses',
                 'NBDL6'='Bio diversity losses',
                 'NBDLT'='Biodiversity losses incl total',
                 'NBIOMES'='Biomes',
                 'NDIFFYR'='Globio output years',
                 'NDRT'='Dependency ratios',
                 'NECP3OECD'='OECD 3 primairy energy carriers',
                 'NECP9'='Primairy energy carriers OECD',
                 'NECP9T'='Primairy energy carriers OECD + total',
                 'NECS9OECD'='Secondairy energy carriers',
                 'NECS9OECDT'='OECD secondairy energy carriers',
                 'NEQ3'='GHG gases in equivalents',
                 'NEQ3REV'='GHG gases in equivalents reversed order',
                 'NEQREV'='reversed Contributors to equivalent CO2 concentration',
                 'NNEXPT'='River N Export incl total',
                 'NNIN'='N input terms',
                 'NNOUT'='N output terms',
                 'NOR'='oil reserves/resources',
                 'NR25T'='IMAGE regions plus greenland (OECD viewer)',
                 'NROECD'='OECD aggr feb regions',
                 'NROECD13'='OECD 13 region (version sept 2006)',
                 'NROECD13T'='OECD 13 region plus world (version sept 2006)',
                 'NROECD3'='aggregated OECD 3 region (version sept 2006)',
                 'NROECD3T'='aggregated OECD 3 regionplus world (version sept 2006)',
                 'NROECD6'='OECD, BRICs expliciet, ROW',
                 'NROECDAGGR4'='Regions Aggregated + NON OECD febr version',
                 'NROECDAGGRT'='Regions Aggregated + total, febr version',
                 'NROECDAM'='OECD regions North & South America',
                 'NROECDT'='OECD aggr feb regions world, febr version',
                 'NRQFA'='Quality Forest area',
                 'NS10'='OECD emission sources',
                 'NS10REV'='reversed NS10 dimension',
                 'NS5'='OECD sectors',
                 'NS5T'='OECD sectors',
                 'NSC_1'='OECD scenario combinatie',
                 'NSC_2'='OECD scenario combinatie',
                 'NSC_3'='OECD scenario combinatie',
                 'NSC_4'='OECD scenario combinatie',
                 'NSC_5'='OECD scenario combinatie',
                 'NSC_6'='OECD scenario combinatie',
                 'NSC_7'='OECD scenario combinatie',
                 'NSC2'='OECD scenarios',
                 'NSC4'='OECD scenarios',
                 'NWSS'='Water stress',
                 'NYR'='Year selection + potential',
                 'NAGE'='0-100 year age cohorts: per 5 year',
                 'NAGE21'='age cohorts, from 0-1 year isolated',
                 'NDIS'='disease categories',
                 'NEDUC'='Education levels',
                 'NEXP'='Exposure categories',
                 'NFER'='Age cohorts of fertile woman',
                 'NP'='Population',
                 'NPAR'='Parity fertile woman',
                 'NSEX'='Sexes',
                 'NSEXT'='Sexes incl total',
                 'NUR'='urban - rural',
                 'NURT'='urban - rural + total',
                 'NR'='current IMAGE regions',
                 'NR13'='Old Image regions',
                 'NR13_4_2'='Old IMAGE + IPCC + Annex regions combined',
                 'NR13_4_2T'='Old IMAGE + IPCC + Annex regions combined, incl world',
                 'NR13T'='Old IMAGE incl world',
                 'NR17'='First 17 IMAGE regions',
                 'NR17_4_2'='IMAGE + IPCC + Annex regions combined',
                 'NR17_4_2T'='IMAGE + IPCC + Annex regions combined + total',
                 'NR17T'='First 17 IMAGE regions incl. World',
                 'NR18'='First 18 IMAGE regions',
                 'NR18O'='First 18 regions + Other',
                 'NR18OT'='First 18 regions + Other Inc. World',
                 'NR18T'='First 18 IMAGE regions incl world',
                 'NR24'='Image 24 regions version',
                 'NR24T'='IMAGE 24 regions incl World',
                 'NR27'='Timer 26 regions + dummy',
                 'NR27G'='27 gismo regions',
                 'NR27T'='Timer 26 regions + dummy + world',
                 'NR45'='IMAGE europe version',
                 'NR45G'='IMAGE europe version + Greenland',
                 'NR45GO'='IMAGE europe version + Greenland + other',
                 'NR45GOT'='IMAGE europe version + Greenland + other incl world',
                 'NR45GT'='IMAGE europe version + Greenland incl world',
                 'NR45T'='IMAGE europe version incl world',
                 'NRANNEX'='Annex regions',
                 'NRC'='224 countries op ISO volgorde',
                 'NRG'='current IMAGE regions + Greenland',
                 'NRGO'='current IMAGE regions + Greenland + other',
                 'NRGOT'='current IMAGE regions + Greenland + other + world',
                 'NRGT'='current IMAGE regions + Greenland incl world',
                 'NRIPCC'='IPCC regions',
                 'NRIPCCT'='IPCC regions incl total',
                 'NROECD29'='OECD 29 regions from ENV Linkage (2010)',
                 'NRT'='current IMAGE regions incl world',
                 'REGIONS'='Image 17 regions incl total',
                 'REGIONS27T'='TIMER 26 regions incl dummy + World',
                 'NES'='Sectors related to emissions',
                 'NESI'='sectors rel. to emissions plus industry process',
                 'NESIT'='sectors rel. to emissions plus industry process icl total',
                 'NESREV'='energy sectors reversed oder NES',
                 'NEST'='Sectors related to emissions incl. Total',
                 'NS'='Sectors',
                 'NSB'='Energy sectors for modern biofuels',
                 'NSBT'='Energy sectors for modern biofuels incl. Total',
                 'NSE'='Sectors economic structure',
                 'NSET'='Sectors economic structure incl total',
                 'NSG'='Government sectors',
                 'NSGT'='Government sectors incl total',
                 'NSP'='Production Sectors',
                 'NSPT'='Production Sectors incl total',
                 'NST'='Sectors incl Total',
                 'NTRIP'='Triptych sectors',
                 'NTRIPT'='Triptych sectors incl Total',
                 'RAINS_NS'='Sectors tbv RAINS conversie',
                 'NALL'='cells in ocean grid',
                 'NC'='world grid cells',
                 'NCW'='world gridcells including water/land <50%',
                 'NHEMIS'='2 Latitudinal zones',
                 'NHEMIST'='2 Latitudinal zones + total',
                 'NLATBELT'='18 Latitudinal zones',
                 'NLATI'='Latitudional belts',
                 'NLATIT'='Latitudional belts + total',
                 'NOCLAY'='Number of ocean layers',
                 'NWS'='Water demand sectors',
                 
                 dimname )
  longname <- gsub("\\+", "", longname)
  longname <- gsub("  ", " ", longname)
  return(gsub(" ", "_", longname))
}
# PBL color definitions
colorlist_pbl <- list(
hemelblauw3010 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw3011 = rgb(24, 165, 227, maxColorValue = 255),
hemelblauw3012 = rgb(74, 175, 232, maxColorValue = 255),
hemelblauw3013 = rgb(105, 185, 236, maxColorValue = 255),
hemelblauw3014 = rgb(132, 196, 240, maxColorValue = 255),
hemelblauw3015 = rgb(156, 206, 243, maxColorValue = 255),
hemelblauw3016 = rgb(178, 216, 246, maxColorValue = 255),
hemelblauw3017 = rgb(199, 227, 249, maxColorValue = 255),
hemelblauw3018 = rgb(218, 236, 251, maxColorValue = 255),
hemelblauw3019 = rgb(237, 246, 254, maxColorValue = 255),
mosgroen3020 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen3021 = rgb(152, 154, 52, maxColorValue = 255),
mosgroen3022 = rgb(162, 164, 72, maxColorValue = 255),
mosgroen3023 = rgb(173, 175, 92, maxColorValue = 255),
mosgroen3024 = rgb(184, 185, 114, maxColorValue = 255),
mosgroen3025 = rgb(196, 196, 136, maxColorValue = 255),
mosgroen3026 = rgb(207, 207, 159, maxColorValue = 255),
mosgroen3027 = rgb(218, 219, 182, maxColorValue = 255),
mosgroen3028 = rgb(230, 230, 206, maxColorValue = 255),
mosgroen3029 = rgb(243, 243, 230, maxColorValue = 255),
violet3030 = rgb(157, 0, 100, maxColorValue = 255),
violet3031 = rgb(164, 47, 112, maxColorValue = 255),
violet3032 = rgb(172, 73, 126, maxColorValue = 255),
violet3033 = rgb(180, 97, 141, maxColorValue = 255),
violet3034 = rgb(190, 120, 158, maxColorValue = 255),
violet3035 = rgb(199, 144, 175, maxColorValue = 255),
violet3036 = rgb(210, 167, 192, maxColorValue = 255),
violet3037 = rgb(221, 189, 208, maxColorValue = 255),
violet3038 = rgb(232, 211, 224, maxColorValue = 255),
violet3039 = rgb(243, 233, 240, maxColorValue = 255),
donkergeel3040 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel3041 = rgb(233, 181, 58, maxColorValue = 255),
donkergeel3042 = rgb(236, 189, 82, maxColorValue = 255),
donkergeel3043 = rgb(238, 198, 105, maxColorValue = 255),
donkergeel3044 = rgb(241, 206, 128, maxColorValue = 255),
donkergeel3045 = rgb(243, 214, 150, maxColorValue = 255),
donkergeel3046 = rgb(246, 222, 171, maxColorValue = 255),
donkergeel3047 = rgb(248, 230, 193, maxColorValue = 255),
donkergeel3048 = rgb(250, 239, 214, maxColorValue = 255),
donkergeel3049 = rgb(253, 247, 234, maxColorValue = 255),
paars3050 = rgb(61, 29, 92, maxColorValue = 255),
paars3051 = rgb(73, 44, 102, maxColorValue = 255),
paars3052 = rgb(87, 60, 115, maxColorValue = 255),
paars3053 = rgb(102, 78, 129, maxColorValue = 255),
paars3054 = rgb(120, 98, 144, maxColorValue = 255),
paars3055 = rgb(138, 120, 161, maxColorValue = 255),
paars3056 = rgb(158, 145, 180, maxColorValue = 255),
paars3057 = rgb(180, 170, 198, maxColorValue = 255),
paars3058 = rgb(203, 196, 217, maxColorValue = 255),
paars3059 = rgb(228, 225, 236, maxColorValue = 255),
lichtblauw3060 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw3061 = rgb(175, 213, 243, maxColorValue = 255),
lichtblauw3062 = rgb(185, 218, 245, maxColorValue = 255),
lichtblauw3063 = rgb(195, 223, 247, maxColorValue = 255),
lichtblauw3064 = rgb(204, 228, 248, maxColorValue = 255),
lichtblauw3065 = rgb(213, 232, 249, maxColorValue = 255),
lichtblauw3066 = rgb(221, 237, 251, maxColorValue = 255),
lichtblauw3067 = rgb(230, 242, 252, maxColorValue = 255),
lichtblauw3068 = rgb(238, 246, 253, maxColorValue = 255),
lichtblauw3069 = rgb(247, 251, 254, maxColorValue = 255),
roze3070 = rgb(220, 156, 193, maxColorValue = 255),
roze3071 = rgb(224, 167, 201, maxColorValue = 255),
roze3072 = rgb(227, 178, 208, maxColorValue = 255),
roze3073 = rgb(231, 189, 215, maxColorValue = 255),
roze3074 = rgb(234, 199, 221, maxColorValue = 255),
roze3075 = rgb(238, 209, 228, maxColorValue = 255),
roze3076 = rgb(241, 218, 234, maxColorValue = 255),
roze3077 = rgb(245, 228, 240, maxColorValue = 255),
roze3078 = rgb(248, 237, 245, maxColorValue = 255),
roze3079 = rgb(251, 246, 250, maxColorValue = 255),
groen3080 = rgb(110, 169, 64, maxColorValue = 255),
groen3081 = rgb(126, 178, 78, maxColorValue = 255),
groen3082 = rgb(142, 185, 95, maxColorValue = 255),
groen3083 = rgb(158, 194, 114, maxColorValue = 255),
groen3084 = rgb(173, 203, 135, maxColorValue = 255),
groen3085 = rgb(188, 212, 155, maxColorValue = 255),
groen3086 = rgb(202, 221, 176, maxColorValue = 255),
groen3087 = rgb(216, 230, 197, maxColorValue = 255),
groen3088 = rgb(229, 238, 217, maxColorValue = 255),
groen3089 = rgb(242, 247, 236, maxColorValue = 255),
rood3090 = rgb(195, 33, 23, maxColorValue = 255),
rood3091 = rgb(200, 62, 40, maxColorValue = 255),
rood3092 = rgb(205, 87, 58, maxColorValue = 255),
rood3093 = rgb(210, 111, 78, maxColorValue = 255),
rood3094 = rgb(216, 134, 101, maxColorValue = 255),
rood3095 = rgb(223, 155, 124, maxColorValue = 255),
rood3096 = rgb(229, 177, 150, maxColorValue = 255),
rood3097 = rgb(236, 197, 176, maxColorValue = 255),
rood3098 = rgb(242, 217, 202, maxColorValue = 255),
rood3099 = rgb(249, 236, 228, maxColorValue = 255),
donkergroen3100 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen3101 = rgb(43, 78, 47, maxColorValue = 255),
donkergroen3102 = rgb(61, 92, 60, maxColorValue = 255),
donkergroen3103 = rgb(79, 107, 77, maxColorValue = 255),
donkergroen3104 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen3105 = rgb(121, 142, 116, maxColorValue = 255),
donkergroen3106 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen3107 = rgb(171, 183, 166, maxColorValue = 255),
donkergroen3108 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen3109 = rgb(225, 229, 223, maxColorValue = 255),
oranje3110 = rgb(209, 106, 25, maxColorValue = 255),
oranje3111 = rgb(213, 122, 48, maxColorValue = 255),
oranje3112 = rgb(218, 137, 68, maxColorValue = 255),
oranje3113 = rgb(223, 153, 90, maxColorValue = 255),
oranje3114 = rgb(227, 169, 113, maxColorValue = 255),
oranje3115 = rgb(232, 184, 136, maxColorValue = 255),
oranje3116 = rgb(237, 199, 160, maxColorValue = 255),
oranje3117 = rgb(242, 213, 183, maxColorValue = 255),
oranje3118 = rgb(246, 228, 207, maxColorValue = 255),
oranje3119 = rgb(250, 241, 231, maxColorValue = 255),
donkerbruin3120 = rgb(76, 37, 21, maxColorValue = 255),
donkerbruin3121 = rgb(90, 51, 36, maxColorValue = 255),
donkerbruin3122 = rgb(105, 66, 51, maxColorValue = 255),
donkerbruin3123 = rgb(120, 83, 67, maxColorValue = 255),
donkerbruin3124 = rgb(136, 102, 87, maxColorValue = 255),
donkerbruin3125 = rgb(154, 124, 109, maxColorValue = 255),
donkerbruin3126 = rgb(172, 147, 134, maxColorValue = 255),
donkerbruin3127 = rgb(192, 172, 161, maxColorValue = 255),
donkerbruin3128 = rgb(212, 198, 189, maxColorValue = 255),
donkerbruin3129 = rgb(233, 225, 221, maxColorValue = 255),
robijnrood3130 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood3131 = rgb(195, 51, 118, maxColorValue = 255),
robijnrood3132 = rgb(200, 80, 132, maxColorValue = 255),
robijnrood3133 = rgb(206, 107, 148, maxColorValue = 255),
robijnrood3134 = rgb(212, 131, 164, maxColorValue = 255),
robijnrood3135 = rgb(219, 154, 180, maxColorValue = 255),
robijnrood3136 = rgb(226, 176, 196, maxColorValue = 255),
robijnrood3137 = rgb(233, 197, 212, maxColorValue = 255),
robijnrood3138 = rgb(241, 217, 227, maxColorValue = 255),
robijnrood3139 = rgb(248, 236, 241, maxColorValue = 255),
bruin3140 = rgb(178, 142, 23, maxColorValue = 255),
bruin3141 = rgb(186, 152, 48, maxColorValue = 255),
bruin3142 = rgb(194, 162, 70, maxColorValue = 255),
bruin3143 = rgb(201, 172, 91, maxColorValue = 255),
bruin3144 = rgb(209, 183, 113, maxColorValue = 255),
bruin3145 = rgb(216, 194, 136, maxColorValue = 255),
bruin3146 = rgb(224, 205, 159, maxColorValue = 255),
bruin3147 = rgb(231, 217, 182, maxColorValue = 255),
bruin3148 = rgb(239, 230, 206, maxColorValue = 255),
bruin3149 = rgb(247, 242, 230, maxColorValue = 255),
mintgroen3150 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen3151 = rgb(167, 207, 198, maxColorValue = 255),
mintgroen3152 = rgb(178, 212, 204, maxColorValue = 255),
mintgroen3153 = rgb(188, 218, 211, maxColorValue = 255),
mintgroen3154 = rgb(199, 223, 217, maxColorValue = 255),
mintgroen3155 = rgb(209, 229, 224, maxColorValue = 255),
mintgroen3156 = rgb(218, 234, 230, maxColorValue = 255),
mintgroen3157 = rgb(228, 239, 237, maxColorValue = 255),
mintgroen3158 = rgb(237, 245, 243, maxColorValue = 255),
mintgroen3159 = rgb(246, 250, 249, maxColorValue = 255),
geel3160 = rgb(255, 238, 54, maxColorValue = 255),
geel3161 = rgb(255, 239, 81, maxColorValue = 255),
geel3162 = rgb(255, 241, 105, maxColorValue = 255),
geel3163 = rgb(255, 243, 128, maxColorValue = 255),
geel3164 = rgb(255, 244, 149, maxColorValue = 255),
geel3165 = rgb(255, 246, 169, maxColorValue = 255),
geel3166 = rgb(255, 248, 188, maxColorValue = 255),
geel3167 = rgb(255, 250, 206, maxColorValue = 255),
geel3168 = rgb(255, 251, 223, maxColorValue = 255),
geel3169 = rgb(255, 253, 239, maxColorValue = 255),
donkerblauw3170 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw3171 = rgb(56, 115, 184, maxColorValue = 255),
donkerblauw3172 = rgb(80, 127, 192, maxColorValue = 255),
donkerblauw3173 = rgb(103, 141, 200, maxColorValue = 255),
donkerblauw3174 = rgb(127, 156, 208, maxColorValue = 255),
donkerblauw3175 = rgb(149, 171, 217, maxColorValue = 255),
donkerblauw3176 = rgb(171, 187, 225, maxColorValue = 255),
donkerblauw3177 = rgb(192, 203, 233, maxColorValue = 255),
donkerblauw3178 = rgb(213, 220, 240, maxColorValue = 255),
donkerblauw3179 = rgb(234, 237, 248, maxColorValue = 255),
roodgeelgroen3200 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen3201 = rgb(203, 86, 29, maxColorValue = 255),
roodgeelgroen3202 = rgb(215, 129, 36, maxColorValue = 255),
roodgeelgroen3203 = rgb(228, 168, 43, maxColorValue = 255),
roodgeelgroen3204 = rgb(242, 205, 50, maxColorValue = 255),
roodgeelgroen3205 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen3206 = rgb(223, 221, 56, maxColorValue = 255),
roodgeelgroen3207 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen3208 = rgb(151, 187, 61, maxColorValue = 255),
roodgeelgroen3209 = rgb(110, 169, 64, maxColorValue = 255),
roodhemelblauw3300 = rgb(195, 33, 23, maxColorValue = 255),
roodhemelblauw3301 = rgb(205, 87, 58, maxColorValue = 255),
roodhemelblauw3302 = rgb(216, 134, 101, maxColorValue = 255),
roodhemelblauw3303 = rgb(229, 177, 150, maxColorValue = 255),
roodhemelblauw3304 = rgb(242, 217, 202, maxColorValue = 255),
roodhemelblauw3305 = rgb(218, 236, 251, maxColorValue = 255),
roodhemelblauw3306 = rgb(178, 216, 246, maxColorValue = 255),
roodhemelblauw3307 = rgb(132, 196, 240, maxColorValue = 255),
roodhemelblauw3308 = rgb(74, 175, 232, maxColorValue = 255),
roodhemelblauw3309 = rgb(0, 156, 223, maxColorValue = 255),
zwart3400 = rgb(0, 0, 0, maxColorValue = 255),
zwart3401 = rgb(38, 38, 38, maxColorValue = 255),
zwart3402 = rgb(77, 77, 77, maxColorValue = 255),
zwart3403 = rgb(102, 102, 102, maxColorValue = 255),
zwart3404 = rgb(128, 128, 128, maxColorValue = 255),
zwart3405 = rgb(153, 153, 153, maxColorValue = 255),
zwart3406 = rgb(191, 191, 191, maxColorValue = 255),
zwart3407 = rgb(204, 204, 204, maxColorValue = 255),
zwart3408 = rgb(230, 230, 230, maxColorValue = 255),
zwart3409 = rgb(242, 242, 242, maxColorValue = 255),
wit = rgb(255, 255, 255, maxColorValue = 255),
hemelblauwdonker3610 = rgb(0, 110, 154, maxColorValue = 255),
hemelblauwdonker3611 = rgb(0, 135, 190, maxColorValue = 255),
mosgroendonker3620 = rgb(100, 104, 22, maxColorValue = 255),
mosgroendonker3621 = rgb(121, 125, 28, maxColorValue = 255),
violetdonker3630 = rgb(111, 0, 70, maxColorValue = 255),
violetdonker3631 = rgb(134, 0, 85, maxColorValue = 255),
geeldonker3640 = rgb(202, 133, 24, maxColorValue = 255),
geeldonker3641 = rgb(215, 152, 24, maxColorValue = 255),
groendonker3680 = rgb(33, 128, 62, maxColorValue = 255),
groendonker3681 = rgb(69, 146, 64, maxColorValue = 255),
rooddonker3690 = rgb(134, 30, 4, maxColorValue = 255),
rooddonker3691 = rgb(165, 33, 18, maxColorValue = 255),
hemelblauw10_1 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw10_2 = rgb(24, 165, 227, maxColorValue = 255),
hemelblauw10_3 = rgb(74, 175, 232, maxColorValue = 255),
hemelblauw10_4 = rgb(105, 185, 236, maxColorValue = 255),
hemelblauw10_5 = rgb(132, 196, 240, maxColorValue = 255),
hemelblauw10_6 = rgb(156, 206, 243, maxColorValue = 255),
hemelblauw10_7 = rgb(178, 216, 246, maxColorValue = 255),
hemelblauw10_8 = rgb(199, 227, 249, maxColorValue = 255),
hemelblauw10_9 = rgb(218, 236, 251, maxColorValue = 255),
hemelblauw10_10 = rgb(237, 246, 254, maxColorValue = 255),
hemelblauw5_1 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw5_2 = rgb(74, 175, 232, maxColorValue = 255),
hemelblauw5_3 = rgb(132, 196, 240, maxColorValue = 255),
hemelblauw5_4 = rgb(178, 216, 246, maxColorValue = 255),
hemelblauw5_5 = rgb(218, 236, 251, maxColorValue = 255),
hemelblauw4_1 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw4_2 = rgb(105, 185, 236, maxColorValue = 255),
hemelblauw4_3 = rgb(178, 216, 246, maxColorValue = 255),
hemelblauw4_4 = rgb(218, 236, 251, maxColorValue = 255),
hemelblauw3_1 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw3_2 = rgb(132, 196, 240, maxColorValue = 255),
hemelblauw3_3 = rgb(218, 236, 251, maxColorValue = 255),
hemelblauw2_1 = rgb(0, 156, 223, maxColorValue = 255),
hemelblauw2_2 = rgb(156, 206, 243, maxColorValue = 255),
hemelblauw1_1 = rgb(0, 156, 223, maxColorValue = 255),
mosgroen10_1 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen10_2 = rgb(152, 154, 52, maxColorValue = 255),
mosgroen10_3 = rgb(162, 164, 72, maxColorValue = 255),
mosgroen10_4 = rgb(173, 175, 92, maxColorValue = 255),
mosgroen10_5 = rgb(184, 185, 114, maxColorValue = 255),
mosgroen10_6 = rgb(196, 196, 136, maxColorValue = 255),
mosgroen10_7 = rgb(207, 207, 159, maxColorValue = 255),
mosgroen10_8 = rgb(218, 219, 182, maxColorValue = 255),
mosgroen10_9 = rgb(230, 230, 206, maxColorValue = 255),
mosgroen10_10 = rgb(243, 243, 230, maxColorValue = 255),
mosgroen5_1 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen5_2 = rgb(162, 164, 72, maxColorValue = 255),
mosgroen5_3 = rgb(184, 185, 114, maxColorValue = 255),
mosgroen5_4 = rgb(207, 207, 159, maxColorValue = 255),
mosgroen5_5 = rgb(230, 230, 206, maxColorValue = 255),
mosgroen4_1 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen4_2 = rgb(173, 175, 92, maxColorValue = 255),
mosgroen4_3 = rgb(207, 207, 159, maxColorValue = 255),
mosgroen4_4 = rgb(230, 230, 206, maxColorValue = 255),
mosgroen3_1 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen3_2 = rgb(184, 185, 114, maxColorValue = 255),
mosgroen3_3 = rgb(230, 230, 206, maxColorValue = 255),
mosgroen2_1 = rgb(141, 145, 31, maxColorValue = 255),
mosgroen2_2 = rgb(196, 196, 136, maxColorValue = 255),
mosgroen1_1 = rgb(141, 145, 31, maxColorValue = 255),
violet10_1 = rgb(157, 0, 100, maxColorValue = 255),
violet10_2 = rgb(164, 47, 112, maxColorValue = 255),
violet10_3 = rgb(172, 73, 126, maxColorValue = 255),
violet10_4 = rgb(180, 97, 141, maxColorValue = 255),
violet10_5 = rgb(190, 120, 158, maxColorValue = 255),
violet10_6 = rgb(199, 144, 175, maxColorValue = 255),
violet10_7 = rgb(210, 167, 192, maxColorValue = 255),
violet10_8 = rgb(221, 189, 208, maxColorValue = 255),
violet10_9 = rgb(232, 211, 224, maxColorValue = 255),
violet10_10 = rgb(243, 233, 240, maxColorValue = 255),
violet5_1 = rgb(157, 0, 100, maxColorValue = 255),
violet5_2 = rgb(172, 73, 126, maxColorValue = 255),
violet5_3 = rgb(190, 120, 158, maxColorValue = 255),
violet5_4 = rgb(210, 167, 192, maxColorValue = 255),
violet5_5 = rgb(232, 211, 224, maxColorValue = 255),
violet4_1 = rgb(157, 0, 100, maxColorValue = 255),
violet4_2 = rgb(180, 97, 141, maxColorValue = 255),
violet4_3 = rgb(210, 167, 192, maxColorValue = 255),
violet4_4 = rgb(232, 211, 224, maxColorValue = 255),
violet3_1 = rgb(157, 0, 100, maxColorValue = 255),
violet3_2 = rgb(190, 120, 158, maxColorValue = 255),
violet3_3 = rgb(232, 211, 224, maxColorValue = 255),
violet2_1 = rgb(157, 0, 100, maxColorValue = 255),
violet2_2 = rgb(199, 144, 175, maxColorValue = 255),
violet1_1 = rgb(157, 0, 100, maxColorValue = 255),
donkergeel10_1 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel10_2 = rgb(233, 181, 58, maxColorValue = 255),
donkergeel10_3 = rgb(236, 189, 82, maxColorValue = 255),
donkergeel10_4 = rgb(238, 198, 105, maxColorValue = 255),
donkergeel10_5 = rgb(241, 206, 128, maxColorValue = 255),
donkergeel10_6 = rgb(243, 214, 150, maxColorValue = 255),
donkergeel10_7 = rgb(246, 222, 171, maxColorValue = 255),
donkergeel10_8 = rgb(248, 230, 193, maxColorValue = 255),
donkergeel10_9 = rgb(250, 239, 214, maxColorValue = 255),
donkergeel10_10 = rgb(253, 247, 234, maxColorValue = 255),
donkergeel5_1 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel5_2 = rgb(236, 189, 82, maxColorValue = 255),
donkergeel5_3 = rgb(241, 206, 128, maxColorValue = 255),
donkergeel5_4 = rgb(246, 222, 171, maxColorValue = 255),
donkergeel5_5 = rgb(250, 239, 214, maxColorValue = 255),
donkergeel4_1 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel4_2 = rgb(238, 198, 105, maxColorValue = 255),
donkergeel4_3 = rgb(246, 222, 171, maxColorValue = 255),
donkergeel4_4 = rgb(250, 239, 214, maxColorValue = 255),
donkergeel3_1 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel3_2 = rgb(241, 206, 128, maxColorValue = 255),
donkergeel3_3 = rgb(250, 239, 214, maxColorValue = 255),
donkergeel2_1 = rgb(230, 173, 31, maxColorValue = 255),
donkergeel2_2 = rgb(243, 214, 150, maxColorValue = 255),
donkergeel1_1 = rgb(230, 173, 31, maxColorValue = 255),
paars10_1 = rgb(61, 29, 92, maxColorValue = 255),
paars10_2 = rgb(73, 44, 102, maxColorValue = 255),
paars10_3 = rgb(87, 60, 115, maxColorValue = 255),
paars10_4 = rgb(102, 78, 129, maxColorValue = 255),
paars10_5 = rgb(120, 98, 144, maxColorValue = 255),
paars10_6 = rgb(138, 120, 161, maxColorValue = 255),
paars10_7 = rgb(158, 145, 180, maxColorValue = 255),
paars10_8 = rgb(180, 170, 198, maxColorValue = 255),
paars10_9 = rgb(203, 196, 217, maxColorValue = 255),
paars10_10 = rgb(228, 225, 236, maxColorValue = 255),
paars5_1 = rgb(61, 29, 92, maxColorValue = 255),
paars5_2 = rgb(87, 60, 115, maxColorValue = 255),
paars5_3 = rgb(120, 98, 144, maxColorValue = 255),
paars5_4 = rgb(158, 145, 180, maxColorValue = 255),
paars5_5 = rgb(203, 196, 217, maxColorValue = 255),
paars4_1 = rgb(61, 29, 92, maxColorValue = 255),
paars4_2 = rgb(102, 78, 129, maxColorValue = 255),
paars4_3 = rgb(158, 145, 180, maxColorValue = 255),
paars4_4 = rgb(203, 196, 217, maxColorValue = 255),
paars3_1 = rgb(61, 29, 92, maxColorValue = 255),
paars3_2 = rgb(120, 98, 144, maxColorValue = 255),
paars3_3 = rgb(203, 196, 217, maxColorValue = 255),
paars2_1 = rgb(61, 29, 92, maxColorValue = 255),
paars2_2 = rgb(138, 120, 161, maxColorValue = 255),
paars1_1 = rgb(61, 29, 92, maxColorValue = 255),
lichtblauw10_1 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw10_2 = rgb(175, 213, 243, maxColorValue = 255),
lichtblauw10_3 = rgb(185, 218, 245, maxColorValue = 255),
lichtblauw10_4 = rgb(195, 223, 247, maxColorValue = 255),
lichtblauw10_5 = rgb(204, 228, 248, maxColorValue = 255),
lichtblauw10_6 = rgb(213, 232, 249, maxColorValue = 255),
lichtblauw10_7 = rgb(221, 237, 251, maxColorValue = 255),
lichtblauw10_8 = rgb(230, 242, 252, maxColorValue = 255),
lichtblauw10_9 = rgb(238, 246, 253, maxColorValue = 255),
lichtblauw10_10 = rgb(247, 251, 254, maxColorValue = 255),
lichtblauw5_1 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw5_2 = rgb(185, 218, 245, maxColorValue = 255),
lichtblauw5_3 = rgb(204, 228, 248, maxColorValue = 255),
lichtblauw5_4 = rgb(221, 237, 251, maxColorValue = 255),
lichtblauw5_5 = rgb(238, 246, 253, maxColorValue = 255),
lichtblauw4_1 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw4_2 = rgb(195, 223, 247, maxColorValue = 255),
lichtblauw4_3 = rgb(221, 237, 251, maxColorValue = 255),
lichtblauw4_4 = rgb(238, 246, 253, maxColorValue = 255),
lichtblauw3_1 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw3_2 = rgb(204, 228, 248, maxColorValue = 255),
lichtblauw3_3 = rgb(238, 246, 253, maxColorValue = 255),
lichtblauw2_1 = rgb(165, 209, 242, maxColorValue = 255),
lichtblauw2_2 = rgb(213, 232, 249, maxColorValue = 255),
lichtblauw1_1 = rgb(165, 209, 242, maxColorValue = 255),
roze10_1 = rgb(220, 156, 193, maxColorValue = 255),
roze10_2 = rgb(224, 167, 201, maxColorValue = 255),
roze10_3 = rgb(227, 178, 208, maxColorValue = 255),
roze10_4 = rgb(231, 189, 215, maxColorValue = 255),
roze10_5 = rgb(234, 199, 221, maxColorValue = 255),
roze10_6 = rgb(238, 209, 228, maxColorValue = 255),
roze10_7 = rgb(241, 218, 234, maxColorValue = 255),
roze10_8 = rgb(245, 228, 240, maxColorValue = 255),
roze10_9 = rgb(248, 237, 245, maxColorValue = 255),
roze10_10 = rgb(251, 246, 250, maxColorValue = 255),
roze5_1 = rgb(220, 156, 193, maxColorValue = 255),
roze5_2 = rgb(227, 178, 208, maxColorValue = 255),
roze5_3 = rgb(234, 199, 221, maxColorValue = 255),
roze5_4 = rgb(241, 218, 234, maxColorValue = 255),
roze5_5 = rgb(248, 237, 245, maxColorValue = 255),
roze4_1 = rgb(220, 156, 193, maxColorValue = 255),
roze4_2 = rgb(231, 189, 215, maxColorValue = 255),
roze4_3 = rgb(241, 218, 234, maxColorValue = 255),
roze4_4 = rgb(248, 237, 245, maxColorValue = 255),
roze3_1 = rgb(220, 156, 193, maxColorValue = 255),
roze3_2 = rgb(234, 199, 221, maxColorValue = 255),
roze3_3 = rgb(248, 237, 245, maxColorValue = 255),
roze2_1 = rgb(220, 156, 193, maxColorValue = 255),
roze2_2 = rgb(238, 209, 228, maxColorValue = 255),
roze1_1 = rgb(220, 156, 193, maxColorValue = 255),
groen10_1 = rgb(110, 169, 64, maxColorValue = 255),
groen10_2 = rgb(126, 178, 78, maxColorValue = 255),
groen10_3 = rgb(142, 185, 95, maxColorValue = 255),
groen10_4 = rgb(158, 194, 114, maxColorValue = 255),
groen10_5 = rgb(173, 203, 135, maxColorValue = 255),
groen10_6 = rgb(188, 212, 155, maxColorValue = 255),
groen10_7 = rgb(202, 221, 176, maxColorValue = 255),
groen10_8 = rgb(216, 230, 197, maxColorValue = 255),
groen10_9 = rgb(229, 238, 217, maxColorValue = 255),
groen10_10 = rgb(242, 247, 236, maxColorValue = 255),
groen5_1 = rgb(110, 169, 64, maxColorValue = 255),
groen5_2 = rgb(142, 185, 95, maxColorValue = 255),
groen5_3 = rgb(173, 203, 135, maxColorValue = 255),
groen5_4 = rgb(202, 221, 176, maxColorValue = 255),
groen5_5 = rgb(229, 238, 217, maxColorValue = 255),
groen4_1 = rgb(110, 169, 64, maxColorValue = 255),
groen4_2 = rgb(158, 194, 114, maxColorValue = 255),
groen4_3 = rgb(202, 221, 176, maxColorValue = 255),
groen4_4 = rgb(229, 238, 217, maxColorValue = 255),
groen3_1 = rgb(110, 169, 64, maxColorValue = 255),
groen3_2 = rgb(173, 203, 135, maxColorValue = 255),
groen3_3 = rgb(229, 238, 217, maxColorValue = 255),
groen2_1 = rgb(110, 169, 64, maxColorValue = 255),
groen2_2 = rgb(188, 212, 155, maxColorValue = 255),
groen1_1 = rgb(110, 169, 64, maxColorValue = 255),
rood10_1 = rgb(195, 33, 23, maxColorValue = 255),
rood10_2 = rgb(200, 62, 40, maxColorValue = 255),
rood10_3 = rgb(205, 87, 58, maxColorValue = 255),
rood10_4 = rgb(210, 111, 78, maxColorValue = 255),
rood10_5 = rgb(216, 134, 101, maxColorValue = 255),
rood10_6 = rgb(223, 155, 124, maxColorValue = 255),
rood10_7 = rgb(229, 177, 150, maxColorValue = 255),
rood10_8 = rgb(236, 197, 176, maxColorValue = 255),
rood10_9 = rgb(242, 217, 202, maxColorValue = 255),
rood10_10 = rgb(249, 236, 228, maxColorValue = 255),
rood5_1 = rgb(195, 33, 23, maxColorValue = 255),
rood5_2 = rgb(205, 87, 58, maxColorValue = 255),
rood5_3 = rgb(216, 134, 101, maxColorValue = 255),
rood5_4 = rgb(229, 177, 150, maxColorValue = 255),
rood5_5 = rgb(242, 217, 202, maxColorValue = 255),
rood4_1 = rgb(195, 33, 23, maxColorValue = 255),
rood4_2 = rgb(210, 111, 78, maxColorValue = 255),
rood4_3 = rgb(229, 177, 150, maxColorValue = 255),
rood4_4 = rgb(242, 217, 202, maxColorValue = 255),
rood3_1 = rgb(195, 33, 23, maxColorValue = 255),
rood3_2 = rgb(216, 134, 101, maxColorValue = 255),
rood3_3 = rgb(242, 217, 202, maxColorValue = 255),
rood2_1 = rgb(195, 33, 23, maxColorValue = 255),
rood2_2 = rgb(223, 155, 124, maxColorValue = 255),
rood1_1 = rgb(195, 33, 23, maxColorValue = 255),
donkergroen10_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen10_2 = rgb(43, 78, 47, maxColorValue = 255),
donkergroen10_3 = rgb(61, 92, 60, maxColorValue = 255),
donkergroen10_4 = rgb(79, 107, 77, maxColorValue = 255),
donkergroen10_5 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen10_6 = rgb(121, 142, 116, maxColorValue = 255),
donkergroen10_7 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen10_8 = rgb(171, 183, 166, maxColorValue = 255),
donkergroen10_9 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen10_10 = rgb(225, 229, 223, maxColorValue = 255),
donkergroen5_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen5_2 = rgb(61, 92, 60, maxColorValue = 255),
donkergroen5_3 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen5_4 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen5_5 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen4_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen4_2 = rgb(79, 107, 77, maxColorValue = 255),
donkergroen4_3 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen4_4 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen3_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen3_2 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen3_3 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen2_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen2_2 = rgb(121, 142, 116, maxColorValue = 255),
donkergroen1_1 = rgb(26, 66, 34, maxColorValue = 255),
oranje10_1 = rgb(209, 106, 25, maxColorValue = 255),
oranje10_2 = rgb(213, 122, 48, maxColorValue = 255),
oranje10_3 = rgb(218, 137, 68, maxColorValue = 255),
oranje10_4 = rgb(223, 153, 90, maxColorValue = 255),
oranje10_5 = rgb(227, 169, 113, maxColorValue = 255),
oranje10_6 = rgb(232, 184, 136, maxColorValue = 255),
oranje10_7 = rgb(237, 199, 160, maxColorValue = 255),
oranje10_8 = rgb(242, 213, 183, maxColorValue = 255),
oranje10_9 = rgb(246, 228, 207, maxColorValue = 255),
oranje10_10 = rgb(250, 241, 231, maxColorValue = 255),
oranje5_1 = rgb(209, 106, 25, maxColorValue = 255),
oranje5_2 = rgb(218, 137, 68, maxColorValue = 255),
oranje5_3 = rgb(227, 169, 113, maxColorValue = 255),
oranje5_4 = rgb(237, 199, 160, maxColorValue = 255),
oranje5_5 = rgb(246, 228, 207, maxColorValue = 255),
oranje4_1 = rgb(209, 106, 25, maxColorValue = 255),
oranje4_2 = rgb(223, 153, 90, maxColorValue = 255),
oranje4_3 = rgb(237, 199, 160, maxColorValue = 255),
oranje4_4 = rgb(246, 228, 207, maxColorValue = 255),
oranje3_1 = rgb(209, 106, 25, maxColorValue = 255),
oranje3_2 = rgb(227, 169, 113, maxColorValue = 255),
oranje3_3 = rgb(246, 228, 207, maxColorValue = 255),
oranje2_1 = rgb(209, 106, 25, maxColorValue = 255),
oranje2_2 = rgb(232, 184, 136, maxColorValue = 255),
oranje1_1 = rgb(209, 106, 25, maxColorValue = 255),
donkergroen10_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen10_2 = rgb(43, 78, 47, maxColorValue = 255),
donkergroen10_3 = rgb(61, 92, 60, maxColorValue = 255),
donkergroen10_4 = rgb(79, 107, 77, maxColorValue = 255),
donkergroen10_5 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen10_6 = rgb(121, 142, 116, maxColorValue = 255),
donkergroen10_7 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen10_8 = rgb(171, 183, 166, maxColorValue = 255),
donkergroen10_9 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen10_10 = rgb(225, 229, 223, maxColorValue = 255),
donkergroen5_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen5_2 = rgb(61, 92, 60, maxColorValue = 255),
donkergroen5_3 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen5_4 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen5_5 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen4_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen4_2 = rgb(79, 107, 77, maxColorValue = 255),
donkergroen4_3 = rgb(145, 162, 140, maxColorValue = 255),
donkergroen4_4 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen3_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen3_2 = rgb(99, 124, 95, maxColorValue = 255),
donkergroen3_3 = rgb(197, 205, 193, maxColorValue = 255),
donkergroen2_1 = rgb(26, 66, 34, maxColorValue = 255),
donkergroen2_2 = rgb(121, 142, 116, maxColorValue = 255),
donkergroen1_1 = rgb(26, 66, 34, maxColorValue = 255),
robijnrood10_1 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood10_2 = rgb(195, 51, 118, maxColorValue = 255),
robijnrood10_3 = rgb(200, 80, 132, maxColorValue = 255),
robijnrood10_4 = rgb(206, 107, 148, maxColorValue = 255),
robijnrood10_5 = rgb(212, 131, 164, maxColorValue = 255),
robijnrood10_6 = rgb(219, 154, 180, maxColorValue = 255),
robijnrood10_7 = rgb(226, 176, 196, maxColorValue = 255),
robijnrood10_8 = rgb(233, 197, 212, maxColorValue = 255),
robijnrood10_9 = rgb(241, 217, 227, maxColorValue = 255),
robijnrood10_10 = rgb(248, 236, 241, maxColorValue = 255),
robijnrood5_1 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood5_2 = rgb(200, 80, 132, maxColorValue = 255),
robijnrood5_3 = rgb(212, 131, 164, maxColorValue = 255),
robijnrood5_4 = rgb(226, 176, 196, maxColorValue = 255),
robijnrood5_5 = rgb(241, 217, 227, maxColorValue = 255),
robijnrood4_1 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood4_2 = rgb(206, 107, 148, maxColorValue = 255),
robijnrood4_3 = rgb(226, 176, 196, maxColorValue = 255),
robijnrood4_4 = rgb(241, 217, 227, maxColorValue = 255),
robijnrood3_1 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood3_2 = rgb(212, 131, 164, maxColorValue = 255),
robijnrood3_3 = rgb(241, 217, 227, maxColorValue = 255),
robijnrood2_1 = rgb(191, 0, 105, maxColorValue = 255),
robijnrood2_2 = rgb(219, 154, 180, maxColorValue = 255),
robijnrood1_1 = rgb(191, 0, 105, maxColorValue = 255),
bruin10_1 = rgb(178, 142, 23, maxColorValue = 255),
bruin10_2 = rgb(186, 152, 48, maxColorValue = 255),
bruin10_3 = rgb(194, 162, 70, maxColorValue = 255),
bruin10_4 = rgb(201, 172, 91, maxColorValue = 255),
bruin10_5 = rgb(209, 183, 113, maxColorValue = 255),
bruin10_6 = rgb(216, 194, 136, maxColorValue = 255),
bruin10_7 = rgb(224, 205, 159, maxColorValue = 255),
bruin10_8 = rgb(231, 217, 182, maxColorValue = 255),
bruin10_9 = rgb(239, 230, 206, maxColorValue = 255),
bruin10_10 = rgb(247, 242, 230, maxColorValue = 255),
bruin5_1 = rgb(178, 142, 23, maxColorValue = 255),
bruin5_2 = rgb(194, 162, 70, maxColorValue = 255),
bruin5_3 = rgb(209, 183, 113, maxColorValue = 255),
bruin5_4 = rgb(224, 205, 159, maxColorValue = 255),
bruin5_5 = rgb(239, 230, 206, maxColorValue = 255),
bruin4_1 = rgb(178, 142, 23, maxColorValue = 255),
bruin4_2 = rgb(201, 172, 91, maxColorValue = 255),
bruin4_3 = rgb(224, 205, 159, maxColorValue = 255),
bruin4_4 = rgb(239, 230, 206, maxColorValue = 255),
bruin3_1 = rgb(178, 142, 23, maxColorValue = 255),
bruin3_2 = rgb(209, 183, 113, maxColorValue = 255),
bruin3_3 = rgb(239, 230, 206, maxColorValue = 255),
bruin2_1 = rgb(178, 142, 23, maxColorValue = 255),
bruin2_2 = rgb(216, 194, 136, maxColorValue = 255),
bruin1_1 = rgb(178, 142, 23, maxColorValue = 255),
mintgroen10_1 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen10_2 = rgb(167, 207, 198, maxColorValue = 255),
mintgroen10_3 = rgb(178, 212, 204, maxColorValue = 255),
mintgroen10_4 = rgb(188, 218, 211, maxColorValue = 255),
mintgroen10_5 = rgb(199, 223, 217, maxColorValue = 255),
mintgroen10_6 = rgb(209, 229, 224, maxColorValue = 255),
mintgroen10_7 = rgb(218, 234, 230, maxColorValue = 255),
mintgroen10_8 = rgb(228, 239, 237, maxColorValue = 255),
mintgroen10_9 = rgb(237, 245, 243, maxColorValue = 255),
mintgroen10_10 = rgb(246, 250, 249, maxColorValue = 255),
mintgroen5_1 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen5_2 = rgb(178, 212, 204, maxColorValue = 255),
mintgroen5_3 = rgb(199, 223, 217, maxColorValue = 255),
mintgroen5_4 = rgb(218, 234, 230, maxColorValue = 255),
mintgroen5_5 = rgb(237, 245, 243, maxColorValue = 255),
mintgroen4_1 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen4_2 = rgb(188, 218, 211, maxColorValue = 255),
mintgroen4_3 = rgb(218, 234, 230, maxColorValue = 255),
mintgroen4_4 = rgb(237, 245, 243, maxColorValue = 255),
mintgroen3_1 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen3_2 = rgb(199, 223, 217, maxColorValue = 255),
mintgroen3_3 = rgb(237, 245, 243, maxColorValue = 255),
mintgroen2_1 = rgb(156, 201, 191, maxColorValue = 255),
mintgroen2_2 = rgb(209, 229, 224, maxColorValue = 255),
mintgroen1_1 = rgb(156, 201, 191, maxColorValue = 255),
geel10_1 = rgb(255, 238, 54, maxColorValue = 255),
geel10_2 = rgb(255, 239, 81, maxColorValue = 255),
geel10_3 = rgb(255, 241, 105, maxColorValue = 255),
geel10_4 = rgb(255, 243, 128, maxColorValue = 255),
geel10_5 = rgb(255, 244, 149, maxColorValue = 255),
geel10_6 = rgb(255, 246, 169, maxColorValue = 255),
geel10_7 = rgb(255, 248, 188, maxColorValue = 255),
geel10_8 = rgb(255, 250, 206, maxColorValue = 255),
geel10_9 = rgb(255, 251, 223, maxColorValue = 255),
geel10_10 = rgb(255, 253, 239, maxColorValue = 255),
geel5_1 = rgb(255, 238, 54, maxColorValue = 255),
geel5_2 = rgb(255, 241, 105, maxColorValue = 255),
geel5_3 = rgb(255, 244, 149, maxColorValue = 255),
geel5_4 = rgb(255, 248, 188, maxColorValue = 255),
geel5_5 = rgb(255, 251, 223, maxColorValue = 255),
geel4_1 = rgb(255, 238, 54, maxColorValue = 255),
geel4_2 = rgb(255, 243, 128, maxColorValue = 255),
geel4_3 = rgb(255, 248, 188, maxColorValue = 255),
geel4_4 = rgb(255, 251, 223, maxColorValue = 255),
geel3_1 = rgb(255, 238, 54, maxColorValue = 255),
geel3_2 = rgb(255, 244, 149, maxColorValue = 255),
geel3_3 = rgb(255, 251, 223, maxColorValue = 255),
geel2_1 = rgb(255, 238, 54, maxColorValue = 255),
geel2_2 = rgb(255, 246, 169, maxColorValue = 255),
geel1_1 = rgb(255, 238, 54, maxColorValue = 255),
donkerblauw10_1 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw10_2 = rgb(56, 115, 184, maxColorValue = 255),
donkerblauw10_3 = rgb(80, 127, 192, maxColorValue = 255),
donkerblauw10_4 = rgb(103, 141, 200, maxColorValue = 255),
donkerblauw10_5 = rgb(127, 156, 208, maxColorValue = 255),
donkerblauw10_6 = rgb(149, 171, 217, maxColorValue = 255),
donkerblauw10_7 = rgb(171, 187, 225, maxColorValue = 255),
donkerblauw10_8 = rgb(192, 203, 233, maxColorValue = 255),
donkerblauw10_9 = rgb(213, 220, 240, maxColorValue = 255),
donkerblauw10_10 = rgb(234, 237, 248, maxColorValue = 255),
donkerblauw5_1 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw5_2 = rgb(80, 127, 192, maxColorValue = 255),
donkerblauw5_3 = rgb(127, 156, 208, maxColorValue = 255),
donkerblauw5_4 = rgb(171, 187, 225, maxColorValue = 255),
donkerblauw5_5 = rgb(213, 220, 240, maxColorValue = 255),
donkerblauw4_1 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw4_2 = rgb(103, 141, 200, maxColorValue = 255),
donkerblauw4_3 = rgb(171, 187, 225, maxColorValue = 255),
donkerblauw4_4 = rgb(213, 220, 240, maxColorValue = 255),
donkerblauw3_1 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw3_2 = rgb(127, 156, 208, maxColorValue = 255),
donkerblauw3_3 = rgb(213, 220, 240, maxColorValue = 255),
donkerblauw2_1 = rgb(23, 103, 175, maxColorValue = 255),
donkerblauw2_2 = rgb(149, 171, 217, maxColorValue = 255),
donkerblauw1_1 = rgb(23, 103, 175, maxColorValue = 255),
roodgeelgroen10_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen10_2 = rgb(203, 86, 29, maxColorValue = 255),
roodgeelgroen10_3 = rgb(215, 129, 36, maxColorValue = 255),
roodgeelgroen10_4 = rgb(228, 168, 43, maxColorValue = 255),
roodgeelgroen10_5 = rgb(242, 205, 50, maxColorValue = 255),
roodgeelgroen10_6 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen10_7 = rgb(223, 221, 56, maxColorValue = 255),
roodgeelgroen10_8 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen10_9 = rgb(151, 187, 61, maxColorValue = 255),
roodgeelgroen10_10 = rgb(110, 169, 64, maxColorValue = 255),
roodgeelgroen9_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen9_2 = rgb(203, 86, 29, maxColorValue = 255),
roodgeelgroen9_3 = rgb(215, 129, 36, maxColorValue = 255),
roodgeelgroen9_4 = rgb(228, 168, 43, maxColorValue = 255),
roodgeelgroen9_5 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen9_6 = rgb(223, 221, 56, maxColorValue = 255),
roodgeelgroen9_7 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen9_8 = rgb(151, 187, 61, maxColorValue = 255),
roodgeelgroen9_9 = rgb(110, 169, 64, maxColorValue = 255),
roodgeelgroen7_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen7_2 = rgb(203, 86, 29, maxColorValue = 255),
roodgeelgroen7_3 = rgb(228, 168, 43, maxColorValue = 255),
roodgeelgroen7_4 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen7_5 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen7_6 = rgb(151, 187, 61, maxColorValue = 255),
roodgeelgroen7_7 = rgb(110, 169, 64, maxColorValue = 255),
roodgeelgroen5_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen5_2 = rgb(215, 129, 36, maxColorValue = 255),
roodgeelgroen5_3 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen5_4 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen5_5 = rgb(110, 169, 64, maxColorValue = 255),
roodgeelgroen4_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen4_2 = rgb(215, 129, 36, maxColorValue = 255),
roodgeelgroen4_3 = rgb(189, 205, 59, maxColorValue = 255),
roodgeelgroen4_4 = rgb(110, 169, 64, maxColorValue = 255),
roodgeelgroen3_1 = rgb(195, 33, 23, maxColorValue = 255),
roodgeelgroen3_2 = rgb(255, 238, 54, maxColorValue = 255),
roodgeelgroen3_3 = rgb(110, 169, 64, maxColorValue = 255),
roodhemelblauw10_1 = rgb(195, 33, 23, maxColorValue = 255),
roodhemelblauw10_2 = rgb(205, 87, 58, maxColorValue = 255),
roodhemelblauw10_3 = rgb(216, 134, 101, maxColorValue = 255),
roodhemelblauw10_4 = rgb(229, 177, 150, maxColorValue = 255),
roodhemelblauw10_5 = rgb(242, 217, 202, maxColorValue = 255),
roodhemelblauw10_6 = rgb(218, 236, 251, maxColorValue = 255),
roodhemelblauw10_7 = rgb(178, 216, 246, maxColorValue = 255),
roodhemelblauw10_8 = rgb(132, 196, 240, maxColorValue = 255),
roodhemelblauw10_9 = rgb(74, 175, 232, maxColorValue = 255),
roodhemelblauw10_10 = rgb(0, 156, 223, maxColorValue = 255),
roodhemelblauw6_1 = rgb(195, 33, 23, maxColorValue = 255),
roodhemelblauw6_2 = rgb(205, 87, 58, maxColorValue = 255),
roodhemelblauw6_3 = rgb(229, 177, 150, maxColorValue = 255),
roodhemelblauw6_4 = rgb(178, 216, 246, maxColorValue = 255),
roodhemelblauw6_5 = rgb(74, 175, 232, maxColorValue = 255),
roodhemelblauw6_6 = rgb(0, 156, 223, maxColorValue = 255),
roodhemelblauw4_1 = rgb(195, 33, 23, maxColorValue = 255),
roodhemelblauw4_2 = rgb(229, 177, 150, maxColorValue = 255),
roodhemelblauw4_3 = rgb(178, 216, 246, maxColorValue = 255),
roodhemelblauw4_4 = rgb(0, 156, 223, maxColorValue = 255),
roodhemelblauw2_1 = rgb(195, 33, 23, maxColorValue = 255),
roodhemelblauw2_2 = rgb(0, 156, 223, maxColorValue = 255),
zwart10_1 = rgb(0, 0, 0, maxColorValue = 255),
zwart10_2 = rgb(38, 38, 38, maxColorValue = 255),
zwart10_3 = rgb(77, 77, 77, maxColorValue = 255),
zwart10_4 = rgb(102, 102, 102, maxColorValue = 255),
zwart10_5 = rgb(128, 128, 128, maxColorValue = 255),
zwart10_6 = rgb(153, 153, 153, maxColorValue = 255),
zwart10_7 = rgb(191, 191, 191, maxColorValue = 255),
zwart10_8 = rgb(204, 204, 204, maxColorValue = 255),
zwart10_9 = rgb(230, 230, 230, maxColorValue = 255),
zwart10_10 = rgb(242, 242, 242, maxColorValue = 255),
zwart5_1 = rgb(0, 0, 0, maxColorValue = 255),
zwart5_2 = rgb(77, 77, 77, maxColorValue = 255),
zwart5_3 = rgb(128, 128, 128, maxColorValue = 255),
zwart5_4 = rgb(191, 191, 191, maxColorValue = 255),
zwart5_5 = rgb(230, 230, 230, maxColorValue = 255),
zwart4_1 = rgb(77, 77, 77, maxColorValue = 255),
zwart4_2 = rgb(128, 128, 128, maxColorValue = 255),
zwart4_3 = rgb(191, 191, 191, maxColorValue = 255),
zwart4_4 = rgb(230, 230, 230, maxColorValue = 255),
zwart3_1 = rgb(77, 77, 77, maxColorValue = 255),
zwart3_2 = rgb(128, 128, 128, maxColorValue = 255),
zwart3_3 = rgb(191, 191, 191, maxColorValue = 255),
zwart2_1 = rgb(77, 77, 77, maxColorValue = 255),
zwart2_2 = rgb(191, 191, 191, maxColorValue = 255),
zwart1_1 = rgb(77, 77, 77, maxColorValue = 255)
)


