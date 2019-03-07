library(tidyverse)
library(readxl)

DDT_colnum <- function(alpha)
{ if (alpha==0.001) {colnum=2}
  else if(alpha==0.002) {colnum=3}
  else if(alpha==0.005) {colnum=4}
  else if(alpha==0.01) {colnum=5}
  else if(alpha==0.02) {colnum=6}
  else if(alpha==0.05) {colnum=7}
  else if(alpha==0.1) {colnum=8}
  else if(alpha==0.2) {colnum=9}
  else {colnum=9999}
  
}

DeanDixonTest <- function (v, alpha=0.05, decrease=TRUE)
{ 
  # alpha can be 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2 
  #http://www.statistics4u.com/fundstat_eng/cc_outlier_tests_dixon.html  
  DDT <- read.csv("data/DeanDixonTable.csv", sep=";")
  DDT <- gather(DDT, 2:9, key=alpha, value=value)
  DDT$alpha=str_replace_all(DDT$alpha,"alpha.","") %>% as.double()
  
  l <- as.integer(length(v))
  a <- alpha
  v <- sort(v, decreasing=decrease)
  
  if(l >= 3){
    x_1=v[1]
    x_2=v[2] 
    x_3=v[3]
    x_N_2=v[N-2]
    x_N_1=v[N-1]
    x_N=v[N]
  } 
    
  if(l<3) {
    outlier <- FALSE
  } else if (l>=3 & l<=7) {
         Q10=abs(x_2-x_1)/abs(x_N-x_N_1)
         R10 <- read_excel("data/critical values Dean and Dixon test.xlsx", sheet="r10")
         rnum <- which(R10$N==l)
         cnum <- DDT_colnum(a)
         r10 = R10[rnum,cnum]
         if (Q10>r10) outlier <- TRUE else outlier <- FALSE
         } else if (l>=8 & l<=10) {
                Q11=abs(x_2-x_1)/abs(x_N_1-x_1)
                R11 <- read_excel("data/critical values Dean and Dixon test.xlsx", sheet="r11")
                rnum <- which(R11$N==l)
                cnum <- DDT_colnum(a)
                r11 = R11[rnum,cnum]
                if (Q11>r11) outlier <- TRUE else outlier <- FALSE
                } else if (l>=11 & l<=13) {
                       Q21=abs(x_3-x_1)/abs(x_N_1-x_1)
                       R21 <- read_excel("data/critical values Dean and Dixon test.xlsx", sheet="r21") 
                       rnum <- which(R21$N==l)
                       cnum <- DDT_colnum(a)
                       r21 = R21[rnum,cnum]
                       if (Q21>r21) outlier <- TRUE else outlier <- FALSE
                       } else if(l>=14) { 
                              Q22=abs(x_3-x_1)/abs(x_N_2-x_1)
                              R22 <- read_excel("data/critical values Dean and Dixon test.xlsx", sheet="r22")
                              rnum <- which(R22$N==l)
                              cnum <- DDT_colnum(a)
                              r22 = R22[rnum,cnum]
                              if (Q22>r22) outlier <- TRUE else outlier <- FALSE
                              }
  outlier
}


regs <- c("World", "BRA",  "CHN", "EU",  "IND", "JPN", "RUS", "USA")
outlier_IAM <- NA
outlier_IAM <- data.frame(region=character(), model=character(), location=character(), outlier=logical())
outlier_IAM$location <- factor(outlier_IAM$location, levels=c('small', 'large'))
IAM_models <- unique(all_paper$model)
outlier_IAM$model <- factor(outlier_IAM$model, levels=IAM_models)

for (r in regs){
  cat(r,"\n")
  r="BRA"
  d_NDC_largest <- filter(all_paper, Category=="NDC", region==r, period==2030, variable=="Emissions|Kyoto Gases") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(desc(value))
  d_NDC_smallest <- filter(all_paper, Category=="NDC", region==r, period==2030, variable=="Emissions|Kyoto Gases") %>%
    select(Category, model, region, period, value, variable) %>%
    arrange(value) 

  cat(" - test largest value for outlier")
  dl <- as.vector(d_NDC_largest$value)
  dl_outlier <- DeanDixonTest(dl, 0.1,TRUE)
  xl <- data.frame(region=r, model=d_NDC_largest[1,]$model, location="large", outlier=dl_outlier)
  outlier_IAM <- rbind(outlier_IAM, xl)
  
  cat(" - test largest value for outlier")
  ds <- as.vector(d_NDC_smallest$value)
  ds_outlier <- DeanDixonTest(ds, 0.1,TRUE)
  xs <- data.frame(region=r, model=d_NDC_smallest[1,]$model, location="small", outlier=ds_outlier)
  outlier_IAM <- rbind(outlier_IAM, xs)
  
}


d_NPi <- filter(all_paper, Category=="NPi", region=="EU", period==2030, variable=="Emissions|Kyoto Gases") %>%
  select(Category, model, region, period, value, variable) %>%
  arrange(value)
d_NPi <- as.vector(d_NPi$value)
t_NPi_largest <- DeanDixonTest(d_NPi, 0.1,TRUE)
t_NPi_smallest <- DeanDixonTest(d_NPi, 0.1,FALSE)
