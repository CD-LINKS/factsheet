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
    x_N_2=v[l-2]
    x_N_1=v[l-1]
    x_N=v[l]
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


