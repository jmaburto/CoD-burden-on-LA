
###############################################################################
### Program to create homogeneous datasets with all countries                 #
###############################################################################

# Classification of causes of death
# 1 total deaths
# 2 Certain infectious and parasitic diseases                           A00-B99
# 3 Neoplasms                                                      C00-D48
# 4 Diseases of the circulatory system                        I00-I99
# 5 Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                             R00-R99
# 6 Mental and behavioural disorders F01-F99
# 7 Diseases of the nervous system            G00-G98
# 8 Endocrine, nutritional and metabolic diseases                 E00-E88 
# 9 Diseases of the digestive system K00-K92
# 10 Diseases of the genitourinary system               N00-N98
# 11 P00-P96          Certain conditions originating in the perinatal period & Q00-Q99                Congenital malformations, deformations and chromosomal abnormalities
# 12 Diseases of the respiratory system    J00-J98
# 13 External causes of morbidity and mortality     V01-Y89 minus homicide
# 14 X85-Y09 Assault - homicide


library(data.table)
library(reshape2)

setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')
source('R/7_1Europe.R')

cause.name.vec <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                    'Nervous','Endocrine','Digestive',
                    'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')


Deaths.data <- Deaths.data[Deaths.data$Age < 96,]
Deaths.data$Age3<- Deaths.data$Age2
Deaths.data[Deaths.data$Age2 >= 1 & Deaths.data$Age2 <= 4]$Age3 <- 1
CoD.data       <- Deaths.data[,list(Female =sum(Female)), by = list(X,Country,Year,Cause,Age3)]
CoD.data$Male  <- Deaths.data[,list(Male =sum(Male)), by = list(X,Country,Year,Cause,Age3)]$Male
### Exclude ages 96 and 97, UNK and total

# ###Get HMD Data
# library(HMDHFDplus)
# XYZ <- getHMDcountries()
# XYZ <-c('AUS','BEL','DNK','FIN','FRATNP','DEUTNP','GRC','IRL','ITA','LUX','NLD','PRT','ESP','SWE','GBR_NP')
# 
# us <- "HMD_User"
# pw <- "Password"
# HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
#   cat(x,"\n")
#   Males        <- readHMDweb(x,"mltper_5x5",username=us,password=pw,)
#   Females      <- readHMDweb(x,"fltper_5x5",username=us,password=pw)
#   Males$Sex    <- "m"
#   Females$Sex  <- "f"
#   CTRY         <- rbind(Females, Males)
#   CTRY$PopName <- x
#   CTRY    
# }, us = us, pw = pw))
# 
# HMDL <- data.table(HMDL)
# HMDL <- HMDL[HMDL$Year >= 1990,]
#save(HMDL,file = 'Data/HMD.RData')

load('Data/HMD.RData')


### group causes of death by 5 years, 1990-1994, 1995-199-, etc.
CoD.data$Period <-  cut(CoD.data$Year,breaks = rev(c(1989,1994,1999,2004,2009,2014)),
                        labels = c('1990','1995','2000','2005','2010'))

CoD.data$Period <- as.numeric(as.character(CoD.data$Period))
CoD.data$Period2 <- CoD.data$Period
CoD.data$Period2 <- factor(CoD.data$Period2,levels=seq(1990,2010,5),labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2014'))



CoD.data2      <- CoD.data[, list(Female=sum(Female)),by= list(X,Country,Cause,Age3,Period,Period2)]
CoD.data2$Male <- CoD.data[, list(Male=sum(Male)),by= list(X,Country,Cause,Age3,Period,Period2)]$Male

### Add code to HMD to have comparable datasets by country
Country.code.vec <- c(4010,4020,4050,4070,4080,4085,4140,4170,4180,4190,4210,4240,4280,4290,4308)
Country.code.vec2 <- c(4010,4020,4050,4070,4080,4085,4140,4170,4180,4190,4210,4240,4280,4290,4308)

Country.name.vec <- toupper(c('Austria','Belgium','Denmark','Finland','France','Germany','Greece','Ireland','Italy',
                              'Luxembourg','Netherlands','Portugal','Spain','Sweden','UK'))

Country.HMD      <- c('AUS','BEL','DNK','FIN','FRATNP','DEUTNP','GRC','IRL','ITA','LUX','NLD','PRT','ESP','SWE','GBR_NP')
names(Country.code.vec2) <- Country.HMD

HMDL$X <- Country.code.vec2[as.character(HMDL$PopName)]


### Now do decompositions
Countries <- unique(HMDL$X)
Sex       <- c('f','m')
source('R/Functions_LT.R')
Europe_decomp <- NULL

#i <- 4020
for (i in Countries){
  new.CoD.data <- CoD.data2[CoD.data2$X == i, ]
  new.HMD      <- HMDL[HMDL$X == i, ]
  P1 <- unique(new.CoD.data$Period)
  P2 <- unique(HMDL$Year)
  
  for (j in P1[-length(P1)]){
    for (k in c('f','m')){
      CoD.data3 <- new.CoD.data[new.CoD.data$Period == j,]
      CoD.data4 <- new.CoD.data[new.CoD.data$Period == j+5,]
      
      lifetable <- new.HMD[new.HMD$Year == j & new.HMD$Sex == k]
      lifetable2 <- new.HMD[new.HMD$Year == j+5 & new.HMD$Sex == k]
      
      ## convert CoD.data in a matrix and add the column that is missing
      sex.col <- 7
      if (k=='m') sex.col <- 8
      M1 <- acast(CoD.data3,Age3~Cause, value.var = colnames(CoD.data3)[sex.col],fill=0,drop=F)
      M2 <- acast(CoD.data4,Age3~Cause, value.var = colnames(CoD.data3)[sex.col],fill=0,drop=F)
      
      new1 <- M1[,1]-rowSums(M1[,2:14])
      new2 <- M2[,1]-rowSums(M2[,2:14])
      
      print(new1[new1 <0])
      print(new2[new2 <0])
      
      M1 <- cbind(M1[,-1],new1)
      M2 <- cbind(M2[,-1],new2)
      
      M1 <- M1/rowSums(M1)
      M2 <- M2/rowSums(M2)
      M1[is.na(M1[,14]),14] <- 1
      M2[is.na(M2[,14]),14] <- 1
      M2[is.na(M2)] <- 0
      M1[is.na(M1)] <- 0
      colnames(M1) <- 2:15
      colnames(M2) <- 2:15
      
      Decomp <- AgeDecomp2(lifetable,lifetable2)
      Decomp[is.na(Decomp)] <- 0
      
      dd  <- length(Decomp)
      dd2 <- dim(M1)[1]
      mis <- dd-dd2
      md <- matrix(c(rep(0,dim(M1)[2]*mis-mis),rep(1,mis)),mis,dim(M1)[2])
      M1 <- rbind(M1,md)
      M2 <- rbind(M2,md)
      rownames(M1) <- lifetable$Age
      rownames(M2) <- lifetable$Age
      
      Decomp.CoD <- Decomp*((M2*lifetable2$mx)-(M1*lifetable$mx))/(lifetable2$mx-lifetable$mx)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp))
      
      
      
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- j
      Results$Period2 <- j+5
      Results$Sex     <- k
      Results$Country <- i
      Results$Country.name <- CoD.data3$Country[1]
      Results$e01     <- lifetable$ex[1]
      Results$e02     <- lifetable2$ex[1]
      Europe_decomp  <- rbind(Europe_decomp,Results)
      }
  }
}

save(Europe_decomp, file = 'Outcomes/Europe_decomp.RData')