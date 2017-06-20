library(data.table)
library(reshape2)

setwd('C:/Users/jmaburto/Documents/GitHub Backup 2/CoD-burden-on-LA')
#source('R/7_1Europe.R')

load('Data/Europe_Data.RData')


### group causes of death by 2010-2015 or +
CoD.data        <- Deaths.data
unique(CoD.data$Year)
CoD.data$Period <-  2010
CoD.data$Period2 <- CoD.data$Period
CoD.data$Period2 <- factor(CoD.data$Period2,levels=2010,labels=c('2010-2015'))

CoD.data2      <- CoD.data[, list(Female=sum(Female)),by= list(X,Cause,Age,Country,Age2,Period2)]
CoD.data2$Male <- CoD.data[, list(Male=sum(Male)),by= list(X,Cause,Age,Country,Age2,Period2)]$Male

### Add code to HMD to have comparable datasets by country
Country.code.vec <- c(4010,4020,4050,4070,4080,4085,4170,4180,4190,4210,4240,4280,4290,4308,4140)
Country.code.vec2 <- c(4010,4020,4050,4070,4080,4085,4170,4180,4190,4210,4240,4280,4290,4308,4140)

Country.name.vec <- toupper(c('Austria','Belgium','Denmark','Finland','France','Germany','Ireland','Italy',
                              'Luxembourg','Netherlands','Portugal','Spain','Sweden','UK','Greece'))

Country.HMD      <- c('AUT','BEL','DNK','FIN','FRATNP','DEUTNP','IRL','ITA','LUX','NLD','PRT','ESP','SWE','GBR_NP','GRC')
names(Country.code.vec2) <- Country.HMD

HMDL$X <- Country.code.vec2[as.character(HMDL$PopName)]


### Now create a master lifetable for Europe and CoD proportions (remember to add category 15)
Countries <- unique(HMDL$X)
Sex       <- c('f','m')


Master_mx.f <- rowMeans(acast(HMDL[HMDL$Sex == 'f'],Age ~ X, value.var = 'mx'),na.rm = T)
Master_mx.m <- rowMeans(acast(HMDL[HMDL$Sex == 'm'],Age ~ X, value.var = 'mx'),na.rm = T)
Master_mx.f[19] <- mean(Master_mx.f[19:24],na.rm = T) 
Master_mx.f <- Master_mx.f[1:19]
Master_mx.m[19] <- mean(Master_mx.m[19:24],na.rm = T) 
Master_mx.m <- Master_mx.m[1:19]

### Now a master CoD matrix
CoD.data3.f <- CoD.data2[,sum(Female),by = list(Cause,Age2)]
new.cat <- acast(CoD.data3.f, Age2~Cause, value.var = "V1")[,1]-rowSums(acast(CoD.data3.f, Age2~Cause, value.var = "V1")[,2:14])
Master.CoD.f <- cbind(acast(CoD.data3.f, Age2~Cause, value.var = "V1")[,2:14],new.cat)
colnames(Master.CoD.f) <- 2:15

CoD.data3.m <- CoD.data2[,sum(Male),by = list(Cause,Age2)]
new.cat <- acast(CoD.data3.m, Age2~Cause, value.var = "V1")[,1]-rowSums(acast(CoD.data3.m, Age2~Cause, value.var = "V1")[,2:14])
Master.CoD.m <- cbind(acast(CoD.data3.m, Age2~Cause, value.var = "V1")[,2:14],new.cat)
colnames(Master.CoD.m) <- 2:15

# now convert to proportions
t1 <- rowSums(Master.CoD.f)
t2 <- rowSums(Master.CoD.m)
Master.CoD.f <- Master.CoD.f/t1
Master.CoD.m <- Master.CoD.m/t2

#### Now do decomposition
load("Outcomes/Harmonized_CoDData.RData")
load('Outcomes/Data_Lifetables.RData')


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,Master_mx.m,sure=T)
source('R/Functions_LT.R')


Decomp.comparision <- NULL


UND <-   Rest[(Rest$X == 2170|Rest$X == 2140|Rest$X == 2190|Rest$X == 2380|Rest$X == 2440|
                 Rest$X == 2250|Rest$X == 2310|Rest$X == 2340|Rest$X == 2280|Rest$X == 2350|Rest$X == 2290|
                 Rest$X == 2020|Rest$X == 2120|Rest$X == 2130|
                 Rest$X == 2180|Rest$X == 2360|Rest$X == 2370|
                 Rest$X == 2460|Rest$X == 2070|Rest$X == 2470|
                 Rest$X == 2150),]

UNLT <-            Data.LT[(Data.LT$Code == 2170|Data.LT$Code == 2140|Data.LT$Code == 2190|Data.LT$Code == 2380|Data.LT$Code == 2440|
                              Data.LT$Code == 2280|Data.LT$Code == 2350|Data.LT$Code == 2290|
                              Data.LT$Code == 2250|Data.LT$Code == 2310|Data.LT$Code == 2340|
                              Data.LT$Code == 2020|Data.LT$Code == 2120|Data.LT$Code == 2130|
                              Data.LT$Code == 2180|Data.LT$Code == 2360|Data.LT$Code == 2370|
                              Data.LT$Code == 2460|Data.LT$Code == 2070|Data.LT$Code == 2470|
                              Data.LT$Code == 2150) & Data.LT$Source == 'UN',]
unique(UND$Country)
unique(UNLT$Country)
unique(UNLT$Year)
unique(UND$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
Periods      <- seq(1990,2010,5)
Period.labels <- unique(UNLT$Period)
UND$Period5       <- (cut(UND$Year+1, breaks=c(Periods,Inf),labels=Period.labels))

UND2 <- UND[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
UND2$Male <- UND[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
unique(UND2$Period5)

UND3 <- UND2[UND2$Period5=='2010-2015',]

### Now a master CoD matrix
CoD.data.LAC.f <- UND3[,sum(Female),by = list(Cause,Age2)]
Master.CoD.fLAC  <- acast(CoD.data.LAC.f, Age2~Cause, value.var = "V1")

CoD.data.LAC.m <- UND3[,sum(Male),by = list(Cause,Age2)]
Master.CoD.mLAC  <- acast(CoD.data.LAC.m, Age2~Cause, value.var = "V1")

# now convert to proportions
t1.LAC <- rowSums(Master.CoD.fLAC)
t2.LAC <- rowSums(Master.CoD.mLAC)
Master.CoD.FLAC <- Master.CoD.fLAC/t1.LAC
Master.CoD.MLAC <- Master.CoD.mLAC/t2.LAC



### Ok, we can perform decomp for UN
UNLT$Sex <- as.numeric(UNLT$Sex)
Sex       <- unique(UNLT$Sex)
Countries <- unique(UND2$X)
UND2$Period5 <- as.character(UND2$Period5)

#k <- 2
#l <-3
#j <-2280
#j <-2290
#j <-2350
## Because everything is different and we need separate files for each country, I'll do this with loops

for (j in Countries){
  print(j)
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    
    if (k == 1) Master.mx <- Master_mx.m
    if (k == 2) Master.mx <- Master_mx.f
    
    if (k == 1) Master.CoD <- Master.CoD.m
    if (k == 2) Master.CoD <- Master.CoD.f
    # subset data with info requireMethods
    new.data <- UNLT[UNLT$Code==j & UNLT$Sex==k,]
    new.CoD  <- UND2[UND2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    l <- length(Periods)
    
    
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      
      if (rowSums(R1)[dim(R1)[1]]==0) R1[dim(R1)[1],dim(R1)[2]] <- 1
      
      if (rowSums(R1)[dim(R1)[1]-1]==0) R1[dim(R1)[1]-1,dim(R1)[2]] <- 1
      
      R1.1 <- R1/rowSums(R1)
      
      if (sum(rowSums(R1.1)) != 19) print('Dimension R1')
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- Master.mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      R2.1 <- Master.CoD
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'UN'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.comparision  <- rbind(Decomp.comparision,Results)
      }
}

Results <- NULL
for (k in Sex){
  if (k == 1) Master.mx <- Master_mx.m
  if (k == 2) Master.mx <- Master_mx.f
  
  if (k == 1) Master.CoD <- Master.CoD.m
  if (k == 2) Master.CoD <- Master.CoD.f
  
  if (k == 1) Master.mx.LAC <- Data.LT[Data.LT$Country=='LATIN AMERICA' & Data.LT$Source=='UN' &
                                         Data.LT$Sex =='1' & Data.LT$Period=='2010-2015']
  if (k == 2) Master.mx.LAC <- Data.LT[Data.LT$Country=='LATIN AMERICA' & Data.LT$Source=='UN' &
                                         Data.LT$Sex =='2' & Data.LT$Period=='2010-2015']
  
  if (k == 1) Master.CoD.LAC <- Master.CoD.MLAC
  if (k == 2) Master.CoD.LAC <- Master.CoD.FLAC
  
  
  
  R1.1 <- Master.CoD.LAC
  R2.1 <- Master.CoD
  
  
  mx1 <- Master.mx.LAC$mx
  mx2 <- Master.mx
  Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
  print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
  # calculate cause-specific contributions
  Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
  Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
  Decomp.CoD[is.na(Decomp.CoD)] <- 0
  print(sum(Decomp.CoD)-sum(Decomp.age))
  # Dataframe with decomposition results
  Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
  Results$Period1 <- '2010-2015'
  Results$Age     <- rep(Master.mx.LAC$Age,14)
  Results$Sex     <- k
  Results$Country <- 9999
  Results$Sources <- 'UN'
  Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
  Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
  Decomp.comparision  <- rbind(Decomp.comparision,Results)
}



## Now code mortality from age 80 as 'Rest' since it is unreliable
Decomp.comparision <- data.table(Decomp.comparision)
Decomp.comparision[Decomp.comparision$Age >= 80]$Cause <- 15 
Decomp.comparision <- Decomp.comparision[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Period1,Sex,Country,Sources,e01,e02)]


unique(Decomp.comparision$Country)

cause.name.vec      <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                         'Nervous','Endocrine','Digestive',
                         'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')
Decomp.comparision$Cause.name <- Decomp.comparision$Cause
Decomp.comparision$Cause.name <- factor(Decomp.comparision$Cause.name, levels = 1:15, labels = cause.name.vec)

Country.name.vec <- toupper(c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                              'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                              'Honduras','Mexico','Nicaragua','Panama','Argentina',
                              'Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela',
                              'Haiti','Bolivia','Brazil', 'Latin America'))

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                      2340,2350,2020,2120,2130,2180,2360,2370,2460,2470,2270,2060,2070,9999)
names(Country.code.vec) <- Country.name.vec

Decomp.comparision$Country.name <- Decomp.comparision$Country
Decomp.comparision$Country.name <- factor(Decomp.comparision$Country.name, levels = Country.code.vec, 
                                      labels = Country.name.vec)
unique(Decomp.comparision$Country.name)
Decomp.comparision <- Decomp.comparision

Decomp.comparision[Decomp.comparision$Country.name=='PARAGUAY',]


Decomp.comparision[Decomp.comparision$Period1 == '2010-2015']$Period1 <- '2010-2014'




save(Decomp.comparision, file = 'Outcomes/Decomp_results_Europe.RData')
save(Decomp.comparision, file = 'R/Decomp_App/Decomp_results_Europe.RData')
save(Decomp.comparision, file = 'R/Decomp_Ranking_App/Decomp_results_Europe.RData')


