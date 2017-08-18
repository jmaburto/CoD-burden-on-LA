library(data.table)
library(reshape2)

setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')


load('Data/Europe_Data.RData')


# Get master tables for Europe --------------------------------------------


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


# Get master tables for LAC -----------------------------------------------
load("Outcomes/Harmonized_CoDData.RData")
load('Outcomes/Data_Lifetables.RData')


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,Master_mx.m,sure=T)
source('R/Functions_LT.R')

######### Get master mx and CoD data for High e0
###  CHILE, COSTA RICA, PUERTO RICO, CUBA, URUGUAY, PANAMA, MEXICO.
###  2120, 2140, 2380, 2150, 2460, 2350, 2310
CoD.1 <-   Rest[(Rest$X == 2120|Rest$X == 2140|Rest$X == 2380|Rest$X == 2150|Rest$X == 2460|
                 Rest$X == 2350|Rest$X == 2310) & Rest$Year >= 2010,]

LAC.1 <-            Data.LT[(Data.LT$Code == 2120|Data.LT$Code == 2140|Data.LT$Code == 2380|
                               Data.LT$Code == 2150|Data.LT$Code == 2460|
                              Data.LT$Code == 2350|Data.LT$Code == 2310) & Data.LT$Source == 'UN' & 
                              Data.LT$Period == '2010-2015',]
unique(CoD.1$Country)
unique(LAC.1$Country)
unique(CoD.1$Year)
unique(LAC.1$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
### Now a master CoD matrix

CoD.data.LAC.1.f <- CoD.1[,sum(Female),by = list(Cause,Age2)]
Master.CoD.fLAC.1  <- acast(CoD.data.LAC.1.f, Age2~Cause, value.var = "V1")

CoD.data.LAC.1.m <- CoD.1[,sum(Male),by = list(Cause,Age2)]
Master.CoD.mLAC.1  <- acast(CoD.data.LAC.1.m, Age2~Cause, value.var = "V1")

# now convert to proportions
t1.LAC <- rowSums(Master.CoD.fLAC.1)
t2.LAC <- rowSums(Master.CoD.mLAC.1)
Master.CoD.FLAC.1 <- Master.CoD.fLAC.1/t1.LAC
Master.CoD.MLAC.1 <- Master.CoD.mLAC.1/t2.LAC

## get master mx for LAC 1

LAC.1.mx   <- LAC.1[,list(mx=mean(mx)),by = list(Age,Sex)]
LAC.1.mx.f <- LAC.1.mx[LAC.1.mx$Sex=='2',]$mx
LAC.1.mx.m <- LAC.1.mx[LAC.1.mx$Sex=='1',]$mx


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,
            Master_mx.m,LAC.1.mx.f,LAC.1.mx.m,Master.CoD.FLAC.1,Master.CoD.MLAC.1,sure=T)
source('R/Functions_LT.R')



######### Get master mx and CoD data for medium high e0
###  ARGENTINA, ECUADOR, JAMAICA, NICARAGUA, VENEZUELA, PERU
###  2020, 2180, 2290, 2340, 2470, 2370

CoD.2 <-   Rest[(Rest$X == 2020|Rest$X == 2180|Rest$X == 2290|Rest$X == 2340|Rest$X == 2470|
                   Rest$X == 2370) & Rest$Year >= 2010,]

LAC.2 <-            Data.LT[(Data.LT$Code == 2020|Data.LT$Code == 2180|Data.LT$Code == 2290|
                               Data.LT$Code == 2340|Data.LT$Code == 2470|
                               Data.LT$Code == 2370) & Data.LT$Source == 'UN' & 
                              Data.LT$Period == '2010-2015',]
unique(CoD.2$Country)
unique(LAC.2$Country)
unique(CoD.2$Year)
unique(LAC.2$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
### Now a master CoD matrix

CoD.data.LAC.2.f <- CoD.2[,sum(Female),by = list(Cause,Age2)]
Master.CoD.fLAC.2  <- acast(CoD.data.LAC.2.f, Age2~Cause, value.var = "V1")

CoD.data.LAC.2.m <- CoD.2[,sum(Male),by = list(Cause,Age2)]
Master.CoD.mLAC.2  <- acast(CoD.data.LAC.2.m, Age2~Cause, value.var = "V1")

# now convert to proportions
t1.LAC <- rowSums(Master.CoD.fLAC.2)
t2.LAC <- rowSums(Master.CoD.mLAC.2)
Master.CoD.FLAC.2 <- Master.CoD.fLAC.2/t1.LAC
Master.CoD.MLAC.2 <- Master.CoD.mLAC.2/t2.LAC

## get master mx for LAC 1

LAC.2.mx   <- LAC.2[,list(mx=mean(mx)),by = list(Age,Sex)]
LAC.2.mx.f <- LAC.2.mx[LAC.2.mx$Sex=='2',]$mx
LAC.2.mx.m <- LAC.2.mx[LAC.2.mx$Sex=='1',]$mx


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,
            Master_mx.m,
            LAC.1.mx.f,LAC.1.mx.m,Master.CoD.FLAC.1,Master.CoD.MLAC.1,
            LAC.2.mx.f,LAC.2.mx.m,Master.CoD.FLAC.2,Master.CoD.MLAC.2,sure=T)
source('R/Functions_LT.R')




######### Get master mx and CoD data for medium low e0
###  COLOMBIA, BRAZUK, DOMINICAN REPUBLIC, HONDURAS, PARAGUAY, EL SALVADOR
###  2130, 2070, 2170, 2280, 2360, 2190


CoD.3 <-   Rest[(Rest$X == 2130|Rest$X == 2070|Rest$X == 2170|Rest$X == 2280|Rest$X == 2360|
                   Rest$X == 2190) & Rest$Year >= 2010,]

LAC.3 <-            Data.LT[(Data.LT$Code == 2130|Data.LT$Code == 2070|Data.LT$Code == 2170|
                               Data.LT$Code == 2280|Data.LT$Code == 2360|
                               Data.LT$Code == 2190) & Data.LT$Source == 'UN' & 
                              Data.LT$Period == '2010-2015',]
unique(CoD.3$Country)
unique(LAC.3$Country)
unique(CoD.3$Year)
unique(LAC.3$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
### Now a master CoD matrix

CoD.data.LAC.3.f <- CoD.3[,sum(Female),by = list(Cause,Age2)]
Master.CoD.fLAC.3  <- acast(CoD.data.LAC.3.f, Age2~Cause, value.var = "V1")

CoD.data.LAC.3.m <- CoD.3[,sum(Male),by = list(Cause,Age2)]
Master.CoD.mLAC.3  <- acast(CoD.data.LAC.3.m, Age2~Cause, value.var = "V1")

# now convert to proportions
t1.LAC <- rowSums(Master.CoD.fLAC.3)
t2.LAC <- rowSums(Master.CoD.mLAC.3)
Master.CoD.FLAC.3 <- Master.CoD.fLAC.3/t1.LAC
Master.CoD.MLAC.3 <- Master.CoD.mLAC.3/t2.LAC

## get master mx for LAC 1

LAC.3.mx   <- LAC.3[,list(mx=mean(mx)),by = list(Age,Sex)]
LAC.3.mx.f <- LAC.3.mx[LAC.3.mx$Sex=='2',]$mx
LAC.3.mx.m <- LAC.3.mx[LAC.3.mx$Sex=='1',]$mx


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,
            Master_mx.m,
            LAC.1.mx.f,LAC.1.mx.m,Master.CoD.FLAC.1,Master.CoD.MLAC.1,
            LAC.2.mx.f,LAC.2.mx.m,Master.CoD.FLAC.2,Master.CoD.MLAC.2,
            LAC.3.mx.f,LAC.3.mx.m,Master.CoD.FLAC.3,Master.CoD.MLAC.3,sure=T)
source('R/Functions_LT.R')



######### Get master mx and CoD data for low e0
###  GUATEMALA, TRINIDAD AND TOBAGO, BOLIVIA AND HAITI
###  2250, 2440, 2060, 2270



CoD.4 <-   Rest[(Rest$X == 2250|Rest$X == 2440|Rest$X == 2060|Rest$X == 2270) & Rest$Year >= 2010,]

LAC.4 <-            Data.LT[(Data.LT$Code == 2250|Data.LT$Code == 2440|Data.LT$Code == 2060|
                               Data.LT$Code == 2270) & Data.LT$Source == 'UN' & 
                              Data.LT$Period == '2010-2015',]
unique(CoD.4$Country)
unique(LAC.4$Country)
unique(CoD.4$Year)
unique(LAC.4$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
### Now a master CoD matrix

CoD.data.LAC.4.f <- CoD.4[,sum(Female),by = list(Cause,Age2)]
Master.CoD.fLAC.4  <- acast(CoD.data.LAC.4.f, Age2~Cause, value.var = "V1")

CoD.data.LAC.4.m <- CoD.4[,sum(Male),by = list(Cause,Age2)]
Master.CoD.mLAC.4  <- acast(CoD.data.LAC.4.m, Age2~Cause, value.var = "V1")

# now convert to proportions
t1.LAC <- rowSums(Master.CoD.fLAC.4)
t2.LAC <- rowSums(Master.CoD.mLAC.4)
Master.CoD.FLAC.4 <- Master.CoD.fLAC.4/t1.LAC
Master.CoD.MLAC.4 <- Master.CoD.mLAC.4/t2.LAC

## get master mx for LAC 1

LAC.4.mx   <- LAC.4[,list(mx=mean(mx)),by = list(Age,Sex)]
LAC.4.mx.f <- LAC.4.mx[LAC.4.mx$Sex=='2',]$mx
LAC.4.mx.m <- LAC.4.mx[LAC.4.mx$Sex=='1',]$mx


gdata::keep(Rest,Data.LT,Master.CoD.f,Master.CoD.m,Master_mx.f,
            Master_mx.m,
            LAC.1.mx.f,LAC.1.mx.m,Master.CoD.FLAC.1,Master.CoD.MLAC.1,
            LAC.2.mx.f,LAC.2.mx.m,Master.CoD.FLAC.2,Master.CoD.MLAC.2,
            LAC.3.mx.f,LAC.3.mx.m,Master.CoD.FLAC.3,Master.CoD.MLAC.3,
            LAC.4.mx.f,LAC.4.mx.m,Master.CoD.FLAC.4,Master.CoD.MLAC.4,sure=T)
source('R/Functions_LT.R')




# Perform decomposition ---------------------------------------------------

Decomp.Rankings <- NULL
Age <- c(0,1,seq(5,85,5))

#### Men
####### High e0
mx1     <- LAC.1.mx.m
mx2     <- Master_mx.m
LAC.CoD <- Master.CoD.MLAC.1
EU.CoD  <- Master.CoD.m
k       <- 1


Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'High life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)

####### Medium high
mx1     <- LAC.2.mx.m
mx2     <- Master_mx.m
LAC.CoD <- Master.CoD.MLAC.2
EU.CoD  <- Master.CoD.m
k       <- 1


Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Medium high life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)

####### Medium low
mx1     <- LAC.3.mx.m
mx2     <- Master_mx.m
LAC.CoD <- Master.CoD.MLAC.3
EU.CoD  <- Master.CoD.m
k       <- 1


Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Medium low life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)


###### low
mx1     <- LAC.4.mx.m
mx2     <- Master_mx.m
LAC.CoD <- Master.CoD.MLAC.4
EU.CoD  <- Master.CoD.m
k       <- 1

Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Low life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)


######## Women
####### High e0
mx1     <- LAC.1.mx.f
mx2     <- Master_mx.f
LAC.CoD <- Master.CoD.FLAC.1
EU.CoD  <- Master.CoD.f
k       <- 2


Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'High life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)

####### Medium high
mx1     <- LAC.2.mx.f
mx2     <- Master_mx.f
LAC.CoD <- Master.CoD.FLAC.2
EU.CoD  <- Master.CoD.f
k       <- 2



Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Medium high life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)

####### Medium low
mx1     <- LAC.3.mx.f
mx2     <- Master_mx.f
LAC.CoD <- Master.CoD.FLAC.3
EU.CoD  <- Master.CoD.f
k       <- 2



Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Medium low life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)


###### low
mx1     <- LAC.4.mx.f
mx2     <- Master_mx.f
LAC.CoD <- Master.CoD.FLAC.4
EU.CoD  <- Master.CoD.f
k       <- 2


Decomp.Age      <- AgeDecomp(mx1, mx2,Age,k)
Decomp.CoD      <- Decomp.Age*((EU.CoD*mx2)-(LAC.CoD*mx1))/(mx2-mx1)
Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
Results$Period1 <- '2010-2014'
Results$Age     <- rep(Age,14)
Results$Sex     <- k
Results$level   <- 'Low life expectancy'
Results$Sources <- 'UN'
Results$e01     <- e0.from.mx(mx = mx1,Ages=Age,Sex=k)
Results$e02     <- e0.from.mx(mx = mx2,Ages=Age,Sex=k)
Decomp.Rankings  <- rbind(Decomp.Rankings,Results)



## Now code mortality from age 80 as 'Rest' since it is unreliable
Decomp.Rankings <- data.table(Decomp.Rankings)
Decomp.Rankings[Decomp.Rankings$Age >= 80]$Cause <- 15 
Decomp.Rankings <- Decomp.Rankings[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Period1,Sex,level,Sources,e01,e02)]

cause.name.vec      <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                         'Nervous','Endocrine','Digestive',
                         'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')
Decomp.Rankings$Cause.name <- Decomp.Rankings$Cause
Decomp.Rankings$Cause.name <- factor(Decomp.Rankings$Cause.name, levels = 1:15, labels = cause.name.vec)


save(Decomp.Rankings, file = 'Outcomes/Decomp_Rankings.RData')
save(Decomp.Rankings, file = 'R/Decomp_Ranking_App/Decomp_Rankings.RData')


