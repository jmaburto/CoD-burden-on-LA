#### Program to decompose life expectancy by age and cause of death
#### Method proposed by Arriaga
setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')
library(reshape2)
library(data.table)


# Load outcomes from data harmonization and proportions by age of causes of death
# 1 is males
source('R/5_Proportions_Age.R')
load('Outcomes/Data_Lifetables.RData')
source('R/Functions_LT.R')
unique(CEPAL2004$Age)
unique(CEPAL2004$Year)
unique(Data.LT$Year)
unique(CEPAL2004$Country)
unique(Data.LT$Country)

unique(Deaths.data[Deaths.data$Country == 'Venezuela']$Year)
# Decomp CEPAL2004 --------------------------------------------------------


### Lets start with CEPAL2004
### The list of countries for which we have information to
### get 1990-1995,1995-2000,200-2005,2005-2010,2010-2015
### These are Dominican Republic, Costa Rica, El Salavador, Guatemala,
### Mexico, Nicaragua, Argentina, Chile, Colombia, Ecuador, Paraguay,
### Peru, Uruguay, Venezuela, Brazil, Cuba

Country.code.vec

CEPAL2004D <-   CEPAL2004[CEPAL2004$X == 2170|CEPAL2004$X == 2140|CEPAL2004$X == 2190|
                            CEPAL2004$X == 2250|CEPAL2004$X == 2310|CEPAL2004$X == 2340|
                            CEPAL2004$X == 2020|CEPAL2004$X == 2120|CEPAL2004$X == 2130|
                            CEPAL2004$X == 2180|CEPAL2004$X == 2360|CEPAL2004$X == 2370|
                            CEPAL2004$X == 2460|CEPAL2004$X == 2470|CEPAL2004$X == 2070|
                            CEPAL2004$X == 2150,]

CEPAL2004LT <-            Data.LT[(Data.LT$Code == 2170|Data.LT$Code == 2140|Data.LT$Code == 2190|
                                    Data.LT$Code == 2250|Data.LT$Code == 2310|Data.LT$Code == 2340|
                                    Data.LT$Code == 2020|Data.LT$Code == 2120|Data.LT$Code == 2130|
                                    Data.LT$Code == 2180|Data.LT$Code == 2360|Data.LT$Code == 2370|
                                    Data.LT$Code == 2460|Data.LT$Code == 2470|Data.LT$Code == 2070|
                                    Data.LT$Code == 2150) & Data.LT$Source == 'CEPAL2004',]
unique(CEPAL2004D$Country)
unique(CEPAL2004LT$Country)
unique(CEPAL2004LT$Year)
unique(CEPAL2004D$Year)

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
Periods      <- seq(1990,2010,5)
Period.labels <- unique(CEPAL2004LT$Period)
CEPAL2004D$Period5       <- (cut(CEPAL2004D$Year+1, breaks=c(Periods,Inf),labels=Period.labels))

CEPAL2004D2 <- CEPAL2004D[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
CEPAL2004D2$Male <- CEPAL2004D[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
unique(CEPAL2004D2$Period5)

unique(CEPAL2004D2$Age2)
unique(CEPAL2004D2$Cause)
table(CEPAL2004D2$Cause,CEPAL2004D2$Age)
table(CEPAL2004LT$Period,CEPAL2004LT$Country)

### Ok, we can perform decomp for CEPAL 2004
CEPAL2004LT$Sex <- as.numeric(CEPAL2004LT$Sex)
Sex       <- unique(CEPAL2004LT$Sex)
Countries <- unique(CEPAL2004D2$X)
CEPAL2004D2$Period5 <- as.character(CEPAL2004D2$Period5)
#k <- Sex[1]
#l <-1
#j <-2020
## Because everything is different and we need separate files for each country, I'll do this with loops
Decomp.results <- NULL
Lifetables     <- NULL

  
  for (j in Countries){
    print(j)
    for (k in Sex){
      if (k == 1) sex.col <- 10
      if (k == 2) sex.col <- 9
      # subset data with info requireMethods
      new.data <- CEPAL2004LT[CEPAL2004LT$Code==j & CEPAL2004LT$Sex==k,]
      new.CoD  <- CEPAL2004D2[CEPAL2004D2$X==j,]
      
      # find out for which periods we have information
      P1      <- unique(new.data$Period)
      P2      <- unique(new.CoD$Period5)
      Periods <- P1
      if ((length(P2) - length(P1))!= 0) print('Problem')
      
      for (l in 1:length(Periods[-length(P2)])){
        print(l)
        # get vectors of mx
        p1.data    <- new.data[new.data$Period == Periods[l],]
        p2.data    <- new.data[new.data$Period == Periods[l+1],]
        
        # get proportions of causes of death for these periods in matrix format
        CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
        CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
        R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
        R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
        if (rowSums(R1)[dim(R1)[1]]==0) R1[dim(R1)[1],dim(R1)[2]] <- 1
        if (rowSums(R2)[dim(R2)[1]]==0) R2[dim(R2)[1],dim(R2)[2]] <- 1
        R1.1 <- R1/rowSums(R1)
        R2.1 <- R2/rowSums(R2)
        if (sum(rowSums(R1.1)) != 18) print('Dimension R1')
        if (sum(rowSums(R2.1)) != 18) print('Dimension R2')
        
        # calculate age decomposition
        mx1 <- p1.data$mx
        mx2 <- p2.data$mx
        Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
        print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
        # calculate cause-specific contributions
        Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
        Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
        Decomp.CoD[is.na(Decomp.CoD)] <- 0
        print(sum(Decomp.CoD)-sum(Decomp.age))
        # Dataframe with decomposition results
        Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
        Results$Period1 <- p1.data$Period[1]
        Results$Period2 <- p2.data$Period[2]
        Results$Age     <- rep(p1.data$Age,14)
        Results$Sex     <- k
        Results$Country <- j
        Results$Sources <- 'CEPAL2004'
        Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
        Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
        Decomp.results  <- rbind(Decomp.results,Results)
        
      }
      
      for (l2 in (1:length(Periods))){
        print(l2)
        # get vectors of mx
        p11.data    <- new.data[new.data$Period == Periods[l2],]
        mx1 <- p11.data$mx
        # Dataframe with lifetables following the same procedure (Preston et al 2001)
        Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
        Lifetable1$Period <- Periods[l2]
        Lifetable1$Sex    <- k
        Lifetable1$Country<- j
        Lifetable1$Source <- 'CEPAL2004'
        
        Lifetables  <- rbind(Lifetables,Lifetable1)
        
      }
      
    }
  }






# Decomp CEPAL 2010 -------------------------------------------------------

gdata::keep(CEPAL2004,cause.name.vec,Countries,Country.name.vec,Country.code.vec,Rest,Lifetables,Decomp.results,Rest,Data.LT,sure=T)
source('R/Functions_LT.R')
Decomp.results[Decomp.results$Sources=='CEPAL2004' & Decomp.results$Country==2470,]
### CEPAL2010, be carefull because CEPAL 2010 ends up lifetable at 100+
### The list of countries for which we have information to
### get 1990-1995,1995-2000,200-2005,2005-2010,2010-2015
### These are Dominican Republic, Costa Rica, El Salavador, Guatemala,
### Mexico, Nicaragua, Argentina, Chile, Colombia, Ecuador, Paraguay,
### Peru, Uruguay, Venezuela, Brazil, Cuba

CEPAL2010D <-   Rest[(Rest$X == 2170|Rest$X == 2140|Rest$X == 2190|
                      Rest$X == 2250|Rest$X == 2310|Rest$X == 2340|
                      Rest$X == 2020|Rest$X == 2120|Rest$X == 2130|
                      Rest$X == 2180|Rest$X == 2360|Rest$X == 2370|
                      Rest$X == 2460|Rest$X == 2070|
                      Rest$X == 2150),]

CEPAL2010LT <-            Data.LT[(Data.LT$Code == 2170|Data.LT$Code == 2140|Data.LT$Code == 2190|
                                     Data.LT$Code == 2250|Data.LT$Code == 2310|Data.LT$Code == 2340|
                                     Data.LT$Code == 2020|Data.LT$Code == 2120|Data.LT$Code == 2130|
                                     Data.LT$Code == 2180|Data.LT$Code == 2360|Data.LT$Code == 2370|
                                     Data.LT$Code == 2460|Data.LT$Code == 2070|
                                     Data.LT$Code == 2150) & Data.LT$Source == 'CEPAL2010',]
unique(CEPAL2010D$Country)
unique(CEPAL2010LT$Country)
unique(CEPAL2010LT$Year)
unique(CEPAL2010D$Year)

Country.code.vec

## Aggregate deaths in periods of 5 years to get more robust CoD decomp
Periods      <- seq(1990,2010,5)
Period.labels <- unique(CEPAL2010LT$Period)
CEPAL2010D$Period5       <- (cut(CEPAL2010D$Year+1, breaks=c(Periods,Inf),labels=Period.labels))

CEPAL2010D2 <- CEPAL2010D[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
CEPAL2010D2$Male <- CEPAL2010D[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
unique(CEPAL2010D2$Period5)

unique(CEPAL2010D2$Age2)
unique(CEPAL2010D2$Cause)
table(CEPAL2010D2$Cause,CEPAL2010D2$Age)
table(CEPAL2010LT$Period,CEPAL2010LT$Country)

### Ok, we can perform decomp for CEPAL 2004
CEPAL2010LT$Sex <- as.numeric(CEPAL2010LT$Sex)
Sex       <- unique(CEPAL2010LT$Sex)
Countries <- unique(CEPAL2010D2$X)
CEPAL2010D2$Period5 <- as.character(CEPAL2010D2$Period5)
#k <- Sex[1]
#l <-1
#j <-2470
## Because everything is different and we need separate files for each country, I'll do this with loops

for (j in Countries){
  print(j)
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- CEPAL2010LT[CEPAL2010LT$Code==j & CEPAL2010LT$Sex==k,]
    new.CoD  <- CEPAL2010D2[CEPAL2010D2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      if (rowSums(R1)[dim(R1)[1]]==0) R1[dim(R1)[1],dim(R1)[2]] <- 1
      if (rowSums(R2)[dim(R2)[1]]==0) R2[dim(R2)[1],dim(R2)[2]] <- 1
      if (rowSums(R1)[dim(R1)[1]-1]==0) R1[dim(R1)[1]-1,dim(R1)[2]] <- 1
      if (rowSums(R2)[dim(R2)[1]]==0) R2[dim(R2)[1],dim(R2)[2]-1] <- 1
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      Extra <- dim(p1.data)[1]- dim(R1.1)[1]
      
      md <- matrix(c(rep(0,dim(R1.1)[2]*Extra-Extra),rep(1,Extra)),Extra,dim(R1.1)[2])
      R1.1 <- rbind(R1.1,md)
      R2.1 <- rbind(R2.1,md)

      
      if (sum(rowSums(R1.1)) != 22) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 22) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'CEPAL2010'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'CEPAL2010'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
}



# Decomp UN ---------------------------------------------------------------

unique(Data.LT[(Data.LT$Code == 2440) & Data.LT$Source == 'UN',]$Period)

source('R/Functions_LT.R')

### UN, be carefull because UN 2010 ends up lifetable at 85+
### The list of countries for which we have information to
### get 1990-1995,1995-2000,200-2005,2005-2010,2010-2015
### These are Dominican Republic, Costa Rica, El Salavador, Guatemala,
### Mexico, Nicaragua, Argentina, Chile, Colombia, Ecuador, Paraguay,
### Peru, Uruguay, Venezuela, Brazil, Cuba

UND <-   Rest[(Rest$X == 2170|Rest$X == 2140|Rest$X == 2190|Rest$X == 2380|Rest$X == 2440|
                        Rest$X == 2250|Rest$X == 2310|Rest$X == 2340|
                        Rest$X == 2020|Rest$X == 2120|Rest$X == 2130|
                        Rest$X == 2180|Rest$X == 2360|Rest$X == 2370|
                        Rest$X == 2460|Rest$X == 2070|Rest$X == 2470|
                        Rest$X == 2150),]

UNLT <-            Data.LT[(Data.LT$Code == 2170|Data.LT$Code == 2140|Data.LT$Code == 2190|Data.LT$Code == 2380|Data.LT$Code == 2440|
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

unique(UND2$Age2)
unique(UND2$Cause)
table(UND2$Cause,UND2$Age)
table(UNLT$Period,UNLT$Country)

### Ok, we can perform decomp for UN
UNLT$Sex <- as.numeric(UNLT$Sex)
Sex       <- unique(UNLT$Sex)
Countries <- unique(UND2$X)
UND2$Period5 <- as.character(UND2$Period5)

Country.code.vec
#k <- 2
#l <-3
#j <-2170
#j <-2150
## Because everything is different and we need separate files for each country, I'll do this with loops

for (j in Countries){
  print(j)
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- UNLT[UNLT$Code==j & UNLT$Sex==k,]
    new.CoD  <- UND2[UND2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      if (rowSums(R1)[dim(R1)[1]]==0) R1[dim(R1)[1],dim(R1)[2]] <- 1
      if (rowSums(R2)[dim(R2)[1]]==0) R2[dim(R2)[1],dim(R2)[2]] <- 1
      if (rowSums(R1)[dim(R1)[1]-1]==0) R1[dim(R1)[1]-1,dim(R1)[2]] <- 1
      if (rowSums(R2)[dim(R2)[1]]==0) R2[dim(R2)[1],dim(R2)[2]-1] <- 1
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      
      
      if (sum(rowSums(R1.1)) != 19) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 19) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'UN'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'UN'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
}






# Perform decomp for those for which we have limited data -----------------

### These guys wil be treated differently
### Honduras, (1990,2008-2013)
### Panama, (1996-2014)
### Haiti (1997,1999,2001-2004) NOT ENOUGH
### Bolivia (2000-2003) NOT ENOUGH
### Jamaica (1990,1991,2000-2006, 2009-2011) Just LT from UN
### Puerto Rico, no info on CoD, lifetables from UN
### Trinidad and Tobago, no info on CoD, lifetables from UN


gdata::keep(CEPAL2004,cause.name.vec,Countries,Country.code.vec,Rest,Lifetables,Decomp.results,Rest,Data.LT,sure=T)
source('R/Functions_LT.R')
### Honduras, (1990,2008-2013)
### 2280

HONDURACEPAL2004 <-   CEPAL2004[(CEPAL2004$X == 2280),]
HONDURASD <-   Rest[(Rest$X == 2280),]
### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)

HONDURAS_04 <-   Data.LT[Data.LT$Code == 2280 & Data.LT$Source == 'CEPAL2004',]
HONDURAS_10 <-   Data.LT[Data.LT$Code == 2280 & Data.LT$Source == 'CEPAL2010',]
HONDURAS_UN <-   Data.LT[Data.LT$Code == 2280 & Data.LT$Source == 'UN',]

HONDURAS_04 <-   HONDURAS_04[HONDURAS_04$Period == '1990-1995' |HONDURAS_04$Period == '2005-2010' ,]
HONDURAS_10 <-   HONDURAS_10[HONDURAS_10$Period == '1990-1995' |HONDURAS_10$Period == '2005-2010',]
HONDURAS_UN <-   HONDURAS_UN[HONDURAS_UN$Period == '1990-1995' |HONDURAS_UN$Period == '2005-2010' ,]

HONDURACEPAL2004$X

## Aggregate deaths in periods  to get more robust CoD decomp
unique(HONDURACEPAL2004$Year)
unique(HONDURASD$Year)


Periods      <- c(1990,2007)
Period.labels <- unique(HONDURAS_04$Period)
HONDURACEPAL2004$Period5       <- (cut(HONDURACEPAL2004$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
HONDURASD$Period5       <- (cut(HONDURASD$Year+1, breaks=c(Periods,Inf),labels=Period.labels))

HONDURASD2 <- HONDURASD[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
HONDURASD2$Male <- HONDURASD[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
unique(HONDURASD2$Period5)

HONDURACEPAL20042 <- HONDURACEPAL2004[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
HONDURACEPAL20042$Male <- HONDURACEPAL2004[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
unique(HONDURACEPAL20042$Period5)

### Ready to decompose for CEPAL2004,CEPAL2010, and UN

### Ok, we can perform decomp for CEPAL 2004
HONDURAS_UN$Sex <- as.numeric(HONDURAS_UN$Sex)
HONDURAS_04$Sex <- as.numeric(HONDURAS_04$Sex)
HONDURAS_10$Sex <- as.numeric(HONDURAS_10$Sex)
Sex       <- unique(HONDURAS_UN$Sex)
Countries <- unique(HONDURACEPAL20042$X)
HONDURACEPAL20042$Period5 <- as.character(HONDURACEPAL20042$Period5)
HONDURASD2$Period5 <- as.character(HONDURASD2$Period5)
  #k <- Sex[1]
#l <-1
#
## Because everything is different and we need separate files for each country, I'll do this with loops
j <-2280
  print(j)
  # For UN
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- HONDURAS_UN[HONDURAS_UN$Code==j & HONDURAS_UN$Sex==k,]
    new.CoD  <- HONDURASD2[HONDURASD2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      
      if (sum(rowSums(R1.1)) != 19) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 19) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'UN'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'UN'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
  
  #For CEPAL2010
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- HONDURAS_10[HONDURAS_10$Code==j & HONDURAS_10$Sex==k,]
    new.CoD  <- HONDURASD2[HONDURASD2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      Extra <- dim(p1.data)[1]- dim(R1.1)[1]
      
      md <- matrix(c(rep(0,dim(R1.1)[2]*Extra-Extra),rep(1,Extra)),Extra,dim(R1.1)[2])
      R1.1 <- rbind(R1.1,md)
      R2.1 <- rbind(R2.1,md)
      
      
      if (sum(rowSums(R1.1)) != 22) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 22) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'CEPAL2010'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'CEPAL2010'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
  
  #For CEPAL 2004
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- HONDURAS_04[HONDURAS_04$Code==j & HONDURAS_04$Sex==k,]
    new.CoD  <- HONDURACEPAL20042[HONDURACEPAL20042$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      if (sum(rowSums(R1.1)) != 18) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 18) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'CEPAL2004'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'CEPAL2004'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }



### Panama, (1996-2014)
### 2350
  
  PANAMA2004 <-   CEPAL2004[(CEPAL2004$X == 2350),]
  PANAMAD <-   Rest[(Rest$X == 2350),]
  ### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)
  
  PANAMA_04 <-   Data.LT[Data.LT$Code == 2350 & Data.LT$Source == 'CEPAL2004',]
  PANAMA_10 <-   Data.LT[Data.LT$Code == 2350 & Data.LT$Source == 'CEPAL2010',]
  PANAMA_UN <-   Data.LT[Data.LT$Code == 2350 & Data.LT$Source == 'UN',]
  
  unique(PANAMA_04$Period)

  PANAMA_04 <-   PANAMA_04[PANAMA_04$Period != "1990-1995" ,]
  PANAMA_10 <-   PANAMA_10[PANAMA_10$Period != "1990-1995" ,]
  PANAMA_UN <-   PANAMA_UN[PANAMA_UN$Period != "1990-1995" ,]
  
  ## Aggregate deaths in periods  to get more robust CoD decomp
  unique(PANAMA2004$Year)
  unique(PANAMAD$Year)
  
  Periods      <- seq(1995,2010,5)
  Period.labels <- unique(PANAMA_10$Period)
  
  PANAMA2004$Period5       <- (cut(PANAMA2004$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
  PANAMAD$Period5       <- (cut(PANAMAD$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
  
  PANAMAD2 <- PANAMAD[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
  PANAMAD2$Male <- PANAMAD[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
  unique(PANAMAD2$Period5)
  
  PANAMA20042 <- PANAMA2004[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
  PANAMA20042$Male <- PANAMA2004[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
  unique(PANAMA20042$Period5)
  
  ### Ready to decompose for CEPAL2004,CEPAL2010, and UN
  
  ### Ok, we can perform decomp for CEPAL 2004
  PANAMA_UN$Sex <- as.numeric(PANAMA_UN$Sex)
  PANAMA_04$Sex <- as.numeric(PANAMA_04$Sex)
  PANAMA_10$Sex <- as.numeric(PANAMA_10$Sex)
  Sex       <- unique(PANAMA_UN$Sex)
  Countries <- unique(PANAMA20042$X)
  PANAMA20042$Period5 <- as.character(PANAMA20042$Period5)
  PANAMAD2$Period5 <- as.character(PANAMAD2$Period5)
  #k <- Sex[1]
  #l <-1
  #
  ## Because everything is different and we need separate files for each country, I'll do this with loops
  j <-2350
  print(j)
  # For UN
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- PANAMA_UN[PANAMA_UN$Code==j & PANAMA_UN$Sex==k,]
    new.CoD  <- PANAMAD2[PANAMAD2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      
      if (sum(rowSums(R1.1)) != 19) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 19) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'UN'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'UN'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
  
  #For CEPAL2010
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- PANAMA_10[PANAMA_10$Code==j & PANAMA_10$Sex==k,]
    new.CoD  <- PANAMAD2[PANAMAD2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      Extra <- dim(p1.data)[1]- dim(R1.1)[1]
      
      md <- matrix(c(rep(0,dim(R1.1)[2]*Extra-Extra),rep(1,Extra)),Extra,dim(R1.1)[2])
      R1.1 <- rbind(R1.1,md)
      R2.1 <- rbind(R2.1,md)
      
      
      if (sum(rowSums(R1.1)) != 22) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 22) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'CEPAL2010'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'CEPAL2010'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
  
  #For CEPAL 2004
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- PANAMA_04[PANAMA_04$Code==j & PANAMA_04$Sex==k,]
    new.CoD  <- PANAMA20042[PANAMA20042$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      if (sum(rowSums(R1.1)) != 18) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 18) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'CEPAL2004'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'CEPAL2004'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }

### Jamaica (1990,1991(1990-1995) ,2000-2006 (2000-2005), 2009-2011 (2005-2010)) Just LT from UN
### 2290

  
  JAMAICAD <-   Rest[(Rest$X == 2290),]
  ### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)
  
  
  JAMAICA_UN <-   Data.LT[Data.LT$Code == 2290 & Data.LT$Source == 'UN',]
  
    unique(JAMAICA_UN$Period)
  
  JAMAICA_UN <-   JAMAICA_UN[JAMAICA_UN$Period != "1995-2000" ,]
  JAMAICA_UN <-   JAMAICA_UN[JAMAICA_UN$Period != "2010-2015" ,]
  
  ## Aggregate deaths in periods  to get more robust CoD decomp
  unique(JAMAICAD$Year)
  
  Periods      <- c(1990,2000,2006)
  Period.labels <- unique(JAMAICA_UN$Period)
  
  JAMAICAD$Period5       <- (cut(JAMAICAD$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
  
  JAMAICAD2 <- JAMAICAD[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
  JAMAICAD2$Male <- JAMAICAD[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
  unique(JAMAICAD2$Period5)
  
  ### Ready to decompose for UN
  
  ### Ok, we can perform decomp for CEPAL 2004
  JAMAICA_UN$Sex <- as.numeric(JAMAICA_UN$Sex)
  Sex       <- unique(JAMAICA_UN$Sex)
  Countries <- unique(JAMAICA_UN$X)
  JAMAICAD2$Period5 <- as.character(JAMAICAD2$Period5)
  #k <- Sex[1]
  #l <-1
  #
  ## Because everything is different and we need separate files for each country, I'll do this with loops
  j <-2290
  print(j)
  # For UN
  for (k in Sex){
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- JAMAICA_UN[JAMAICA_UN$Code==j & JAMAICA_UN$Sex==k,]
    new.CoD  <- JAMAICAD2[JAMAICAD2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    for (l in 1:length(Periods[-length(P2)])){
      print(l)
      # get vectors of mx
      p1.data    <- new.data[new.data$Period == Periods[l],]
      p2.data    <- new.data[new.data$Period == Periods[l+1],]
      
      # get proportions of causes of death for these periods in matrix format
      CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
      CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
      R1 <- acast(CoD.1, Age2 ~ Cause, value.var = colnames(CoD.1)[sex.col],fill = 0,drop = F)
      R2 <- acast(CoD.2, Age2 ~ Cause, value.var = colnames(CoD.2)[sex.col],fill = 0,drop = F)
      R1.1 <- R1/rowSums(R1)
      R2.1 <- R2/rowSums(R2)
      
      if (sum(rowSums(R1.1)) != 19) print('Dimension R1')
      if (sum(rowSums(R2.1)) != 19) print('Dimension R2')
      
      # calculate age decomposition
      mx1 <- p1.data$mx
      mx2 <- p2.data$mx
      Decomp.age <- AgeDecomp(mx1=mx1,mx2=mx2,Age=p1.data$Age,Sex=k)
      print(sum(Decomp.age)-(e0.from.mx(mx2,p1.data$Age,k)-e0.from.mx(mx1,p1.data$Age,k)))
      # calculate cause-specific contributions
      Decomp.CoD <- Decomp.age*((R2.1*mx2)-(R1.1*mx1))/(mx2-mx1)
      Decomp.CoD[is.infinite(Decomp.CoD)] <- 0
      Decomp.CoD[is.na(Decomp.CoD)] <- 0
      print(sum(Decomp.CoD)-sum(Decomp.age))
      # Dataframe with decomposition results
      Results         <- melt(Decomp.CoD,varnames = c('Age','Cause'),value.name = 'Contribution')
      Results$Period1 <- Periods[l]
      Results$Period2 <- Periods[l+1]
      Results$Age     <- rep(p1.data$Age,14)
      Results$Sex     <- k
      Results$Country <- j
      Results$Sources <- 'UN'
      Results$e01     <- e0.from.mx(mx = mx1,Ages=p1.data$Age,Sex=k)
      Results$e02     <- e0.from.mx(mx = mx2,Ages=p1.data$Age,Sex=k)
      Decomp.results  <- rbind(Decomp.results,Results)
      
    }
    
    for (l2 in (1:length(Periods))){
      print(l2)
      # get vectors of mx
      p11.data    <- new.data[new.data$Period == Periods[l2],]
      mx1 <- p11.data$mx
      # Dataframe with lifetables following the same procedure (Preston et al 2001)
      Lifetable1        <- LT.from.mx(mx = mx1,Ages=p11.data$Age,Sex=k)
      Lifetable1$Period <- Periods[l2]
      Lifetable1$Sex    <- k
      Lifetable1$Country<- j
      Lifetable1$Source <- 'UN'
      
      Lifetables  <- rbind(Lifetables,Lifetable1)
      
    }
    
  }
  
## Now code mortality from age 80 as 'Rest' since it is unreliable
  Decomp.results <- data.table(Decomp.results)
  Decomp.results[Decomp.results$Age >= 80]$Cause <- 15 
  Decomp.results <- Decomp.results[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Period1,Period2,Sex,Country,Sources,e01,e02)]
  
  
unique(Decomp.results$Country)

cause.name.vec      <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                    'Nervous','Endocrine','Digestive',
                    'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')
Decomp.results$Cause.name <- Decomp.results$Cause
Decomp.results$Cause.name <- factor(Decomp.results$Cause.name, levels = 1:15, labels = cause.name.vec)

Country.name.vec <- toupper(c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                              'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                              'Honduras','Mexico','Nicaragua','Panama','Argentina',
                              'Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela',
                              'Haiti','Bolivia','Brazil', 'Latin America'))

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                      2340,2350,2020,2120,2130,2180,2360,2370,2460,2470,2270,2060,2070,9999)
names(Country.code.vec) <- Country.name.vec

Decomp.results$Country.name <- Decomp.results$Country
Decomp.results$Country.name <- factor(Decomp.results$Country.name, levels = Country.code.vec, 
                                      labels = Country.name.vec)
unique(Decomp.results$Country.name)
Decomp_results <- Decomp.results

unique(Decomp_results$Period1)
unique(Decomp_results$Period2)

Decomp_results[Decomp_results$Period1 == '1990-1995']$Period1 <- '1990-1994'
Decomp_results[Decomp_results$Period1 == '1995-2000']$Period1 <- '1995-1999'
Decomp_results[Decomp_results$Period1 == '2000-2005']$Period1 <- '2000-2004'
Decomp_results[Decomp_results$Period1 == '2005-2010']$Period1 <- '2005-2009'

Decomp_results[Decomp_results$Period2 == '1995-2000']$Period2 <- '1995-1999'
Decomp_results[Decomp_results$Period2 == '2000-2005']$Period2 <- '2000-2004'
Decomp_results[Decomp_results$Period2 == '2005-2010']$Period2 <- '2005-2009'
Decomp_results[Decomp_results$Period2 == '2010-2015']$Period2 <- '2010-2014'


Decomp_results[Decomp_results$Country.name=='PARAGUAY',]




save(Decomp_results, file = 'Outcomes/Decomp_results.RData')
save(Decomp_results, file = 'R/Decomp_App/Decomp_results.RData')
save(Lifetables, file = 'Outcomes/LatinAmerica_Lifetables.RData')

