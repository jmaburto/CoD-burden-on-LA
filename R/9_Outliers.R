#### Analyze some outliers
setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')
library(reshape2)
library(data.table)


# Load outcomes from data harmonization and proportions by age of causes of death
# 1 is males
source('R/5_Proportions_Age.R')
load('Outcomes/Data_Lifetables.RData')

Country.code.vec


# Classification of causes of death
# 1 total deaths
# 2 Certain infectious and parasitic diseases     A00-B99
# 3 Neoplasms                                     C00-D48
# 4 Diseases of the circulatory system            I00-I99
# 5 Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified                                                                             R00-R99
# 6 Mental and behavioural disorders F01-F99
# 7 Diseases of the nervous system            G00-G98
# 8 Endocrine, nutritional and metabolic diseases     E00-E88 
# 9 Diseases of the digestive system K00-K92  
# 10 Diseases of the genitourinary system               N00-N98
# 11 P00-P96          Certain conditions originating in the perinatal period & Q00-Q99    Congenital malformations, deformations and chromosomal abnormalities
# 12 Diseases of the respiratory system    J00-J98
# 13 External causes of morbidity and mortality     V01-Y89 minus homicide
# 14 X85-Y09 Assault - homicide
# 15 rest of causes



# JAMAICA -----------------------------------------------------------------


# child mortality, perinatal causes


acast(Deaths.data[Deaths.data$Country=='Jamaica' & Deaths.data$Age2 == 0,],Cause~Year,value.var = 'Female')


### Jamaica (1990,1991(1990-1995) ,2000-2006 (2000-2005), 2009-2011 (2005-2010)) Just LT from UN
### 2290


JAMAICAD <-   Rest[(Rest$X == 2290),]

acast(JAMAICAD[JAMAICAD$Age2==0],Cause~Year,value.var = "Female")


### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)


JAMAICA_UN <-   Data.LT[Data.LT$Code == 2290 & Data.LT$Source == 'UN',]
acast(JAMAICA_UN[JAMAICA_UN$Age==85 & JAMAICA_UN$Sex=="2",],Age~Year,value.var = "mx")

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
k <-1
#l <-1
#
## Because everything is different and we need separate files for each country, I'll do this with loops
j <-2290
print(j)
# For UN

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
    #CoD.1[CoD.1$Age==0,] 
    CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
    #CoD.2[CoD.2$Age==0,]
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
  

      
      
  
# domimican republic age 0 ------------------------------------------------------

  
  Country.code.vec
  
  acast(Deaths.data[Deaths.data$Country=='Dominican Republic' & Deaths.data$Age2 == 0,],Cause~Year,value.var = 'Female')
  
  
  
  
  DR <-   Rest[(Rest$X == 2170),]
  
  acast(DR[DR$Age2==0],Cause~Year,value.var = "Female")
  
  
  ### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)
  
  
  DR_UN <-   Data.LT[Data.LT$Code == 2170 & Data.LT$Source == 'CEPAL2004',]
  acast(DR_UN[DR_UN$Age==80 & DR_UN$Sex=="2",],Age~Year,value.var = "mx")
  
  unique(DR_UN$Period)
  
  DR_UN[DR_UN$Period == "1990-1995" ,]
  DR_UN[DR_UN$Period == "1995-2000" ,]
  
  ## Aggregate deaths in periods  to get more robust CoD decomp
  unique(DR$Year)
  
  Periods      <- seq(1990,2010,5)
  Period.labels <- unique(DR_UN$Period)
  DR$Period5       <- (cut(DR$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
  
  
  DR2 <- DR[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
  DR2$Male <- DR[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
  unique(DR2$Period5)
  
  ### Ready to decompose for UN
  
  ### Ok, we can perform decomp for CEPAL 2004
  DR_UN$Sex <- as.numeric(DR_UN$Sex)
  Sex       <- unique(DR_UN$Sex)
  Countries <- unique(DR_UN$X)
  DR2$Period5 <- as.character(DR2$Period5)
  #k <- Sex[1]
  #l <-1
  #
  ## Because everything is different and we need separate files for each country, I'll do this with loops
  j <-2170
  print(j)
  # For UN
  
  if (k == 1) sex.col <- 10
  if (k == 2) sex.col <- 9
  # subset data with info requireMethods
  new.data <- DR_UN[DR_UN$Code==j & DR_UN$Sex==k,]
  new.CoD  <- DR2[DR2$X==j,]
  
  # find out for which periods we have information
  P1      <- unique(new.data$Period)
  P2      <- unique(new.CoD$Period5)
  Periods <- P1
  if ((length(P2) - length(P1))!= 0) print('Problem')
  
 
    print(l)
    # get vectors of mx
    p1.data    <- new.data[new.data$Period == Periods[l],]
    p2.data    <- new.data[new.data$Period == Periods[l+1],]
    
    # get proportions of causes of death for these periods in matrix format
    CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
    #CoD.1[CoD.1$Age==0,] 
    CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
    #CoD.2[CoD.2$Age==0,]
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
    
    
    
  
    
# Dominican Republic age 85+ ----------------------------------------------

    Country.code.vec
    
    acast(Deaths.data[Deaths.data$Country=='Dominican Republic' & Deaths.data$Age2 == 0,],Cause~Year,value.var = 'Female')
    
    
    
    
    DR <-   Rest[(Rest$X == 2170),]
    
    acast(DR[DR$Age2==0],Cause~Year,value.var = "Male")
    
    
    ### just take LT representative of 1990 (1990-1995), and 2010 (2005-2010)
    
    
    DR_UN <-   Data.LT[Data.LT$Code == 2170 & Data.LT$Source == 'UN',]
    acast(DR_UN[DR_UN$Age==85 & DR_UN$Sex=="2",],Age~Year,value.var = "mx")
    
    unique(DR_UN$Period)
    
    DR_UN[DR_UN$Period == "1990-1995" ,]
    DR_UN[DR_UN$Period == "1995-2000" ,]
    
    ## Aggregate deaths in periods  to get more robust CoD decomp
    unique(DR$Year)
    
    Periods      <- seq(1990,2010,5)
    Period.labels <- unique(DR_UN$Period)
    DR$Period5       <- (cut(DR$Year+1, breaks=c(Periods,Inf),labels=Period.labels))
    
    
    DR2 <- DR[,list(Female=sum(Female)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]
    DR2$Male <- DR[,list(Male=sum(Male)), by = list(X,Cause,Age,Country,Age2,Cause.name,Source,Period5)]$Male
    unique(DR2$Period5)
    
    ### Ready to decompose for UN
    
    ### Ok, we can perform decomp for CEPAL 2004
    DR_UN$Sex <- as.numeric(DR_UN$Sex)
    Sex       <- unique(DR_UN$Sex)
    Countries <- unique(DR_UN$X)
    DR2$Period5 <- as.character(DR2$Period5)
    #k <- Sex[2]
    #l <-3
    #
    ## Because everything is different and we need separate files for each country, I'll do this with loops
    j <-2170
    print(j)
    # For UN
    
    if (k == 1) sex.col <- 10
    if (k == 2) sex.col <- 9
    # subset data with info requireMethods
    new.data <- DR_UN[DR_UN$Code==j & DR_UN$Sex==k,]
    new.CoD  <- DR2[DR2$X==j,]
    
    # find out for which periods we have information
    P1      <- unique(new.data$Period)
    P2      <- unique(new.CoD$Period5)
    Periods <- P1
    if ((length(P2) - length(P1))!= 0) print('Problem')
    
    
    print(l)
    # get vectors of mx
    p1.data    <- new.data[new.data$Period == Periods[l],]
    p2.data    <- new.data[new.data$Period == Periods[l+1],]
    
    # get proportions of causes of death for these periods in matrix format
    CoD.1    <- new.CoD[new.CoD$Period5 == Periods[l],]
    #CoD.1[CoD.1$Age==0,] 
    CoD.2    <- new.CoD[new.CoD$Period5 == Periods[l+1],]
    #CoD.2[CoD.2$Age==0,]
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