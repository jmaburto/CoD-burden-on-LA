######################################################
###Program to check consistency with data from Mexico#
######################################################
 
# Por favor haz lo mismo para México (país código 2310) y me dices si encuentras algún problema 
#con los datos. Es decir compáralos con los datos que tú tienes de INEGI (o de la secretaria de salud?).
# Al abrir el folder de WHOhmd de ICD9 y ICD10 te encontraras para cada país un documento que al abrirlo 
#tiene las muertes por: "Year","Cause","Age","Female","Male"
# Year… 1990…2015
# Causes: 1..14, correspondiente a los valores que se dan aquí abajo.
# Age: 0,1,2,3,4,5-9,…90-94,95+, UNK
# Female & Male: te dan las muertes correspondientes por cada sexo
# 
# Los formatos de las edades están explicados en el documento Word “Documentation_15sep2016” en la pagina 6.  
# 


setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")

Mex_ICD9      <-    read.table("Data/ICD9/ICD9-2310.txt",header=T,sep = ",",stringsAsFactors = F)
Mex_ICD10     <-    read.table("Data/ICD10/ICD10-2310.txt",header=T,sep = ",",stringsAsFactors = F)
Mex           <-    rbind(Mex_ICD9,Mex_ICD10)
unique(Mex$Cause)
# Add a new variable age just with integers, 97 = Total, 96 = UNK
code.age         <- unique(Mex_ICD9$Age)
code.age2        <- c(97,0:4,seq(5,95,5),96)
names(code.age2) <- code.age
Mex$Age2         <- code.age2[as.character(Mex$Age)]
Mex              <- Mex[Mex$Age2==97 & Mex$Cause == 1,]


# find the years I want to compare
# just from 1990 to 1997 for ICD 9
range.year <- range(Mex$Year)
range.year

#Load population estimates from INEGI original data and CONAPO estimates, best data though is the new conciliation. 
# This contains INEGI rowdata
load("Data/INEGI_Counts.RData")
# This contains CONAPO estimates
load("Data/CONAPO_estimates.rdata")

nat <- unique(defspry$ent)[24]
# subset just those years that we are interested in
Conapo     <- defspry[defspry$ent == nat   & defspry$a�o >= range.year[1] &
                    defspry$a�o <= range.year[2],]

# aggregate deaths by age and sex to get the national with INEGI data
library(data.table)
tot.deaths <- Data_Counts[,list(National = sum(total)), by = list(sex,age,year)]
INEGI      <- tot.deaths[tot.deaths$year >= range.year[1] & tot.deaths$year <= range.year[2],]


# keep those we are interested in
gdata::keep(Conapo, INEGI,Mex, sure=T)
Conapo <- data.table(Conapo)
INEGI  <- data.table(INEGI)
Mex    <- data.table(Mex)

# Now check consistency in every year
Conapo.total    <- Conapo[,list(Total=sum(defs)), by = list(a�o,sexo)]
INEGI.total     <- INEGI[,list(Total=sum(National)), by = list(year,sex)]

Conapo.total[Conapo.total$sexo=="Mujeres",]$Total

write.csv(Mex,file="Outcomes/WHO_total_Mex.csv")


Consistency.data <- cbind(Mex[,c(2,5,6),with=F],
                          Conapo.f=Conapo.total[Conapo.total$sexo=="Mujeres",]$Total,
                          Conapo.m=Conapo.total[Conapo.total$sexo=="Hombres",]$Total,
                          INEGI.f = INEGI.total[INEGI.total$sex==2,]$Total,
                          INEGI.M = INEGI.total[INEGI.total$sex==1,]$Total)

gdata::keep(Consistency.data, sure = T)

rel.dif.WHO.CONAPO <- 100*cbind(Year= c(1990:2014)/100,females = abs(Consistency.data$Female - Consistency.data$Conapo.f)/Consistency.data$Female, 
                            males   = abs(Consistency.data$Male - Consistency.data$Conapo.m)/Consistency.data$Male)
rel.dif.WHO.INEGI  <- 100*cbind(Year= c(1990:2014)/100,females = abs(Consistency.data$Female - Consistency.data$INEGI.f)/Consistency.data$Female, 
                                males   = abs(Consistency.data$Male - Consistency.data$INEGI.M)/Consistency.data$Male)

rel.dif.WHO.CONAPO

rel.dif.WHO.INEGI

