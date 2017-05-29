######################################################
###Program to check consistency with data from Mexico#
######################################################

# Por favor haz lo mismo para MÃ©xico (paÃ�s cÃ³digo 2310) y me dices si encuentras algÃºn problema 
#con los datos. Es decir compÃ¡ralos con los datos que tÃº tienes de INEGI (o de la secretaria de salud?).
# Al abrir el folder de WHOhmd de ICD9 y ICD10 te encontraras para cada paÃ�s un documento que al abrirlo 
#tiene las muertes por: "Year","Cause","Age","Female","Male"
# Yearâ¦ 1990â¦2015
# Causes: 1..14, correspondiente a los valores que se dan aquÃ� abajo.
# Age: 0,1,2,3,4,5-9,â¦90-94,95+, UNK
# Female & Male: te dan las muertes correspondientes por cada sexo
# 
# Los formatos de las edades estÃ¡n explicados en el documento Word âDocumentation_15sep2016â en la pagina 6.  
setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")


Mex           <-    read.table("Data/ICD10/ICD10-2310.txt",header=T,sep = ",",stringsAsFactors = F)
# Add a new variable age just with integers, 96 = Total, 97 = UNK
code.age         <- unique(Mex$Age)
code.age2        <- c(96,0:4,seq(5,95,5),97)
names(code.age2) <- code.age
Mex$Age2         <- code.age2[as.character(Mex$Age)]
Mex              <- Mex[Mex$Age2==96 & Mex$Cause != 1,]

# find the years I want to compare
# just from 1990 to 1997 for ICD 9
range.year <- range(Mex$Year)
range.year


#### now check consistency with causes of death, just for years after 1997.

library(reshape)
library(gdata)
library(latticeExtra)

initial_year       <- range.year[1]
Nspecified_year    <- 9999
Deaths.NoSpecified <- NULL
# Define an object to store the deaths
Deaths1<-NULL

for(i in range.year[1]:range.year[2]){  
  j<-paste("C:/Users/jmaburto/Documents/Process Mex Mortality data/Original INEGI data 90-2015/DEF",i,".RData",sep="")
  load(j)  
  DEF<-get(paste("DEF",i,sep=""))    
  
  if (i==1998){DEF<- subset(DEF,select=c(ENT_RES,CAUSA,SEXO,EDAD_L,EDAD_N,ANO_D))
  DEF$ANO_D<-as.integer(as.character(DEF$ANO_D))}
  if (i==1999){DEF<- subset(DEF,select=c(ent_res,causa,sexo,edad_l,edad_n,ano_d))
  DEF<-rename.vars(DEF,c("ent_res","causa","sexo","edad_l","edad_n","ano_d"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2000 & i<=2001){DEF<- subset(DEF,select=c(entres,causa,sexo,cveedad,edad,anodef))
  DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","anodef"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2002 & i<=2003){DEF<- subset(DEF,select=c(ENTRES,CAUSA,SEXO,CVEEDAD,EDAD,ANODEF))
  DEF<-rename.vars(DEF,c("ENTRES","CAUSA","SEXO","CVEEDAD","EDAD","ANODEF"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)
  DEF$ANO_D<-as.integer(as.character(DEF$ANO_D))}
  if (i==2004){DEF<- subset(DEF,select=c(entres,causab,causad,sexo,cveedad,edad,anodef))
  DEF$causa<-paste(DEF$causab,DEF$causad,sep="")
  DEF<-subset(DEF,select=c(entres,causa,sexo,cveedad,edad,anodef))
  DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","anodef"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i==2005){DEF<- subset(DEF,select=c(entres,causab,causad,sexo,cveedad,edad,aniodef))
  DEF$causa<-paste(DEF$causab,DEF$causad,sep="")
  DEF<-subset(DEF,select=c(entres,causa,sexo,cveedad,edad,aniodef))
  DEF<-rename.vars(DEF,c("entres","causa","sexo","cveedad","edad","aniodef"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i==2006){DEF$edad_l<-0
  DEF<- subset(DEF,select=c(ent_resid,causa_def,sexo,edad_l,edad,anio_ocur))                              
  DEF<-rename.vars(DEF,c("ent_resid","causa_def","sexo","edad_l","edad","anio_ocur"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2007 & i<=2008){DEF<- subset(DEF,select=c(ENTRH,CAUSADEF,DESDOBLA,SEXO,EDADUNI,EDADVALOR,ANIODEF))
  DEF$causa<-paste0(DEF$CAUSADEF,DEF$DESDOBLA)
  DEF<-subset(DEF,select=c(ENTRH,causa,SEXO,EDADUNI,EDADVALOR,ANIODEF))
  DEF<-rename.vars(DEF,c("ENTRH","causa","SEXO","EDADUNI","EDADVALOR","ANIODEF"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)
  
  DEF$SEXO<-as.numeric(as.character(DEF$SEXO))
  DEF$ANO_D<-as.numeric(as.character(DEF$ANO_D))}
  if (i>=2009 & i<=2011){DEF$edad_l<-0
  DEF<- subset(DEF,select=c(ent_resid,causa_def,sexo,edad_l,edad,anio_ocur))                              
  DEF<-rename.vars(DEF,c("ent_resid","causa_def","sexo","edad_l","edad","anio_ocur"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  if (i>=2012 & i<=2015){DEF$edad_l<-0
  DEF<- subset(DEF,select=c(ENT_RESID,CAUSA_DEF,SEXO,edad_l,EDAD,ANIO_OCUR))                              
  DEF<-rename.vars(DEF,c("ENT_RESID","CAUSA_DEF","SEXO","edad_l","EDAD","ANIO_OCUR"),
                   c("ENT_RES","CAUSA","SEXO","EDAD_L","EDAD_N","ANO_D"),info=TRUE)}
  
  
  if (is.factor(DEF$ENT_RES)){
    DEF$ENT_RES<-as.integer(as.character(DEF$ENT_RES)) 
  }
  
  if (is.factor(DEF$EDAD_N)){
    DEF$EDAD_N<-as.integer(as.character(DEF$EDAD_N)) 
  }
  
  if (is.factor(DEF$SEXO)){
    DEF$SEXO<-as.integer(as.character(DEF$SEXO)) 
  }
  
  
  DEF<-subset(DEF,DEF$ANO_D>=initial_year)
  DEF<-subset(DEF,DEF$ANO_D<Nspecified_year)
  k <- dim(DEF)[1]
  DEF<-subset(DEF,DEF$ENT_RES<33)    
  DEF<-subset(DEF,DEF$SEXO<3)
  
  DEF<-subset(DEF,DEF$SEXO>=1)
  
  if(i<2006){
    DEF$AGE<-DEF$EDAD_N
    DEF$EDAD_L=as.character(DEF$EDAD_L)
    DEF$AGE[DEF$EDAD_L=="D"] <- 0
    DEF$AGE[DEF$EDAD_L=="H"] <- 0
    DEF$AGE[DEF$EDAD_L=="M"] <- 0  
    DEF$AGE<-as.integer(DEF$AGE)
    DEF<-subset(DEF,DEF$AGE<121)
    
  }
  
  if(i==2006){
    DEF$AGE<-DEF$EDAD_N-4000
    DEF$AGE[DEF$AGE<1] <- 0
    DEF$AGE<-as.integer(DEF$AGE)
    DEF<-subset(DEF,DEF$AGE<121)
  }
  
  
  if(i>2006 & i<2009){
    DEF$AGE<-DEF$EDAD_N
    DEF$EDAD_L=as.character(DEF$EDAD_L)
    DEF$AGE[DEF$EDAD_L=="D"] <- 0
    DEF$AGE[DEF$EDAD_L=="H"] <- 0
    DEF$AGE[DEF$EDAD_L=="M"] <- 0  
    DEF$AGE<-as.integer(DEF$AGE) 
    DEF<-subset(DEF,DEF$AGE<121)
    
  }
  
  if(i>=2009){
    DEF$AGE<-DEF$EDAD_N-4000
    DEF$AGE[DEF$AGE<1] <- 0
    DEF$AGE<-as.integer(DEF$AGE)
    DEF<-subset(DEF,DEF$AGE<121)
  }
  
  DEF$CAUSA<-as.character(DEF$CAUSA)                  
  DEF$CV1<-substr(DEF$CAUSA,2,3)
  DEF$CV2<-substr(DEF$CAUSA,1,1)
  DEF$CV3<-substr(DEF$CAUSA,4,4)
  DEF$CV1<-as.integer(DEF$CV1)
  Deaths1<-rbind(Deaths1,DEF)  
  k1 <- k-dim(DEF)[1]
  k2 <- cbind(i,k1)
  Deaths.NoSpecified <- rbind(Deaths.NoSpecified,k2)
  print(i)
}

#Check consistency of the data
colSums(is.na(Deaths1))

#save files
save(Deaths1,Deaths.NoSpecified,Mex, file = "Data/CoD_Consistency data.Rdata")

#keep files we are interested in
gdata::keep(Deaths1,Deaths.NoSpecified,Mex,sure=T)

load("Data/CoD_Consistency data.RData")



unique(Deaths1$CAUSA)
Deaths1$CAUSA<-as.character(Deaths1$CAUSA)                  
Deaths1$CV1<-substr(Deaths1$CAUSA,2,3)
Deaths1$CV2<-substr(Deaths1$CAUSA,1,1)
Deaths1$CV3<-substr(Deaths1$CAUSA,4,4)
Deaths1$CV1<-as.integer(Deaths1$CV1)  

colSums(is.na(Deaths1))


### start grouping
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

## Classify according to WHO

# Those causes not classified
Deaths1$AM<-5

attach(Deaths1)
# 2 Certain infectious and parasitic diseases A00-B99
Deaths1$AM[Deaths1$CV2=="A" & Deaths1$CV1>=0] <- 2
Deaths1$AM[Deaths1$CV2=="B" & Deaths1$CV1>=0 & Deaths1$CV1<=99] <- 2
# 3 Neoplasms   C00-D48
Deaths1$AM[Deaths1$CV2=="C" & Deaths1$CV1>= 0] <- 3
Deaths1$AM[Deaths1$CV2=="D" & Deaths1$CV1<= 48] <- 3
# 4 Diseases of the circulatory system                        I00-I99
Deaths1$AM[Deaths1$CV2=="I" & Deaths1$CV1<=99] <- 4
# 6 Mental and behavioural disorders F01-F99
Deaths1$AM[Deaths1$CV2=="F" & Deaths1$CV1>=1 & Deaths1$CV1<=99] <- 6
# 7 Diseases of the nervous system            G00-G98
Deaths1$AM[Deaths1$CV2=="G" & Deaths1$CV1>=0 & Deaths1$CV1<=98] <- 7
# 8 Endocrine, nutritional and metabolic diseases                 E00-E88 
Deaths1$AM[Deaths1$CV2=="E" & Deaths1$CV1>=0 & Deaths1$CV1<=98] <- 8
# 9 Diseases of the digestive system K00-K92
Deaths1$AM[Deaths1$CV2=="K" & Deaths1$CV1>=0 & Deaths1$CV1<=92] <- 9
# 10 Diseases of the genitourinary system               N00-N98
Deaths1$AM[Deaths1$CV2=="N" & Deaths1$CV1>=0 & Deaths1$CV1<=98] <- 10
# 11 P00-P96          Certain conditions originating in the perinatal period 
# 11 Q00-Q99                Congenital malformations, deformations and chromosomal abnormalities
Deaths1$AM[Deaths1$CV2=="P" & Deaths1$CV1>=0 & Deaths1$CV1<=96] <- 11
Deaths1$AM[Deaths1$CV2=="Q" & Deaths1$CV1>=0 & Deaths1$CV1<=99] <- 11
# 12 Diseases of the respiratory system    J00-J98
Deaths1$AM[Deaths1$CV2=="J" & Deaths1$CV1>=0 & Deaths1$CV1<=98] <- 12
# 13 External causes of morbidity and mortality     V01-Y89 minus homicide
Deaths1$AM[Deaths1$CV2=="V" & Deaths1$CV1>=1] <- 13
Deaths1$AM[Deaths1$CV2=="W" & Deaths1$CV1>=0] <- 13
Deaths1$AM[Deaths1$CV2=="X" & Deaths1$CV1<89] <- 13
Deaths1$AM[Deaths1$CV2=="Y" & Deaths1$CV1>9] <- 13
# 14 X85-Y09 Assault - homicide
Deaths1$AM[Deaths1$CV2=="X" & Deaths1$CV1>=89] <- 14
Deaths1$AM[Deaths1$CV2=="Y" & Deaths1$CV1<=9] <- 14
detach(Deaths1)

#### Now aggregate data by sex, year, cause of death
Deaths1$Deaths <- 1

library(data.table)
Deaths1 <- data.table(Deaths1)
Mex     <- data.table(Mex)
CoD.Mex <- Deaths1[,sum(Deaths),by = list(ANO_D,AM,SEXO)]
CoD.Mex <- CoD.Mex[with(CoD.Mex,order(SEXO,ANO_D,AM)),]
Mex     <- Mex[with(Mex,order(Year,Cause)),]
Mex$INEGI.f <- CoD.Mex[CoD.Mex$SEXO == 2]$V1 
Mex$INEGI.m   <- CoD.Mex[CoD.Mex$SEXO == 1]$V1 
Mex$diff.m <- Mex$Male - Mex$INEGI.m
Mex$diff.f <- Mex$Female - Mex$INEGI.f

write.csv(Mex,file="Outcomes/Consistency_Mex_CoD.csv")
