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
# 
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
library(data.table)

setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")

Mex_ICD9      <-    read.table("Data/ICD9/ICD9-2310.txt",header=T,sep = ",",stringsAsFactors = F)
Mex_ICD10     <-    read.table("Data/ICD10/ICD10-2310.txt",header=T,sep = ",",stringsAsFactors = F)
Mex           <-    rbind(Mex_ICD9,Mex_ICD10)
Mex           <- data.table(Mex)

# Add a new numeric variable to handle ages
code.age         <- unique(Mex$Age)
code.age2        <- c(97,0:4,seq(5,95,5),96)
names(code.age2) <- code.age
Mex$Age2         <- code.age2[as.character(Mex$Age)]

#Order data
Mex   <- Mex[with(Mex,order(Year,Age2,Cause)),]

# Check for consistency between total Age == 97 and Cause == 1
Totals          <- Mex[Mex$Age2 == 97 & Mex$Cause ==  1,]
Mex.noT         <- Mex[Mex$Age2 < 97 & Mex$Cause!=1, ]
Total.sum       <- Mex.noT[,list(Female = sum(Female)), by = list(Year)]
Total.sum$Male  <- Mex.noT[,list(Male = sum(Male)), by = list(Year)]$Male
Totals$Female - Total.sum$Female
Totals$Male   - Total.sum$Male

# There are big differences, the sum of everything does not account for the totals in the dataset.
# I will create a new cause of death by age, year and country that will account for this difference. 
# Also, what is UNK, O am treating it as Not Specified from the root.

#An easy function to find the differences, 
incon <- function(Deaths){
  Total <- Deaths[1]
  Rest  <- sum(Deaths[2:length(Deaths)])
  Dif   <- Total - Rest
  Dif 
}

# Calculate differences for females  and females
New.cat           <- Mex[,list(Female=incon(Female)), by = list(Year,Age2)]
New.cat$Male      <- Mex[,list(Male=incon(Male)), by = list(Year,Age2)]$Male
# Name the casue as 15
New.cat$Cause     <- 15
# Set the data.table in the shame shape as Mex
New.cat$X         <- 2310
code.age3         <- unique(New.cat$Age2)
code.age4         <- code.age
names(code.age4)  <- code.age3
New.cat$Age       <- code.age4[as.character(New.cat$Age2)]
New.cat           <- New.cat[,c('X', 'Year', 'Cause', 'Age', 'Female', 'Male', 'Age2')]

# Rbind the old dataset with the new category
Mex               <- rbind(Mex,New.cat)
Mex               <- Mex[with(Mex,order(Year,Age2,Cause)),]

#Check results on consistency
Totals          <- Mex[Mex$Age2 == 97 & Mex$Cause ==  1,]
Mex.noT         <- Mex[Mex$Age2 < 97 & Mex$Cause!=1, ]
Total.sum       <- Mex.noT[,list(Female = sum(Female)), by = list(Year)]
Total.sum$Male  <- Mex.noT[,list(Male = sum(Male)), by = list(Year)]$Male
Totals$Female - Total.sum$Female
Totals$Male   - Total.sum$Male


# Next step, do it for all the countries