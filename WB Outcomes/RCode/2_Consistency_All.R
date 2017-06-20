###############################################################################
### Program to create a new category to make Total and sum of casues consisten#
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
# 15 rest of causes

library(data.table)
library(reshape2)
setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')


source('R/1_Get_All_Data.R')

# Do not account for cause 15, 
Deaths.data <- Deaths.data[Deaths.data$Cause != 15,]

### Now we want to check consistency between the sum of all causes and the total category

## first order data
Deaths.data   <- Deaths.data[with(Deaths.data,order(X,Year,Age2,Cause)),]

# Check for consistency between total Age == 97 and Cause == 1
# Store total from originals and order all the same way
Big.Totals          <- Deaths.data[Deaths.data$Age2 == 97 & Deaths.data$Cause ==  1, ]
Big.Totals          <- Big.Totals[with(Big.Totals,order(X,Year)),]

Age.Totals          <- Deaths.data[Deaths.data$Age2 < 97 & Deaths.data$Cause ==  1, ]
Age.Totals          <- Age.Totals[with(Age.Totals,order(X,Year,Age2)),] 


# Store all without totals
Deaths             <- Deaths.data[Deaths.data$Age2 < 97 & Deaths.data$Cause != 1, ]
Deaths             <- Deaths[with(Deaths,order(X,Year,Age2,Cause)),]

# Now go back, get the totals by age
Age.Totals2        <- Deaths[,list(Female= sum(Female)), by = list(X,Year,Age2)]
Age.Totals2$Male   <- Deaths[,list(Male= sum(Male)), by = list(X,Year,Age2)]$Male  
Age.Totals2        <- Age.Totals2[with(Age.Totals2,order(X,Year,Age2)),] 

# See differences from the root files

# Now get the difference of original versus new ones by age and store them in a new category 15
# and then add them to the Deaths object
New.cat.age       <- Age.Totals2[,1:3,with=F]
New.cat.age$Cause <- 15
New.cat.age$Female<- Age.Totals$Female - Age.Totals2$Female
New.cat.age$Male  <- Age.Totals$Male - Age.Totals2$Male

New.cat.age[New.cat.age$Male < 0 | New.cat.age$Female < 0 ,]

# 22 cases where the sum of 2:14 is larger than the total from the original one by age Cause == 1



############################# An alternative way to look for inconsistencies and be sure.
## Data wihout Age 97
Deaths.2             <- Deaths.data[Deaths.data$Age2 < 97, ]

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
Inconsist           <- Deaths.2[,list(Female=incon(Female)), by = list(X,Year,Age2)]
Inconsist$Male      <- Deaths.2[,list(Male=incon(Male)), by = list(X,Year,Age2)]$Male

Inconsist[Inconsist$Male < 0 | Inconsist$Female < 0 ,]

# excelent, consistent data

### now return the Deaths.data with the new category
