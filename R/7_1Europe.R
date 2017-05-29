
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

Country.code.vec <- c(4010,4020,4050,4070,4080,4085,4170,4180,4190,4210,4240,4280,4290,4308)

Country.name.vec <- toupper(c('Austria','Belgium','Denmark','Finland','France','Germany','Ireland','Italy',
          'Luxembourg','Netherlands','Portugal','Spain','Sweden','UK'))

names(Country.code.vec) <- Country.name.vec


## Para ICD 10 solo tenemos datos para 4010,4020,4070,4080,4085
## Para ICD 9 solo tenemos datos para 4010,4020,4070,4080,4085,4140,4170,4180,4190,4210,4240,4280,4290,4308
## We are not analizing Greence


Deaths.data  <- NULL

#i<- 8
# Create a loop to find the year of change between ICD9 and ICD10 and create homogeneous datasets
for (i in 1:length(Country.code.vec)){
  
  
  if(Country.code.vec[i] == '4010' | Country.code.vec[i] == '4020' |Country.code.vec[i] == '4070' | Country.code.vec[i] == '4140' |
     Country.code.vec[i] == '4080' | Country.code.vec[i] == '4085' |Country.code.vec[i] == '4170' |Country.code.vec[i] == '4180' |
     Country.code.vec[i] == '4190' |Country.code.vec[i] == '4210' |Country.code.vec[i] == '4240' |Country.code.vec[i] == '4280' |
     Country.code.vec[i] == '4290' |Country.code.vec[i] == '4308') {ICD9.data  <- read.table(paste0('Data/Europe_CoD_data/ICD9-',Country.code.vec[i],'.txt'),header = T, sep = ',', stringsAsFactors = F)} else { ICD9.data <- NULL}
  
  
  
  if (Country.code.vec[i] == '4010' |
      Country.code.vec[i] == '4020' |
      Country.code.vec[i] =='4050' |
      Country.code.vec[i] =='4070' |
      Country.code.vec[i] == '4080' |
      Country.code.vec[i] =='4085') {ICD10.data <- read.table(paste0('Data/Europe_CoD_data/ICD10-',Country.code.vec[i],'.txt'),header = T, sep = ',', stringsAsFactors = F)} else {ICD10.data <- NULL}
  
    
  
  
  ICD10.data <- ICD10.data[ICD10.data$Year >= 1990,]
  ICD9.data <- ICD9.data[ICD9.data$Year >= 1990,]
  
  # rbind both datasets
  Deaths     <- rbind(ICD9.data,ICD10.data)
  
  # convert to data.table, it will make thinhk easier for applying functions
  Deaths     <- data.table(Deaths)
  
  unique(Deaths$Age)
  
  # add a numeric age, makes thinks easier to order when needed by age
  Deaths <- Deaths[!is.na(Deaths$Age)]
  code.age                     <- unique(Deaths$Age)
  
  # age2 is coded numerically, 96 is UNK and 97 is total
  code.age2                    <- c(97,0:4,seq(5,95,5),96)
  names(code.age2)             <- code.age
  Deaths$Age2                  <- code.age2[as.character(Deaths$Age)]
  Deaths                       <- Deaths[with(Deaths,order(Year,Age2,Cause)),]
  
  
  # Complete ages that have no values of causes of death and fill them with 0
  mat.Deaths.f <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Female',fill = 0, drop = F)
  mat.Deaths.m <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Male',fill = 0, drop = F)
  
  # now return to original shape
  Deaths.f      <- melt(mat.Deaths.f,id.vars = c('Age2','Cause'),
                        variable.name = 'Year',value.name = 'Female')
  Deaths.m      <- melt(mat.Deaths.m,id.vars = c('Age2','Cause'),
                        variable.name = 'Year',value.name = 'Male')
  Deaths.f$Year <- as.numeric(as.character(Deaths.f$Year))
  Deaths.m$Year <- as.numeric(as.character(Deaths.m$Year))
  
  # add the code and name of the country to the dataset
  Deaths.complete         <- Deaths.f
  Deaths.complete$Male    <- Deaths.m$Male
  Deaths.complete$X       <- Country.code.vec[i]
  Deaths.complete$Country <- Country.name.vec[i]
  
  
  # add a the label to the age, for consistency with the original files
  code.age3                     <- c(97,0:4,seq(5,95,5),96)
  # age2 is coded numerically, 96 is UNK and 97 is total
  code.age4                    <- code.age
  names(code.age4)             <- code.age3
  Deaths.complete$Age          <- code.age4[as.character(Deaths.complete$Age2)]
  
  # go back to the original shape
  Deaths.complete              <- Deaths.complete[,c('X','Year','Cause','Age','Female', 'Male', 'Country', 'Age2')]
  Deaths.complete              <- Deaths.complete[with(Deaths.complete,order(Year,Age2,Cause)),]
  
  #store all countries in one dataset
  print(Country.name.vec[i])
  print(unique(Deaths.complete$Year))
  Deaths.data <- data.table(rbind(Deaths.data,Deaths.complete))
}

Deaths.data <- Deaths.data[with(Deaths.data, order(X,Year,Cause,Age2)),]

gdata:: keep (Deaths.data ,Country.name.vec,Country.code.vec, sure = T)

