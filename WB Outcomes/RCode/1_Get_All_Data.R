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
#Make sure to install packages needed:data.table, reshape2, ggplot, shiny, DT, latticeExtra,gdata,XLConnect,etc.

# put all the folders in one and set your working directory to this master folder
Your_working_directory <- 'C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA'

library(data.table)
library(reshape2)

setwd(Your_working_directory)

# Create a vector with the countries we are interested in
Country.name.vec <- c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                       'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                       'Honduras','Mexico','Nicaragua','Panama','Argentina',
                       'Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela',
                      'Haiti','Bolivia','Brazil')

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                       2340,2350,2020,2120,2130,2180,2360,2370,2460,2470,2270,2060,2070)


#function to correct cuba subtotals
Complete.cuba <- function(deaths){
  s1 <- sum(deaths[2:15])
  s2 <- deaths[1]
  deaths2 <- deaths
  deaths2[15] <- deaths2[15]+(s2-s1)
  deaths2
}

Deaths.data  <- NULL
ICD10.year   <- NULL
# Create a loop to find the year of change between ICD9 and ICD10 and create homogeneous datasets
for (i in 1:length(Country.code.vec)){
  
  # read data for each country ICD9 and ICD10
  
  if(Country.name.vec[i] == 'Bolivia' | Country.name.vec[i]=='Haiti'){ ICD9.data <- NULL} else {ICD9.data  <- read.table(paste0('Data/ICD9/ICD9-',Country.code.vec[i],'.txt'),
                          header = T, sep = ',', stringsAsFactors = F)}
  
  ICD10.data <- read.table(paste0('Data/ICD10/ICD10-',Country.code.vec[i],'.txt'),
                          header = T, sep = ',', stringsAsFactors = F)
  
  # Peru also has a year 0 in ICD10.data
  ICD10.data <- ICD10.data[ICD10.data$Year > 0,]
  
  # Year of implementation OF ICD10
  ICD10.y    <- c(ICD10.Year=min(ICD10.data$Year),Code=Country.code.vec[i],Country=Country.name.vec[i])
  ICD10.year <- rbind(ICD10.year,ICD10.y)
  # rbind both datasets
  Deaths     <- rbind(ICD9.data,ICD10.data)
  
  # convert to data.table, it will make thinhk easier for applying functions
  Deaths     <- data.table(Deaths)
  
  # Cuba has a year == 0, restrict to values > 0
  Deaths              <- Deaths[Deaths$Year > 0,]
  
  # add a numeric age, makes thinks easier to order when needed by age
  code.age                     <- unique(Deaths$Age)

  # age2 is coded numerically, 96 is UNK and 97 is total
  code.age2                    <- c(97,0:4,seq(5,95,5),96)
  names(code.age2)             <- code.age
  Deaths$Age2                  <- code.age2[as.character(Deaths$Age)]
  Deaths                       <- Deaths[with(Deaths,order(Year,Age2,Cause)),]
  
  # take care of duplicates in Brazil (IT DID NOT WORK COMPLETELY, SOME STRANGE RESULTS)
  if ( i == 14) { Deaths <- unique(Deaths, by = colnames(Deaths)[1:4]) }
  
  # Complete ages that have no values of causes of death and fill them with 0
  print(unique(Deaths$Cause))
  mat.Deaths.f <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Female',fill = 0, drop = F)
  mat.Deaths.m <- dcast(Deaths, Age2 + Cause ~ Year,value.var = 'Male',fill = 0, drop = F)
  
  # now return to original shape
  Deaths.f      <- melt(mat.Deaths.f,id.vars = c('Age2','Cause'),
                       variable.name = 'Year',value.name = 'Female')
  Deaths.m      <- melt(mat.Deaths.m,id.vars = c('Age2','Cause'),
                       variable.name = 'Year',value.name = 'Male')
  Deaths.f$Year <- as.numeric(as.character(Deaths.f$Year))
  Deaths.m$Year <- as.numeric(as.character(Deaths.m$Year))

  
  if (i == 1) {Deaths.cuba1 <- data.table(Deaths.f)
  Deaths.cuba2 <- data.table(Deaths.m)
    females <- Deaths.cuba1[,Complete.cuba(Female), by = list(Age2,Year)]
    males   <- Deaths.cuba2[,Complete.cuba(Male), by = list(Age2,Year)]
  Deaths.f$Female <- females$V1
  Deaths.m$Male <- males$V1
  }
  
  
  # correct cuba, there are inconsistencies with subtotals and totals 
  
  ###Make sure the category 15 containd the sum 2:14 -1
  yrs <- unique(Deaths.f$Year)
  ages <- unique(unique(Deaths.f$Age2))
  #k <- 0
  #j <- 1990
  for (k in ages){
  for (j in yrs){
  s1 <- (sum(Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][2:14,4]) + 
           Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][15,4])
  
  s2 <- s1 - Deaths.f[Deaths.f$Age2==k & Deaths.f$Year==j,][1,4]
  
  s1.1 <- (sum(Deaths.m[Deaths.m$Age2==k & Deaths.m$Year==j,][2:14,4]) + 
           Deaths.m[Deaths.m$Age2==k & Deaths.m$Year==j,][15,4])
  
  s2.1 <- s1.1 - Deaths.m[Deaths.m$Age2==k & Deaths.f$Year==j,][1,4]
  
  if (s2 != 0) print(c(k,s2,j))
  if (s2.1 != 0) print(c(k,s2.1,j))
  
  }}
  
  
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
  Deaths.data <- data.table(rbind(Deaths.data,Deaths.complete))
}

Deaths.data <- Deaths.data[with(Deaths.data, order(X,Year,Cause,Age2)),]

ICD10.year            <- data.table(ICD10.year)
ICD10.year$ICD10.Year <- as.numeric(ICD10.year$ICD10.Year)
ICD10.year$Code       <- as.numeric(ICD10.year$Code)
ICD10.year <- ICD10.year[with(ICD10.year, order(Code)),]

gdata:: keep (Deaths.data , ICD10.year,Country.name.vec,Country.code.vec, sure = T)

