### This is a program to explore the dataset for Brazil, 

library(data.table)
library(reshape2)

setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")

#Create a vector with the countries we are interested in
Country.name.vec <- c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                      'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                      'Honduras','Mexico','Nicaragua','Panama','Argentina',
                      'Brazil','Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela')

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                      2340,2350,2020,2070,2120,2130,2180,2360,2370,2460,2470)

# the i for brazil is 14
i <- 14

    # read data for each country ICD9 and ICD10
  ICD9.data  <- read.table(paste0('Data/ICD9/ICD9-',Country.code.vec[i],'.txt'),
                           header = T, sep = ',', stringsAsFactors = F)
  
  ICD10.data <- read.table(paste0('Data/ICD10/ICD10-',Country.code.vec[i],'.txt'),
                           header = T, sep = ',', stringsAsFactors = F)
  
  # Year of implementation OF ICD10
  ICD10.y    <- c(min(ICD10.data$Year),Country.code.vec[i],Country.code.vec[i])
  
  # rbind both datasets
  Deaths     <- rbind(ICD9.data,ICD10.data)
  
  # convert to data.table, it will make thinhs easier for applying functions
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
  
  # take care of duplicates in Brazil
  if ( i == 14) { Deaths <- unique(Deaths, by = colnames(Deaths)[1:4])}
  
  # I checked Brazil, and it has a lot of duplicates
  
