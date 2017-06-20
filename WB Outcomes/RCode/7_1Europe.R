
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

# not Greece, no CoD info (4140 )
Country.code.vec <- c(4010,4020,4050,4070,4080,4085,4170,4180,4190,4210,4240,4280,4290,4308,4140)

Country.name.vec <- toupper(c('Austria','Belgium','Denmark','Finland','France','Germany','Ireland','Italy',
          'Luxembourg','Netherlands','Portugal','Spain','Sweden','UK','Greece'))

names(Country.code.vec) <- Country.name.vec


Deaths.data  <- NULL

#i<- 15
# Create a loop to find the year of change between ICD9 and ICD10 and create homogeneous datasets
for (i in 1:length(Country.code.vec)){
  
if (Country.code.vec[i]!=4140) ICD10.data  <- read.table(paste0('Data/Europe_CoD_data/ICD10-ICD10-',Country.code.vec[i],'.txt'),header = T, sep = ',', stringsAsFactors = F)
if (Country.code.vec[i]==4140) ICD10.data  <- read.table(paste0('Data/Europe_CoD_data/ICD9-',Country.code.vec[i],'.txt'),header = T, sep = ',', stringsAsFactors = F)


ICD10.data  <- ICD10.data[ICD10.data$Year >= 2010,]
Deaths      <- ICD10.data
Deaths      <- data.table(Deaths)
unique(Deaths$Age)
unique(Deaths$Year)
unique(Deaths$Cause)
Deaths      <- Deaths[!is.na(Deaths$Age)]
code.age    <- unique(Deaths$Age)
  
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
Deaths.data[Deaths.data$Age2 >= 1 & Deaths.data$Age2 <= 4,]$Age2 <- 1
Deaths.data[Deaths.data$Age2 ==1,]$Age <- "1-4"
Deaths.data <- Deaths.data[Deaths.data$Age2 < 96,]
Deaths.data[Deaths.data$Age2 >= 85,]$Age2 <- 85
Deaths.data[Deaths.data$Age2 ==85,]$Age <- "85+"

Deaths.data2 <- Deaths.data[,list(Female=sum(Female)), by = list(X,Year,Cause,Age,Country,Age2)]
Deaths.data2$Male<- Deaths.data[,list(Male=sum(Male)), by = list(X,Year,Cause,Age,Country,Age2)]$Male



### download information from HMD for the countries we are interested in
###Get HMD Data
library(HMDHFDplus)
XYZ <-c('AUT','BEL','DNK','FIN','FRATNP','DEUTNP','IRL','ITA','LUX','NLD','PRT','ESP','SWE','GBR_NP')

us <- "username fromHMD"
pw <- "Password"
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_5x5",username=us,password=pw,)
  Females      <- readHMDweb(x,"fltper_5x5",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY
}, us = us, pw = pw))

HMDL <- data.table(HMDL)
HMDL <- HMDL[HMDL$Year >= 2010,]



#get data for  greece
greece.f <- read.table('Data/Europe_CoD_data/fltper_5x5.txt',header = T, sep = '', stringsAsFactors = F)
greece.m <- read.table('Data/Europe_CoD_data/fltper_5x5.txt',header = T, sep = '', stringsAsFactors = F)
greece.f <- greece.f[145:168,]
greece.m <- greece.m[145:168,]


# put greece in the same format
greece.f$Year <- 2010
greece.m$Year <- 2010
greece.f$OpenInterval <- FALSE
greece.f$Sex <- 'f'
greece.f$PopName <- 'GRC'

greece.m$OpenInterval <- FALSE
greece.m$Sex <- 'm'
greece.m$PopName <- 'GRC'

Ages <- HMDL[HMDL$PopName=='AUT' & HMDL$Sex== 'f',]$Age
greece.m$Age <- Ages
greece.f$Age <- Ages

HMDL <- rbind(HMDL,greece.f,greece.m)

save(HMDL,Deaths.data ,file = 'Data/Europe_Data.RData')

gdata:: keep (Deaths.data ,Country.name.vec,Country.code.vec,HMDL, sure = T)

