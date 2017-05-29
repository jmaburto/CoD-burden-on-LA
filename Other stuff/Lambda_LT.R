#### Program to analyze data from the
#### Latin American Data Base


library(data.table)
library(latticeExtra)


setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")


### Load data
Data<-read.csv(file="Data/LAMbDA_LF.csv",header = T,stringsAsFactors = F)

### We are looking for lifetables for 23 countries, how many of those are in this database


# 3	Haiti
# 4	Jamaica
# 5	Puerto Rico
# 6	Trinidad and Tobago
# 15	Bolivia (Plurinational State of)

## These are in the database

countries <- unique(Data$Country)
# 14	Argentina

# 1	Cuba
# 10	Honduras
# 21	Peru
# 16	Brazil
# 2	Dominican Republic
# 11	Mexico
# 22	Uruguay
# 17	Chile
# 19	Ecuador
# 12	Nicaragua
# 23	Venezuela (Bolivarian Republic of)
# 18	Colombia
# 8	El Salvador
# 13	Panama
# 7	Costa Rica
# 9	Guatemala
# 20	Paraguay

Data <- data.table(Data)

years.country <- cbind(Data[,range(Year)[1], by = list(Country)],
                   Data[,range(Year)[2], by = list(Country)][,2,with=F])

lapply(countries, function(x){
  y <- unique(Data[Data$Country==x]$Year)
  y
})

