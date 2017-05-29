######### Work with results from lifetables and decomposition

setwd('C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA')
library(reshape2)
library(data.table)
source("R/Functions.R")

load('Outcomes/Decomp_results.RData')
load('Outcomes/LatinAmerica_Lifetables.RData')

### Table with life expectancy at birth for this countries
Males          <- Decomp.results[Decomp.results$Age==0 & Decomp.results$Cause==2 & Decomp.results$Sex==1,]
Males$dif      <- Males$e02-Males$e01
Females        <- Decomp.results[Decomp.results$Age==0 & Decomp.results$Cause==2 & Decomp.results$Sex==2,]
Females$dif    <- Females$e02-Females$e01
Results.table1 <- cbind(Males,Females[,c('e01','e02','dif')])
Results.table1$Country.name <- Results.table1$Country
Results.table1$Country.name <- factor(Results.table1$Country.name, levels = Country.code.vec, labels = Country.name.vec)

write.csv(Results.table1,file = 'Outcomes/Result_Table1.csv')

##### A table with the contributions to the change in life expectancy by Cause
Decomp.results        <- data.table(Decomp.results)
Decomp.results$Change <- Decomp.results$e02-Decomp.results$e01
Results.table2        <- Decomp.results[,list(Contribution=sum(Contribution)), 
                                        by = list(Cause,Period1,Period2,Sex,Country,Sources,Change)]
Results.table2$Prop   <- Results.table2$Contribution/Results.table2$Change*100
Results.table2$Country.name <- Results.table2$Country
Results.table2$Country.name <- factor(Results.table2$Country.name, levels = Country.code.vec, labels = Country.name.vec)

Results.table2$Cause.name <- Results.table2$Cause
Results.table2$Cause.name <- factor(Results.table2$Cause.name, levels = cause.code.vec, labels = cause.name.vec)
Table2.males  <- Results.table2[Results.table2$Sex==1,] 
Table2.females<- Results.table2[Results.table2$Sex==2,] 
Table2 <- cbind(Table2.males, Table2.females[,c('Change','Contribution','Prop')])

write.csv(Table2,file = 'Outcomes/Result_Table2.csv')


gdata::keep(Decomp.results,Lifetables,sure=T)
source("R/Functions.R")
#### Some plots, for UN
#### one example
library(ggplot2)
library(data.table)



Data <- Decomp.results[Decomp.results$Period1=='2000-2005' & Decomp.results$Sex == 2 & 
                            Decomp.results$Country==2180 & Decomp.results$Sources=='UN',]

ages  <-  unique(Data$Age)
l     <-length(ages)
if (l == 18) age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80+')
if (l == 19) age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85+')
if (l == 20) age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90+')
if (l == 21) age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85-86','90-95','100+')

Data$Age.label <- Data$Age
Data$Age.label <- factor(Data$Age.label, levels = ages, labels = age.label)

#Data to show in the plot
Country <- as.character(Data$Country.name[1])
Period1 <- Data$Period1[1]
Period2 <- Data$Period2[1]
e01     <- round(Data$e01[1],2)
e02     <- round(Data$e02[1],2)
dife0   <- e02-e01
Total.cause <- Data[,sum(Contribution), by = list(Cause,Sex)]
Total.Age <- Data[,sum(Contribution), by = list(Age.label,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)
cause.name.vec      <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                         'Nervous','Endocrine','Digestive',
                         'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')
cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$V1,2)),')')
source1 <- Data$Sources[1]
text.1 <- paste(paste('Period 1:',Period1,'|','Life expectancy:',e01),'\n',
              paste('Period 2:',Period1,'|','Life expectancy:',e02),'\n',
              paste('Difference',round(dife0,2)),'\n',
              paste('Source:',source1),'\n',
              'Numbers in boxes are age-specific contributions')

# A bar graph
    ggplot(Data, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
    geom_bar(stat = "identity")+
      geom_label(aes(Age.label, V1+.001, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
      ggtitle(Country)+
      annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1) + 
      scale_fill_discrete(name= ' Cause of death (Contribution)',labels = (cause.lab))+
      labs(x = "Age group", y = "Contribution (years)")
    
   runApp()

   
   ### Save_results in csv files
   
   xyz <- unique(Decomp.results$Country)
   #i <- xyz[1]
   for (i in xyz){
     D1 <- Decomp.results[Decomp.results$Country==xyz,]
     write.csv(D1,file = paste0('Outcomes/Main Results/Decomp_Results_',as.character(i),'.csv'))
     
     D2 <- Lifetables[Lifetables$Country==i,]
     write.csv(D1,file = paste0('Outcomes/Main Results/Lifetables_',as.character(i),'.csv'))
   }
   