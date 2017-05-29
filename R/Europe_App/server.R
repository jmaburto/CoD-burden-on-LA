library(ggplot2)

load('Europe_decomp.RData')

shinyServer(
  function(input, output) {
    output$Decomp <- renderPlot({

period    <- (input$periods)
country   <- (input$countries)
sx        <- (input$sexes)

#period <- 1990
#country <- 'AUSTRIA'
#sx      <- 'f'


Data <- Europe_decomp[Europe_decomp$Period1==period & Europe_decomp$Sex == sx & 
                        Europe_decomp$Country.name==country,]

#Data <- Decomp.results[Decomp.results$Country.name=='MEXICO' & Decomp.results$Sources=='Lambda' &
 #                Decomp.results$Sex == 1 & Decomp.results$Period1=='1990-1995' ,]

Data <- data.table(Data)
Data <-Data[Data$Age < 90,]
ages  <-  unique(Data$Age)
l     <-length(ages)
age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84',
                '85-89')

# age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
#                 '25-29','30-34','35-39','40-44','45-49','50-54',
#                 '55-59','60-64','65-69','70-74','75-79','80-84',
#                 '85-89','90-94','95-99','100-104','105-109','110+')

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
cause.name.vec      <- c('Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                         'Nervous','Endocrine','Digestive',
                         'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')

Data$Cause.name <- Data$Cause
Data$Cause.name <- factor(Data$Cause.name, levels = 2:15, labels = cause.name.vec)

cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$V1,2)),')')
source1 <- Data$Sources[1]
text.1 <- paste(paste('Period 1:',Period1,'-',as.character(Period1+4), '|','Life expectancy:',e01),'\n',
                paste('Period 2:',Period2,'-',as.character(Period2+4),'|','Life expectancy:',e02),'\n',
                paste('Difference',round(dife0,2)),'\n',
                'Numbers in boxes are age-specific contributions')
Data$w<-.85
Data[Data$Age==0]$w <- .85/5
# A bar graph
 ggplot(Data, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
  geom_bar(stat = "identity",width = Data$w)+
  geom_label(aes(Age.label, V1+.001, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
  ggtitle(Country)+
  annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1) + 
  scale_fill_discrete(name= ' Cause of death (Contribution)',labels = (cause.lab))+
  labs(x = "Age group", y = "Contribution (years)")
 library(ggplot2)

load('Europe_decomp.RData')

shinyServer(
  function(input, output) {
    output$Decomp <- renderPlot({

period    <- (input$periods)
country   <- (input$countries)
sx        <- (input$sexes)

#period <- 1990
#country <- 'AUSTRIA'
#sx      <- 'f'


Data <- Europe_decomp[Europe_decomp$Period1==period & Europe_decomp$Sex == sx & 
                        Europe_decomp$Country.name==country,]

#Data <- Decomp.results[Decomp.results$Country.name=='MEXICO' & Decomp.results$Sources=='Lambda' &
 #                Decomp.results$Sex == 1 & Decomp.results$Period1=='1990-1995' ,]

Data <- data.table(Data)
Data <-Data[Data$Age < 90,]
ages  <-  unique(Data$Age)
l     <-length(ages)
age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84',
                '85-89')

# age.label <-  c('0-1','1-4','5-9','10-14','15-19','20-24',
#                 '25-29','30-34','35-39','40-44','45-49','50-54',
#                 '55-59','60-64','65-69','70-74','75-79','80-84',
#                 '85-89','90-94','95-99','100-104','105-109','110+')

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
cause.name.vec      <- c('Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                         'Nervous','Endocrine','Digestive',
                         'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')

Data$Cause.name <- Data$Cause
Data$Cause.name <- factor(Data$Cause.name, levels = 2:15, labels = cause.name.vec)

cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$V1,2)),')')
source1 <- Data$Sources[1]
text.1 <- paste(paste('Period 1:',Period1,'-',as.character(Period1+4), '|','Life expectancy:',e01),'\n',
                paste('Period 2:',Period2,'-',as.character(Period2+4),'|','Life expectancy:',e02),'\n',
                paste('Difference',round(dife0,2)),'\n',
                'Numbers in boxes are age-specific contributions')
Data$w<-.85
Data[Data$Age==0]$w <- .85/5
# A bar graph
 ggplot(Data, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
  geom_bar(stat = "identity",width = Data$w)+
  geom_label(aes(Age.label, V1+.001, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
  ggtitle(Country)+
  annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1) + 
  scale_fill_discrete(name= ' Cause of death (Contribution)',labels = (cause.lab))+
  labs(x = "Age group", y = "Contribution (years)")
 
 
 
},width = 1500,height = 800)
})


 
 
},width = 1500,height = 800)
})

