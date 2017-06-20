
#setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_App")
library(ggplot2)
library(DT)
load('Decomp_Rankings.RData')
load('Decomp_results_Europe.RData')

shinyServer(
  function(input, output) {
  
    
    output$text1 <- renderText({
      t1 <- 'Level of life expectancy refers to the difference between life expectancies: '
      t1
    })
    
    output$text2 <- renderText({
      t2 <- 'maximum in the region minus in each country. This level includes:'
      t2
    })
    
    output$text3 <- renderText({
      level.LAC <- input$level
      if (level.LAC == toupper('Total Latin America'))         t3 <- 'All the countries in the region.'
      if (level.LAC == toupper('High life expectancy'))        t3 <- 'Chile, Costa Rica, Puerto Rico, Cuba, Uruguay, Panama & Mexico.'
      if (level.LAC == toupper('Medium high life expectancy')) t3 <- 'Argentina, Ecuador, Jamaica, Nicaragua, Venezuela & Peru.'
      if (level.LAC == toupper('Medium low life expectancy'))  t3 <- 'Colombia, Brazil, Dominican Republic, Honduras, Paraguay & El Salvador.'
      if (level.LAC == toupper('Low life expectancy'))         t3 <- 'Guatemala, Trinidad and Tobago, Bolivia & Haiti.'
      t3
    })
    
    output$text4 <- renderText({
      level.LAC <- input$level
      if (level.LAC == toupper('Total Latin America'))         l <- NULL
      if (level.LAC == toupper('High life expectancy'))        l <- "less than 4 years from the maximum"
      if (level.LAC == toupper('Medium high life expectancy')) l <- "between 4 and 6 years from the maximum"
      if (level.LAC == toupper('Medium low life expectancy'))  l <- "between 6 and 8 years from the maximum"
      if (level.LAC == toupper('Low life expectancy'))         l <- "more than 8 years from the maximum"
      t4 <- paste('Refers to countries with total life expectancy, female and male combined,',l)
      if (level.LAC == toupper('Total Latin America')) t4 <- NULL
      t4
    })
    
    
      
      output$CompareLAC <- renderPlot({
        #level.LAC <- toupper('High life expectancy')
        #level.LAC <- toupper('Total Latin America')
        #sx      <- 1
        #unique(Decomp.Rankings$level)
        
        level.LAC <- input$level
        sx      <-  input$sexes
        
        if (level.LAC == toupper('Total Latin America'))         Data <- Decomp.comparision[Decomp.comparision$Sex == sx & Decomp.comparision$Country.name=='LATIN AMERICA',]
        if (level.LAC == toupper('High life expectancy'))        Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='High life expectancy',]
        if (level.LAC == toupper('Medium high life expectancy')) Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Medium high life expectancy',]
        if (level.LAC == toupper('Medium low life expectancy'))  Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Medium low life expectancy',]
        if (level.LAC == toupper('Low life expectancy'))         Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Low life expectancy',]
        
        
        Data <- data.table(Data)
        
      
        Data3 <- Data
        #Data3$Cause.name <- as.character(Data3$Cause.name)
        Data3[Data3$Cause== 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
                Data3$Cause == 15]$Cause.name <- 'Rest'
        Data3[Data3$Cause == 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
                Data3$Cause == 15]$Cause <- 15
        
        Data3 <- Data3[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Sex,Cause.name)]
        
        Data3 <- Data3[with(Data3, order(Cause,Age))]
        
        
        ages  <-  unique(Data$Age)
        l     <-length(ages)
        
        if (l == 19) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                                     '25-29','30-34','35-39','40-44','45-49','50-54',
                                     '55-59','60-64','65-69','70-74','75-79','80-84','85+')
        Data3$Age.label <- Data3$Age
        Data3$Age.label <- factor(Data3$Age.label, levels = ages, labels = age.label)
        
        #Data to show in the plot
        Country <- level.LAC
        Period1 <- Data$Period1[1]
        e01     <- round(Data$e01[1],2)
        e02     <- round(Data$e02[1],2)
        dife0   <- round(e02-e01,2)
        Total.cause <- Data3[,sum(Contribution), by = list(Cause,Sex)]
        
        
        Total.Age <- Data3[,sum(Contribution), by = list(Age.label,Sex)]
        Total.Age$V1 <- round(Total.Age$V1,2)
        
        
        cause.name.vec      <- c('Infectious','Neoplasms', 'Circulatory','Endocrine','Digestive','Perinatal','Respiratory','External','Homicide','Rest')
        cause.code <- unique(Data3$Cause)
        
        
        base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7",'orange','red','lightgrey'))
        
        cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$V1,2)),')')
        source1 <- Data$Sources[1]
        text.1 <- paste(Country,':',e01,'| EU-15:',e02,'\n','Difference:',dife0,'\n','Source: UN')
        
        Data3$w<-.85
        Data3[Data3$Age==0]$w <- .85/5
        
        Data2.pos <- Data3[Data3$Contribution >0,]
        Data2.neg <- Data3[Data3$Contribution <0,]
        Data2.pos <- Data2.pos[,sum(Contribution), by = list(Age)]
        Data2.neg <- Data2.neg[,sum(Contribution), by = list(Age)]
        
        ylim1 <- min(Data2.neg$V1)
        ylim2 <- max(Data2.pos$V1)
        
        # A bar graph
        ggplot(Data3, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
          scale_fill_manual(name= ' Cause of death (Contribution)',values=base2,labels = (cause.lab))+
          geom_bar(stat = "identity",width = Data3$w)+
          theme(text = element_text(size=22),
                axis.text.x = element_text(angle=45, hjust=1)) +
          geom_label(aes(Age.label, ylim1+ylim1/5, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
          ggtitle(paste(Country,"vs EU-15 (2010-2014)"))+
          coord_cartesian(ylim = c(-.5, 3)) +
          #scale_y_continuous(limits = c(-.5, 3))+
          annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1,size=6) + 
          labs(x = "Age group", y = "Contribution (years)")
        
        
        
      },width = 1300,height = 800)
      
      output$mytable2 = renderDataTable({
        
        level.LAC <- input$level
        sx      <-  input$sexes
        
        if (level.LAC == toupper('Total Latin America'))         Data <- Decomp.comparision[Decomp.comparision$Sex == sx & Decomp.comparision$Country.name=='LATIN AMERICA',]
        if (level.LAC == toupper('High life expectancy'))        Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='High life expectancy',]
        if (level.LAC == toupper('Medium high life expectancy')) Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Medium high life expectancy',]
        if (level.LAC == toupper('Medium low life expectancy'))  Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Medium low life expectancy',]
        if (level.LAC == toupper('Low life expectancy'))         Data <- Decomp.Rankings[Decomp.Rankings$Sex == sx & Decomp.Rankings$level=='Low life expectancy',]
        
        
        Data <- data.table(Data)
        
        
        Total.cause2 <- Data[,sum(Contribution), by = list(Cause,Sex)]
        
        
        
        cause.name.vec2  <-c('a) (A00-B99) Certain infectious and parasitic diseases', 
                             'n) Rest of causes',
                             'b) (C00-D48) Neoplasms',
                             'f) (I00-I99) Diseases of the circulatory system',
                             'k) (R00-R99) Not elsewhere classified',
                             'd) (F01-F99) Mental and behavioural disorders',
                             'e) (G00-G98) Diseases of the nervous system',
                             'c) (E00-E88) Endocrine, nutritional and metabolic diseases' ,
                             'g) (K00-K92) Diseases of the digestive system',  
                             'i) (N00-N98) Diseases of the genitourinary system',
                             'j) (P00-P96) Perinatal  & (Q00-Q99) Congenital malformations',
                             'h) (J00-J98) Diseases of the respiratory system',
                             'l) (V01-Y89) External mortality: accidents and suicide',
                             'm) (X85-Y09) Homicide')
        
        Total.cause2$Cause <- cause.name.vec2
        Total.cause2$Contribution <- round(Total.cause2$V1,2)
        Total.cause2<- data.frame(Total.cause2[,c('Cause','Contribution')])
        Cause.Total <- data.frame(cbind(Cause='Total',Contribution=sum(Total.cause2$Contribution)))
        
        Total.cause2 <- rbind(Total.cause2,Cause.Total)
        Total.cause2 <- Total.cause2[with(Total.cause2,order(Cause)),]
        Total.cause2 <- data.frame(Total.cause2)
        rownames(Total.cause2) <- NULL
        Total.cause2$Contribution <- as.numeric(Total.cause2$Contribution)
        formatRound(datatable(Total.cause2, options = list(paging=FALSE,ordering=T),rownames = F),columns = 'Contribution',2)
        
        
        
        
      })
      
      
})
