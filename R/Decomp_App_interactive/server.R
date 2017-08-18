
#setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_App")
library(ggplot2)
library(plotly)
library(DT)
load('Decomp_results_Europe.RData')
load('Decomp_results.RData')

shinyServer(
  function(input, output) {
    
  
    
      var1 <- reactive( unique(Decomp_results$Period1[Decomp_results$Country.name==input$countries]))

      
      var2 <- reactive(unique(Decomp_results$Sources[Decomp_results$Country.name==input$countries]))
      
        
    
    output$vx <- renderUI({
      selectInput('periods','Initial period',choices = var1())
    })
    
    output$vy <- renderUI({
      selectInput('source','Available sources',choices = var2())
    })
    
    output$text1 <- renderText({
      period    <- (input$periods)
      country   <- (input$countries)
      sx        <- (input$sexes)
      source1   <- (input$source)
      Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
                               Decomp_results$Country.name==country & Decomp_results$Sources==source1,]
      Data <- data.table(Data)
      Country <- as.character(Data$Country.name[1])
      Period1 <- Data$Period1[1]
      Period2 <- Data$Period2[1]
      e01     <- round(Data$e01[1],2)
      e02     <- round(Data$e02[1],2)
      dife0   <- e02-e01
      t1 <- paste('Period 1:',Period1,'|','Life expectancy:',e01)
      t1
    })
    
    output$text2 <- renderText({
      period    <- (input$periods)
      country   <- (input$countries)
      sx        <- (input$sexes)
      source1   <- (input$source)
      Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
                               Decomp_results$Country.name==country & Decomp_results$Sources==source1,]
      Data <- data.table(Data)
      Country <- as.character(Data$Country.name[1])
      Period1 <- Data$Period1[1]
      Period2 <- Data$Period2[1]
      e01     <- round(Data$e01[1],2)
      e02     <- round(Data$e02[1],2)
      dife0   <- e02-e01
      t2 <- paste('Period 2:',Period2,'|','Life expectancy:',e02)
      t2
    })
    
    output$text3 <- renderText({
      period    <- (input$periods)
      country   <- (input$countries)
      sx        <- (input$sexes)
      source1   <- (input$source)
      Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
                               Decomp_results$Country.name==country & Decomp_results$Sources==source1,]
      Data <- data.table(Data)
      Country <- as.character(Data$Country.name[1])
      Period1 <- Data$Period1[1]
      Period2 <- Data$Period2[1]
      e01     <- round(Data$e01[1],2)
      e02     <- round(Data$e02[1],2)
      dife0   <- e02-e01
      t3 <- paste('Difference',round(dife0,2))
      t3
    })
    
    

      output$Decomp <- renderPlotly({

period    <- (input$periods)
country   <- (input$countries)
sx        <- (input$sexes)
source1   <- (input$source)
#period <- "1990-1994"
#country <- 'ARGENTINA'
#sx      <- 1
#source1 <- 'CEPAL2004'

Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
                         Decomp_results$Country.name==country & Decomp_results$Sources==source1,]


Data <- data.table(Data)

Data2 <-  Decomp_results[ Decomp_results$Country.name==country,]

  
Data3 <- Data
#Data3$Cause.name <- as.character(Data3$Cause.name)
Data3[Data3$Cause== 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
        Data3$Cause == 15]$Cause.name <- 'Rest'
Data3[Data3$Cause == 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
        Data3$Cause == 15]$Cause <- 15

Data3 <- Data3[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Period1,Period2,Sex,Country,Sources,e01,e02,Cause.name,Country.name)]

Data3 <- Data3[with(Data3, order(Cause,Age))]


Data2.pos <- data.table(Data2[Data2$Contribution >0,])
Data2.neg <- data.table(Data2[Data2$Contribution <0,])
Data2.pos <- Data2.pos[,sum(Contribution), by = list(Age,Period1,Sex,Sources)]
Data2.neg <- Data2.neg[,sum(Contribution), by = list(Age,Period1,Sex,Sources)]

ylim1 <- min(Data2.neg$V1)
ylim2 <- max(Data2.pos$V1)


#Data <- Decomp.results[Decomp.results$Country.name=='MEXICO' & Decomp.results$Sources=='Lambda' &
 #                Decomp.results$Sex == 1 & Decomp.results$Period1=='1990-1995' ,]


ages  <-  unique(Data$Age)
l     <-length(ages)
if (l == 18) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80+')
if (l == 19) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85+')
if (l == 20) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85-89','90+')
if (l == 21) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85-86','90-95','95+')

if (l == 22) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                             '25-29','30-34','35-39','40-44','45-49','50-54',
                             '55-59','60-64','65-69','70-74','75-79','80-84','85-86','90-95','95-99','100+')

Data3$Age.label <- Data3$Age
Data3$Age.label <- factor(Data3$Age.label, levels = ages, labels = age.label)

#Data to show in the plot
Country <- as.character(Data$Country.name[1])
Period1 <- Data$Period1[1]
Period2 <- Data$Period2[1]
e01     <- round(Data$e01[1],2)
e02     <- round(Data$e02[1],2)
dife0   <- e02-e01
Total.cause <- Data3[,sum(Contribution), by = list(Cause,Sex)]



Total.Age <- Data3[,sum(Contribution), by = list(Age.label,Sex)]
Total.Age$V1 <- round(Total.Age$V1,2)


cause.name.vec      <- c('Infectious','Neoplasms', 'Circulatory','Endocrine','Digestive','Perinatal','Respiratory','External','Homicide','Rest')
cause.code <- unique(Data3$Cause)

# c('Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
 #                        'Nervous','Endocrine','Digestive',
  #                       'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')




base2 <- toupper(c("#7265e7", "#e265e7", "#e7657d", "#e7bc65", "#a2e765", "#65e797", "#65c7e7",'orange','red','lightgrey'))

cause.lab <- paste(cause.name.vec,'(',as.character(round(Total.cause$V1,2)),')')
source1 <- Data$Sources[1]
text.1 <- paste('Numbers in boxes are age-specific contributions')

# text.2 <- paste(paste('Period 1:',Period1,'|','Life expectancy:',e01),'\n',
#                 paste('Period 2:',Period2,'|','Life expectancy:',e02),'\n',
#                 paste('Difference',round(dife0,2)),'\n',
#                 paste('Source:',source1),'\n',
#                 ' Cause of death (Contribution)')

Data3$w<-.85
Data3[Data3$Age==0]$w <- .85/5
# A bar graph
p <- ggplot(Data3, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
   scale_fill_manual(name= ' Cause of \n death',values=base2,labels = (cause.lab))+
  geom_bar(stat = "identity",width = Data3$w)+
   theme(text = element_text(size=18),
         axis.text.x = element_text(angle=45, hjust=1),legend.title=element_text(size=16)) +
  geom_label(aes(Age.label, ylim1, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
  ggtitle(Country)+
  coord_cartesian(ylim = c(ylim1, ylim2))+
  annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1,size=10) + 
  labs(x = "Age group", y = "Contribution (years)")

p <- ggplotly(p,width = 1250, height = 800)

config(p, displayModeBar = F, modeBarButtonsToRemove = list(
  'sendDataToCloud',
  'toImage',
  'autoScale2d',
  'resetScale2d',
  'hoverClosestCartesian',
  'hoverCompareCartesian',
  'zoom2d','pan2d','select2d','lasso2d','zoomIn2d'
))

#print(p)
 
 
 
})
    
      output$mytable = renderDataTable({
      
      period    <- (input$periods)
      country   <- (input$countries)
      sx        <- (input$sexes)
      source1   <- (input$source)
      
      Data <- Decomp_results[Decomp_results$Period1==period & Decomp_results$Sex == sx & 
                               Decomp_results$Country.name==country & Decomp_results$Sources==source1 &
                               Decomp_results$Age < 80,]
      Data <- data.table(Data)
      Total.cause2 <- Data[,sum(Contribution), by = list(Cause,Sex)]
      
      cause.name.vec2  <-c('a) (A00-B99) Certain infectious and parasitic diseases', 'b) (C00-D48) Neoplasms',
                           'f) (I00-I99) Diseases of the circulatory system','k) (R00-R99) Not elsewhere classified',
                           'd) (F01-F99) Mental and behavioural disorders','e) (G00-G98) Diseases of the nervous system',
                           'c) (E00-E88) Endocrine, nutritional and metabolic diseases' ,'g) (K00-K92) Diseases of the digestive system',  
                           'i) (N00-N98) Diseases of the genitourinary system','j) (P00-P96) Perinatal  & (Q00-Q99) Congenital malformations',
                           'h) (J00-J98) Diseases of the respiratory system','l) (V01-Y89) External mortality: accidents and suicide',
                           'm) (X85-Y09) Homicide', 'n) Rest of causes')
      
      Total.cause2$Cause <- cause.name.vec2
      Total.cause2$Contribution <- round(Total.cause2$V1,2)
      Total.cause2<- data.frame(Total.cause2[,c('Cause','Contribution')])
      Cause.Total <- data.frame(cbind(Cause='Total',Contribution=sum(Total.cause2$Contribution)))
      
      Total.cause2 <- rbind(Total.cause2,Cause.Total)
      Total.cause2 <- Total.cause2[with(Total.cause2,order(Cause)),]
      Total.cause2 <- data.frame(Total.cause2)
      rownames(Total.cause2) <- NULL
      Total.cause2$Contribution <- as.numeric(Total.cause2$Contribution)
      datatable(Total.cause2, options = list(paging=FALSE,ordering=T),rownames = F)
      })
    
      output$Compare <- renderPlotly({
        
        country   <- (input$countries)
        sx        <- (input$sexes)
        
        #country <- 'MEXICO'
        #sx      <- 1
        
        
        Data <- Decomp.comparision[Decomp.comparision$Sex == sx & 
                                     Decomp.comparision$Country.name==country,]
        
        
        Data <- data.table(Data)
        
        Data2 <-  Decomp_results[ Decomp_results$Country.name==country,]
        
        
        Data3 <- Data
        #Data3$Cause.name <- as.character(Data3$Cause.name)
        Data3[Data3$Cause== 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
                Data3$Cause == 15]$Cause.name <- 'Rest'
        Data3[Data3$Cause== 5 | Data3$Cause == 6 | Data3$Cause == 7 | Data3$Cause == 10 |
                Data3$Cause == 15]$Cause <- 15
        
        Data3 <- Data3[,list(Contribution=sum(Contribution)), by = list(Age,Cause,Sex,Country,Cause.name,Country.name)]
        
        Data3 <- Data3[with(Data3, order(Cause,Age))]
        
        
        #Data <- Decomp.results[Decomp.results$Country.name=='MEXICO' & Decomp.results$Sources=='Lambda' &
        #                Decomp.results$Sex == 1 & Decomp.results$Period1=='1990-1995' ,]
        
        
        ages  <-  unique(Data$Age)
        l     <-length(ages)
        
        if (l == 19) age.label <-  c('0','1-4','5-9','10-14','15-19','20-24',
                                     '25-29','30-34','35-39','40-44','45-49','50-54',
                                     '55-59','60-64','65-69','70-74','75-79','80-84','85+')
        Data3$Age.label <- Data3$Age
        Data3$Age.label <- factor(Data3$Age.label, levels = ages, labels = age.label)
        
        #Data to show in the plot
        Country <- as.character(Data$Country.name[1])
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
        p <- ggplot(Data3, aes(x = Age.label, y = Contribution, fill = Cause.name)) +
          scale_fill_manual(name= ' Cause of death (Contribution)',values=base2,labels = (cause.lab))+
          geom_bar(stat = "identity",width = Data3$w)+
          theme(text = element_text(size=22),
                axis.text.x = element_text(angle=45, hjust=1)) +
          geom_label(aes(Age.label, ylim1+ylim1/5, label = V1,fill=NULL),size=5,show.legend = FALSE, data = Total.Age)+
          ggtitle(paste(Country,"vs EU-15 (2010-2014)"))+
          #coord_cartesian(ylim = c(ylim1+ylim1/5, ylim2)) +
          #scale_y_continuous(limits = c(ylim1, ylim2))+
          annotate("text", x=Inf, y=Inf,hjust=1,vjust=1, label= text.1,size=6) + 
          labs(x = "Age group", y = "Contribution (years)")
        
        print(ggplotly(p,width = 1300, height = 800)) 
        
        
      })
      
      output$mytable2 = renderDataTable({
        
        
        country   <- (input$countries)
        sx        <- (input$sexes)
        
        
        Data <- Decomp.comparision[Decomp.comparision$Sex == sx & 
                                     Decomp.comparision$Country.name==country,]
        
        
        Data <- data.table(Data)
        
        
        Total.cause2 <- Data[,sum(Contribution), by = list(Cause.name,Sex)]
        
        
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
        
        datatable(Total.cause2, options = list(paging=FALSE,ordering=T),rownames = F)
      })
      
      
})

# column(3, selectInput( 'source','Sources',Sources, selected = 'CEPAL2004')),
# # Choices of Periods
# column(3,conditionalPanel(
#   condition="input.countries=='DOMINICAN REPUBLIC' || input.countries=='COSTA RICA' || 
#           input.countries=='EL SALVADOR'|| input.countries=='GUATEMALA'|| input.countries=='MEXICO'||
#           input.countries=='NICARAGUA'|| input.countries=='ARGENTINA'|| input.countries=='CHILE'||
#           input.countries=='COLOMBIA'|| input.countries=='ECUADOR'|| input.countries=='PARAGUAY'|| 
#           input.countries=='PERU'|| input.countries=='URUGUAY'|| input.countries=='VENEZUELA'|| 
#           input.countries=='BRAZIL'|| input.countries=='CUBA'",
#   selectInput( 'periods','Initial period',Periods, selected = '1990-1995')
# ),
# conditionalPanel(
#   condition="input.countries=='HONDURAS'",
#   selectInput( 'periods','Initial period',c('1990-1995'), selected = '1990-1995')
# ),
# conditionalPanel(
#   condition="input.countries=='PANAMA'",
#   selectInput( 'periods','Initial period',c("1995-2000" ,"2000-2005" ,"2005-2010"), selected = '1995-2000')
# ))
# 
# #      conditionalPanel(
# #        condition="input.countries=='JAMAICA'",
# #        selectInput( 'source','Sources',c('UN'), selected = 'UN')
# #      ),