library(shiny)
library(data.table)
names.c <- c("AUSTRIA","BELGIUM","DENMARK","FINLAND","FRANCE","GERMANY","GREECE","IRELAND","ITALY","LUXEMBOURG","NETHERLANDS","PORTUGAL","SPAIN","SWEDEN","UK")

shinyUI(
  fluidPage(
    titlePanel('Decomposition of a difference of life expectancy'),
    img(src='Logo.png', align = "right"),
    navbarPage(
      'Canudas-Romo, V. and Aburto, J.M. "Age- and Cause-Decomposition 
        of the Difference in Life-Expectancy in Latin American and Caribbean Countries." World Bank, 2017.',
      position = c("fixed-bottom")),
    hr(),
    fluidRow(
     column(3, selectInput( 'countries','Country',names.c, selected = 'MEXICO')),
     column(3, selectInput( 'sexes','Sex',choices = list('FEMALES'='f','MALES'='m'), selected = 'f')),
     column(3, selectInput( 'periods','Initial period',
                            choices = list("1990-1995"=1990, "1995-2000"=1995 ,"2000-2005"=2000 ,"2005-2010"=2005),
                            selected = 1))
    ),
  plotOutput('Decomp')
))
  
