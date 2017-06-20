library(shiny)
library(data.table)
library(DT)
   
  names.c <- toupper(c('Total Latin America','High life expectancy','Medium high life expectancy','Medium low life expectancy','Low life expectancy'))
  
  shinyUI(
    fluidPage(
      titlePanel('Decomposition of life expetancy by level'),
      sidebarLayout(
        sidebarPanel(
          selectInput( 'sexes','Sex',choices = list('FEMALES'=2,'MALES'=1), selected = 2),
          br(),
          selectInput( 'level','Level of life expectancy',names.c, selected = 'TOTAL LATIN AMERICA'),
          textOutput("text1"),
          textOutput("text2"),
          textOutput("text3"),
          textOutput("text4"),
          width = 3
          
        ),
        mainPanel(
        tabsetPanel(tabPanel("Decomposition by age and cause of death",
                             # fluidRow(...)
                             plotOutput("CompareLAC")
        ),
        tabPanel("Cause-specific contributions",
                 DT::dataTableOutput("mytable2"),p("Note: Cause-specific Values  refer to ages 0-79. Information for ages 80 and above was omitted as it is problematic data"))
        )))))
  
  
  