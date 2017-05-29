library(shiny)
library(data.table)
library(DT)
   
  names.c <- sort(toupper(c('Dominican Republic', 'Costa Rica','El Salvador', 'Guatemala','Mexico', 'Nicaragua', 
  'Argentina', 'Chile', 'Colombia', 'Ecuador', 'Paraguay',
  'Peru', 'Uruguay', 'Venezuela', 'Brazil', 'Cuba','Honduras','Panama','Jamaica')))
  
  
  Sources <- c('CEPAL2004','CEPAL2010','UN')
  Periods <- c("1990-1995", "1995-2000" ,"2000-2005" ,"2005-2010")
  
  shinyUI(
    fluidPage(
      titlePanel('Decomposition of life expectancy'),
      sidebarLayout(
        sidebarPanel(
          selectInput( 'sexes','Sex',choices = list('FEMALES'=2,'MALES'=1), selected = 2),
          br(),
          selectInput( 'countries','Country',names.c, selected = 'ARGENTINA'),
          br(),
          uiOutput('vx'),
          br(),
          uiOutput('vy'),
          br(),
          textOutput("text1"),
          width = 3
          
        ),
        mainPanel(
        tabsetPanel(tabPanel("Decomposition by age and cause of death",
                             # fluidRow(...)
                             plotOutput("Decomp")
        ),
        tabPanel("Cause-specific contributions", DT::dataTableOutput("mytable"))
        )))))
  
  
  