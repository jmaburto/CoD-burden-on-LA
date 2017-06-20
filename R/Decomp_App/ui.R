library(shiny)
library(data.table)
library(DT)
   
  names.c <- sort(toupper(c('Dominican Republic', 'Costa Rica','El Salvador', 'Guatemala','Mexico', 'Nicaragua', 
  'Argentina', 'Chile', 'Colombia', 'Ecuador', 'Paraguay',
  'Peru', 'Uruguay', 'Venezuela', 'Brazil', 'Cuba','Honduras','Panama','Jamaica','Puerto Rico','Trinidad and Tobago')))
  
  
  Sources <- c('CEPAL2004','CEPAL2010','UN')
  Periods <- c("1990-1994", "1995-1999" ,"2000-2004" ,"2005-2009")
  
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
          textOutput("text2"),
          textOutput("text3"),
          width = 3
          
        ),
        mainPanel(
        tabsetPanel(tabPanel("Decomposition by age and cause of death",
                             # fluidRow(...)
                             plotOutput("Decomp")
        ),
        tabPanel("Cause-specific contributions",
                 DT::dataTableOutput("mytable"),p("Note: Values  represent only the results for ages 0-79. Information for ages 80 and above was omitted as it is problematic data")),
        tabPanel("Comparison with EU-15",plotOutput("Compare")),
        tabPanel("Cause-specific contributions vs EU-15",
                 DT::dataTableOutput("mytable2"),p("Note: Values  represent only the results for ages 0-79. Information for ages 80 and above was omitted as it is problematic data"))
        )))))
  
  
  