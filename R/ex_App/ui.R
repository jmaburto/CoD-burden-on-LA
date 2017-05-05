library(shiny)
library(data.table)
names.c <- toupper(c('Argentina','Bolivia','Brazil','Chile','Colombia','Costa Rica','Cuba',
            'Dominican Republic','Ecuador','El Salvador','Guatemala','Haiti',
            'Honduras','Jamaica','Mexico','Nicaragua','Panama','Paraguay','Peru',
            'Puerto Rico','Trinidad and Tobago','Uruguay','Venezuela'))

shinyUI(
  fluidPage(
    titlePanel('Life expectancy by sex'),
    selectInput( 'name','Country',names.c, selected = 'Argentina'),
  plotOutput('ex')
))
  
