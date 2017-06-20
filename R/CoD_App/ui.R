library(shiny)
library(data.table)
names.c <- sort(c('Argentina','Bolivia','Brazil','Chile','Colombia','Costa Rica','Cuba',
            'Dominican Republic','Ecuador','El Salvador','Guatemala','Haiti',
            'Honduras','Jamaica','Mexico','Nicaragua','Panama','Paraguay','Peru',
            'Puerto Rico','Trinidad and Tobago', 'Uruguay','Venezuela'))

shinyUI(
  fluidPage(
    titlePanel('Proportions by causes of death'),
    selectInput( 'name','Country',names.c, selected = 'Argentina'),
  plotOutput('CoD')
))
  
