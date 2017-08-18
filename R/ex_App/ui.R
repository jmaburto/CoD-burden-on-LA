library(shiny)
library(data.table)
library(plotly)

names.c <- toupper(sort(c('Argentina','Bolivia','Brazil','Chile','Colombia','Costa Rica','Cuba',
            'Dominican Republic','Ecuador','El Salvador','Guatemala','Haiti',
            'Honduras','Jamaica','Mexico','Nicaragua','Panama','Paraguay','Peru',
            'Puerto Rico','Trinidad and Tobago','Uruguay','Venezuela','Latin America')))

shinyUI(
  fluidPage(
    titlePanel('Life expectancy by sex'),
    img(src='Logo.png', align = "right"),
    navbarPage(
      'Canudas-Romo, V. and Aburto, J.M. "Age- and Cause-Decomposition 
      of the Difference in Life-Expectancy in Latin American and Caribbean Countries." World Bank, 2017.',
      position = c("fixed-bottom")),
    selectInput( 'name','Country',names.c, selected = 'Mexico'),
    plotlyOutput('ex')
))
  
