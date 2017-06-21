library(shiny)
library(data.table)
names.c <- sort(c('Argentina','Bolivia','Brazil','Chile','Colombia','Costa Rica','Cuba',
            'Dominican Republic','Ecuador','El Salvador','Guatemala','Haiti',
            'Honduras','Jamaica','Mexico','Nicaragua','Panama','Paraguay','Peru',
            'Puerto Rico','Trinidad and Tobago', 'Uruguay','Venezuela'))

shinyUI(
  fluidPage(
    titlePanel('Proportions by causes of death'),
    img(src='Logo.png', align = "right"),
    navbarPage(
      'Canudas-Romo, V. and Aburto, J.M. "Age- and Cause-Decomposition 
        of the Difference in Life-Expectancy in Latin American and Caribbean Countries." World Bank, 2017.',
      position = c("fixed-bottom")),
    selectInput( 'name','Country',names.c, selected = 'Argentina'),
  plotOutput('CoD')
  
))
  