# Create a shiny app to visualize cause of death diagnostics
# You'll need these packages to make things work, more from modern programming
#install.packages(c("dplyr", "tidyr", "ggplot2", "shiny",'rsconnect'), dependencies=TRUE)

# get proportion data
#setwd("C:/Users/jmaburto/Desktop/World Bank 2017/R/CoD_App")
#source('R/3_Proportions.R')

library(ggplot2)


library(plotly)

load('Life_expectancy.RData')

shinyServer(
  function(input,output){
  output$ex <- renderPlot({ggplot(ex[ex$Country == input$name,], aes(Year,ex))+
                            geom_line(aes(colour = Source), lwd=1)+
                            geom_point(aes(shape = Source,colour = Source),cex=5)+
                            theme(text = element_text(size=22))+
                            scale_shape_manual(values=c(16:20))+
                            ylab("Life expectancy") +
                            theme(strip.text.x = element_text(size = 22))+
                            facet_wrap(~Sex,ncol = 2)},width = 1500,height = 700)
                          
})
  

