# Create a shiny app to visualize cause of death diagnostics
# You'll need these packages to make things work, more from modern programming
#install.packages(c("dplyr", "tidyr", "ggplot2", "shiny",'rsconnect'), dependencies=TRUE)

# rsconnect::setAccountInfo(name='jmaburto',
#                           token='7310E08D0D081D3C3CABCAA90D18045E',
#                           secret='Vzlie6RN39/THGhWKatGf/C68yZp+RENdTdOl/ey')

# get proportion data
#setwd("C:/Users/jmaburto/Desktop/World Bank 2017/R/CoD_App")
#source('R/3_Proportions.R')

library(ggplot2)

load('Life_expectancy.RData')

shinyServer(
  function(input,output){
  output$ex <- renderPlot(ggplot(ex[ex$Country == input$name,], aes(Year,ex))+
                            geom_line(aes(colour = Source), lwd=1)+
                            geom_point(aes(shape = Source,colour = Source),cex=5)+
                            scale_shape_manual(values=c(16:20))+
                            theme(strip.text.x = element_text(size = 15))+
                            facet_wrap(~Sex,ncol = 2)
                          
)})
  

