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

load('Prop_Data.RData')

shinyServer(
  function(input,output){
  output$CoD <- renderPlot(ggplot(Deaths2[Deaths2$X2 == input$name,], aes(Year,Proportion))+
    geom_line(aes(colour = Sex), lwd=1)+
    facet_wrap(~Cause2,scales = "free",ncol = 4)+ xlim(c(1990, 2015))+
    geom_vline(data=vline.data[vline.data$Country==input$name], aes(xintercept=ICD10.Year), colour="blue", lty=2 )+
    coord_fixed(1.5))}
)
  