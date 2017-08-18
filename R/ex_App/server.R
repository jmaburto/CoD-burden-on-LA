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
  output$ex <- renderPlotly({ 
    ex$ex <- round(ex$ex,2)
  
    
    p <- ggplot(ex[ex$Country == input$name,], aes(x = Year,y = ex,colour=Source))+
                            geom_point(aes(shape = Source),cex=5)+
                            geom_line(aes(shape = Source ))+
                            facet_wrap(~Sex) +
                            theme(text = element_text(size=22))+
                            scale_shape_manual(values=c(16:20))+
                            ylab("Life expectancy") +
                            theme(strip.text.x = element_text(size = 22))
                            
  print(ggplotly(p,width = 1500, height = 700))
    
    
    })
                          
})
  

