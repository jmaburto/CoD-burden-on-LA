# Create a shiny app to visualize cause of death diagnostics
# You'll need these packages to make things work, more from modern programming
#install.packages(c("dplyr", "tidyr", "ggplot2", "shiny",'rsconnect'), dependencies=TRUE)

# get proportion data
setwd("C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/CoD_App/")
library(shiny)
rsconnect::setAccountInfo(name='wb-lac',
                          token='06E52479A00F914E6EE44A17FFEA7E80',
                          secret='D7wg0N0miuHwY8reIRkCRy38E+TSCGXy+nHgm+KH')



runApp("R/app.R")

source('R/3_Proportions.R')

names.c <- unique(Deaths2$X2)


CoD <- function(data,vline.data,name){
  ggplot(Deaths2[Deaths2$X2 == name], aes(Year,Proportion))+
    geom_line(aes(colour = Sex), lwd=1)+
    geom_point(aes(colour = Sex), lwd=1)+
    facet_wrap(~Cause2,scales = "free",ncol = 4)+
    geom_vline(data=vline.data[vline.data$Country==name], aes(xintercept=ICD10.Year), colour="blue", lty=2 )
}


ui <- shinyUI(bootstrapPage(
  selectInput('name', 'Country', names.c),
  plotOutput("plot_pyr2")
))


server <- function(input,output){
  output$plot_pyr2 <- renderPlot({
    CoD(data = Deaths2, vline.data = vline.data, name = input$name)
  })
}

shinyApp(ui=ui,server=server)

setwd("C:/Users/jmaburto/Desktop/World Bank 2017/R/CoD_App")

runApp()
