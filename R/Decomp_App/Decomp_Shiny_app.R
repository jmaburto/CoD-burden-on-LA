# Create a shiny app to visualize life expectancies
# You'll need these packages to make things work, more from modern programming
#install.packages(c("dplyr", "tidyr", "ggplot2", "shiny",'rsconnect'), dependencies=TRUE)

setwd( "C:/Users/jmaburto/Documents/GitHub/CoD-burden-on-LA/R/Decomp_2App")
library(shiny)
library(ggplot2)
#rsconnect::setAccountInfo(name='jmaburto',
#                          token='7310E08D0D081D3C3CABCAA90D18045E',
#                          secret='Vzlie6RN39/THGhWKatGf/C68yZp+RENdTdOl/ey')

rsconnect::setAccountInfo(name='wb-lac',
                          token='06E52479A00F914E6EE44A17FFEA7E80',
                          secret='D7wg0N0miuHwY8reIRkCRy38E+TSCGXy+nHgm+KH')


Decomp_results[Decomp_results$Sources=='CEPAL2010' & Decomp_results$Country.name=='VENEZUELA',]


library(ggplot2)
library(data.table)


load('Decomp_results.RData')

#Find conditions for UI

table(Decomp_results[Decomp_results$Sources=='CEPAL2004',]$Country.name,Decomp_results[Decomp_results$Sources=='CEPAL2004',]$Period1,Decomp_results[Decomp_results$Sources=='CEPAL2004',]$Sources)


runApp()
