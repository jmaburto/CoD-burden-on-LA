setwd( "C:/Users/jmaburto/Documents/GitHub Backup 2/CoD-burden-on-LA/R/Decomp_Ranking_App/")

library(shiny)
library(ggplot2)

rsconnect::setAccountInfo(name='wb-lac',
                          token='06E52479A00F914E6EE44A17FFEA7E80',
                          secret='D7wg0N0miuHwY8reIRkCRy38E+TSCGXy+nHgm+KH')

library(ggplot2)
library(data.table)


load('Decomp_Rankings.RData')
load('Decomp_results_Europe.RData')

#Find conditions for UI

runApp()
