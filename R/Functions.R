# useful functions
excel.to.R <- function(i,wb = df_CEPAL2004,t = 'CEPAL2004'){
  Z        <- wb[i]
  Z        <- Z[-1L,]
  Z$Source <- t     
  Z
}

excel.to.R2 <- function(i,wb = df_CEPAL2004,t = 'CEPAL2004'){
  Z        <- wb[i]
  Z$Source <- t     
  Z
}

# Create a vector with the countries we are interested in
Country.name.vec <- toupper(c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                      'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                      'Honduras','Mexico','Nicaragua','Panama','Argentina',
                      'Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela',
                      'Haiti','Bolivia','Brazil', 'Latin America'))

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                      2340,2350,2020,2120,2130,2180,2360,2370,2460,2470,2270,2060,2070,9999)
names(Country.code.vec) <- Country.name.vec