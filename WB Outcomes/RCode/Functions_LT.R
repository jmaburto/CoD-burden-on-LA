### Useful functions

### Function to get the first ax
AKm02a0 <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

### Function to get life expectancy at birth from a vector of mx
e0.from.mx <-function(mx=mx,Ages=Age,Sex=Sex){
  sex <- 'm'
  if (Sex[1] == 2 ) {sex <- 'f'}
  Widths              <- diff(Ages)
  N                   <- length(mx)
  RADIX               <- 1
  i.openage           <- length(mx)
  OPENAGE             <- i.openage - 1
  Widths              <-  c(Widths, Widths[N - 1])
  ax                  <- mx * 0 + Widths/2
  ax[1]               <- AKm02a0(m0 = mx[1], sex = sex)
  qx                  <- (Widths*mx) / (1 + (Widths - ax) * mx)
  qx[i.openage]       <- 1
  qx[qx > 1] 	        <- 1
  px 				          <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			            <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))[1:N]
  dx 				          <-  c(-diff(lx),lx[N])
  Lx 	                <- c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * (1-exp(-10*mx[1]))/mx[1])
  Lx[is.infinite(Lx)] <- 1
  Lx[is.na(Lx)] 	    <- 0
  Tx 				          <- rev(cumsum(rev(Lx)))
  ex 				          <- Tx / lx
  ex[is.na(ex)] 	    <- 0
  ex[i.openage]       <- ifelse(mx[i.openage] == 0, ax[i.openage], {1 / mx[i.openage]})
  ex[1]
}


### Function to get a lifetable from a vector of mx
LT.from.mx <- function(mx=mx,Ages=Age,Sex=Sex){
                  sex <- 'm'
  if (Sex[1] == 2 ) {sex <- 'f'}
  Widths              <- diff(Ages)
  N                   <- length(mx)
  RADIX               <- 1
  i.openage           <- length(mx)
  OPENAGE             <- i.openage - 1
  Widths              <-  c(Widths, Widths[N - 1])
  ax                  <- mx * 0 + Widths/2
  ax[1]               <- AKm02a0(m0 = mx[1], sex = sex)
  qx                  <- (Widths*mx) / (1 + (Widths - ax) * mx)
  qx[i.openage]       <- 1
  qx[qx > 1] 	        <- 1
  px 				          <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			            <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))[1:N]
  dx 				          <-  c(-diff(lx),lx[N])
  Lx 	                <- c(Widths[1:(N - 1)] * lx[2:N] + ax[1:(N - 1)] * dx[1:(N - 1)], lx[N] * (1-exp(-10*mx[1]))/mx[1])
  Lx[is.infinite(Lx)] <- 1
  Lx[is.na(Lx)] 	    <- 0
  Tx 				          <- rev(cumsum(rev(Lx)))
  ex 				          <- Tx / lx
  ex[is.na(ex)] 	    <- 0
  ex[i.openage]       <- ifelse(mx[i.openage] == 0, ax[i.openage], {1 / mx[i.openage]})
  LT <- data.frame(cbind(Age=Ages,mx=mx,lx=lx,dx=dx,Lx=Lx,Tx=Tx,ex))
  return(LT)
}


#example of decomp
#mx1 <- Data.LT[Data.LT$Year==1992.5 & Data.LT$Sex=="1" & Data.LT$Source=='UN' & Data.LT$Country=='ARGENTINA',]$mx
#mx2 <- Data.LT[Data.LT$Year==1997.5 & Data.LT$Sex=="1" & Data.LT$Source=='UN' & Data.LT$Country=='ARGENTINA',]$mx
#Age <-Data.LT[Data.LT$Year==1997.5 & Data.LT$Sex=="1" & Data.LT$Source=='UN' & Data.LT$Country=='ARGENTINA',]$Age
#Sex <- 'm'

# code from VCR
AgeDecomp<-function(mx1,mx2,Age=Age,Sex=Sex,...){

  ##  new Year/Country  life table = A2
  A2 <- LT.from.mx(mx2,Age,Sex)
  ## old Year/Country life table =   A1
  A1 <- LT.from.mx(mx1,Age,Sex)
  
  l1<- A1$lx
  l2<- A2$lx
  d1<- A1$dx
  d2<- A2$dx
  L1<- A1$Lx
  L2<- A2$Lx
  T1<- A1$Tx
  T2<- A2$Tx
  ##  Last Age Group 
  
  LAG<-dim(A1)[1]
  ##  calculate the direct and indirect components
  
  DE<-(l1/l1[1])*((L2/l2)-(L1/l1))
  
  IE<-(T2[-1]/l1[1])*((l1[-LAG]/l2[-LAG])-(l1[-1]/l2[-1]))
  
  # one extra value for the indirect component
  # since there is only direct component in the last age group
  IE<-c(IE,0)
  
  ## add both to get the overall age-decomposition
  ALL<-DE+IE
  return(ALL)
}


AgeDecomp2<-function(A1,A2){
  
  l1<- A1$lx
  l2<- A2$lx
  d1<- A1$dx
  d2<- A2$dx
  L1<- A1$Lx
  L2<- A2$Lx
  T1<- A1$Tx
  T2<- A2$Tx
  ##  Last Age Group 
  
  LAG<-dim(A1)[1]
  ##  calculate the direct and indirect components
  
  DE<-(l1/l1[1])*((L2/l2)-(L1/l1))
  
  IE<-(T2[-1]/l1[1])*((l1[-LAG]/l2[-LAG])-(l1[-1]/l2[-1]))
  
  # one extra value for the indirect component
  # since there is only direct component in the last age group
  IE<-c(IE,0)
  
  ## add both to get the overall age-decomposition
  ALL<-DE+IE
  return(ALL)
}


Country.name.vec <- toupper(c('Cuba','Dominican Republic','Jamaica','Puerto Rico',
                              'Trinidad and Tobago','Costa Rica','El Salvador','Guatemala',
                              'Honduras','Mexico','Nicaragua','Panama','Argentina',
                              'Chile','Colombia','Ecuador','Paraguay','Peru','Uruguay','Venezuela',
                              'Haiti','Bolivia','Brazil', 'Latin America'))

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(2150,2170,2290,2380,2440,2140,2190,2250,2280,2310,
                      2340,2350,2020,2120,2130,2180,2360,2370,2460,2470,2270,2060,2070,9999)
names(Country.code.vec) <- Country.name.vec

cause.code.vec <- 1:15
cause.name.vec <- c('Total', 'Infectious','Neoplasms', 'Circulatory','Abnormal', 'Mental',
                    'Nervous','Endocrine','Digestive',
                    'Genitourinary','Perinatal','Respiratory','External','Homicide','Rest')

names(cause.code.vec) <- cause.name.vec
