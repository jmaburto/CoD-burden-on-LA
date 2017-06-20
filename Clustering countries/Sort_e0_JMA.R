library(dplyr)

setwd( "C:/Users/jmaburto/Documents/GitHub Backup 2/CoD-burden-on-LA/R/ex_App/")
load('Life_expectancy.RData')
ex$Country[ex$Country=="Colombia"]<-"COLOMBIA"
ex$Country[ex$Country=="BOLIVARIAN REPUBLIC OF VENEZUELA"]<-"VENEZUELA"
sort(unique(ex$Year))


D1990<-ex[ex$Year<2000 & (ex$Source == 'UN' | ex$Source == 'CEPAL2010'),]
D2000<-ex[(ex$Year<2010) & (ex$Year>1999) & (ex$Source == 'UN' | ex$Source == 'CEPAL2010'),]
D2010<-ex[ex$Year>2009 & (ex$Source == 'UN' | ex$Source == 'CEPAL2010'),]

unique(D1990$Source)
unique(D2000$Source)
unique(D2010$Source)


D1990f <- D1990 %>% 
  filter(Sex == "Females") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


D1990m <- D1990 %>% 
  filter(Sex == "Males") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


D2000f <- D2000 %>% 
  filter(Sex == "Females") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


D2000m <- D2000 %>% 
  filter(Sex == "Males") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


D2010f <- D2010 %>% 
  filter(Sex == "Females") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


D2010m <- D2010 %>% 
  filter(Sex == "Males") %>%
  group_by(Country,Sex) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(-MeanC)


cbind(D1990f$Country,D2000f$Country,D2010f$Country)
cbind(D1990m$Country,D2000m$Country,D2010m$Country)


round(cbind(D1990f$MeanC,D2000f$MeanC,D2010f$MeanC),2)
round(cbind(D1990m$MeanC,D2000m$MeanC,D2010m$MeanC),2)