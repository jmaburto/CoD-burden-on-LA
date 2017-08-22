library(dplyr)
library(ggplot2)
library(grid)

setwd("C:/Users/jmaburto/Desktop/Canudas-Romo,Aburto_2017/R/Letter_app")


setwd(Data)


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
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(MeanC)


D1990m <- D1990 %>% 
  filter(Sex == "Males") %>%
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(MeanC)


D2000f <- D2000 %>% 
  filter(Sex == "Females") %>%
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(MeanC)


D2000m <- D2000 %>% 
  filter(Sex == "Males") %>%
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(MeanC)


D2010f <- D2010 %>% 
  filter(Sex == "Females") %>%
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  mutate(e0Dif=cut(max(MeanC)-MeanC,c(-0.5,4,6,8,20),labels=c("H","MH","ML","L")))  %>%
  arrange(MeanC)


D2010m <- D2010 %>% 
  filter(Sex == "Males") %>%
  group_by(Country) %>%
  summarise(N.obs = n(), MeanC = mean(ex, na.rm=TRUE)) %>%
  arrange(MeanC)



#7,12,21
Country<-D2010f$Country
  c("COSTA RICA","CHILE","CUBA","PUERTO RICO","PANAMA","URUGUAY",
"JAMAICA","ECUADOR","ARGENTINA","MEXICO","LATIN AMERICA",
"VENEZUELA","PERU","NICARAGUA","BRAZIL","HONDURAS","COLOMBIA","DOMINICAN REPUBLIC","PARAGUAY","EL SALVADOR",
"GUATEMALA","TRINIDAD AND TOBAGO","BOLIVIA","HAITI")

Gr<-D2010f$e0Dif
  #c(rep("A",7),rep("B",4),rep("C",9),rep("D",4))

Sex<-rep(c("Female","Male"),each=length(Country))

Groups<-data.frame(Country,Gr)

Df <- left_join(D1990f,D2010f,by="Country")  

Dm <- left_join(D1990m,D2010m,by="Country")  

D1 <-  left_join(Df,Groups,by="Country") %>% 
  select(Country,MeanC.x,MeanC.y,Gr)

D2 <-  left_join(Dm,Groups,by="Country") %>% 
  select(Country,MeanC.x,MeanC.y,Gr)


D<- data.frame(cbind(rbind(D1,D2),Sex))

#D$`Country` <- factor(D$`Country`, levels = D$`Country`) 



  
  ggplot() +
  geom_point(data=D1,aes(x=reorder(Country,MeanC.y), y=MeanC.y), alpha=I(0.01))+
  geom_point(data=D,aes(x=reorder(Country,MeanC.y), y=MeanC.y,col=Gr, shape=Sex),size=2,position=position_dodge(width = 0.6)) +
  geom_errorbar(data=D,aes(x = reorder(Country,MeanC.y), ymin = MeanC.y,
                           ymax = MeanC.x,col=Gr, shape=Sex),position=position_dodge(width = 0.6), width=0,size=1)+
  scale_x_discrete(name="Countries")+
  scale_y_continuous(name="Life expectancy at birth", limits=c(50,85))+
  scale_color_discrete(name="Life expectancy at birth",labels=c("High","Medium high","Medium low","Low"))+
  scale_shape_manual(values = c(8, 16))+
  theme(legend.key=element_blank())+
  theme_bw()+
  theme(legend.key = element_rect(colour = NA))+
  theme(legend.title=element_blank(), legend.position = "bottom") +
  theme(legend.text = element_text(size = 10))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  theme(plot.margin=unit(c(1,0.5,1,0.5), "cm"))+
    geom_text(aes(x=23, y=50, label="1990"))+
    geom_text(aes(x=23, y=55, label="2014"))+
    annotate("segment", x = 22, xend = 22, y = 51, 
             yend = 55, colour="black", size=1, 
             arrow=arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
    coord_flip()
  
  
  
  
  
  ggplot() +
    geom_point(data=D1,aes(x=reorder(Country,MeanC.y), y=MeanC.y), alpha=I(0.01))+
    geom_point(data=D,aes(x=reorder(Country,MeanC.y), y=MeanC.y,col=Gr, shape=Sex),size=2,position=position_dodge(width = 0.6)) +
    geom_errorbar(data=D,aes(x = reorder(Country,MeanC.y), ymin = MeanC.y,
                            ymax = MeanC.x,col=Gr, shape=Sex),position=position_dodge(width = 0.6), width=0,size=0.5)+
    scale_x_discrete(name="Countries")+
    scale_y_continuous(name="Life expectancy at birth", limits=c(50,85))+
    scale_color_discrete(name="Life expectancy at birth",labels=c("High","Medium high","Medium low","Low"))+
    scale_shape_manual(values = c(8, 16))+
    theme_bw()+
  theme(legend.position = c(.05, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "left")+
    theme(plot.margin=unit(c(1,0.5,1,0.5), "cm"))+
    geom_text(aes(x=13.7, y=51, label="1990"))+
    geom_text(aes(x=13.7, y=55, label="2014"))+
    annotate("segment", x = 13, xend = 13, y = 51, 
             yend = 55, colour="black", size=0.5, 
             arrow=arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
      coord_flip()
  
  save(D,D1,file = 'ex_fig_VCR.RData')
  setwd(Folder)
  Nm<-"e0.ps"
  dev.copy2eps(file=Nm)      
  
  