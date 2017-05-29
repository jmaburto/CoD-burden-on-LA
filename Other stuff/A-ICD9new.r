########################################################################
## Vladimir Canudas-Romo
## Demography 
## Sample program for reading data from WHO mortality database ICD9 
## and making it fit in the same format for all countries
########################################################################
                                                                               
## laptop

folder<-"C:/Users/vcanudas/DATA/WHOhmd/ICD9"
  DAT<-"C:/Users/vcanudas/DATA/WHOhmd/DataCoD"
  CHEK<-"C:/Users/vcanudas/DATA/HMD/USA/STATS"
setwd(DAT)

setwd("C:/Users/jmaburto/Desktop/World Bank 2017/")


## WHO codes B


C<-read.csv("Availability.csv",header=TRUE,sep=";",fill=TRUE,skip=0)
C<-C[C$Icd=="Icd9",]
B<-names(table(C$Country))


AA2<-read.csv("MortIcd9.csv",header=TRUE,sep=";",fill=TRUE,skip=0)
AA<-AA2[(AA2$Country>1999)&(AA2$Country<3000)&(AA2$Year>1989),]
aa<-names(table(AA$Country))
                                                            
## SDC
setwd(folder) 


#########################################
for( i in which((B%in%aa)) ){

if(B[i]%in%aa){D<-AA[(AA$Country==B[i]),]}


D<-AA[(AA$Country==B[i]),]


MT1<-c()
MT2<-c()

AL<-D

Years<-as.numeric(names(table(AL$Year)))

for (t in 1:length(Years)){

A<-AL[AL$Year==Years[t],]






#################################



TT1<-A[(A$Cause=="B00")&(A$Sex==1),]
TT2<-A[(A$Cause=="B00")&(A$Sex==2),] 
TT1<-TT1[1:35] 
TT2<-TT2[1:35]



Infec<-c(paste("B0",c(1:7),sep=""))

INF1<-A[(A$Cause%in%Infec)&(A$Sex==1),]
INF2<-A[(A$Cause%in%Infec)&(A$Sex==2),]
INF1<-t(as.array(c(INF1[1,1:8],colSums(INF1[,9:35]))))
INF2<-t(as.array(c(INF2[1,1:8],colSums(INF2[,9:35]))))


Neop<-c(paste("B0",c(8:9),sep=""),paste("B",c(10:17),sep=""))
NEO1<-A[(A$Cause%in%Neop)&(A$Sex==1),]
NEO2<-A[(A$Cause%in%Neop)&(A$Sex==2),]
NEO1<-t(as.matrix(c(NEO1[1,1:8],colSums(NEO1[,9:35])),1))
NEO2<-t(as.matrix(c(NEO2[1,1:8],colSums(NEO2[,9:35])),1))


Cardio<-c(paste("B",c(25:30),sep=""))

CARD1<-A[(A$Cause%in%Cardio)&(A$Sex==1),]
CARD2<-A[(A$Cause%in%Cardio)&(A$Sex==2),]
CARD1<-t(as.matrix(c(CARD1[1,1:8],colSums(CARD1[,9:35])),1))
CARD2<-t(as.matrix(c(CARD2[1,1:8],colSums(CARD2[,9:35])),1))



## Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
sym<-c("B46")

SYM1<-A[(A$Cause%in%sym)&(A$Sex==1),]
SYM2<-A[(A$Cause%in%sym)&(A$Sex==2),]
SYM1<-t(as.matrix(c(SYM1[1,1:8],colSums(SYM1[,9:35])),1))
SYM2<-t(as.matrix(c(SYM2[1,1:8],colSums(SYM2[,9:35])),1))



## Endocrine, nutritional and metabolic diseases 	
dia<-c(paste("B",c(18:19),sep=""))

DIA1<-A[(A$Cause%in%dia)&(A$Sex==1),]
DIA2<-A[(A$Cause%in%dia)&(A$Sex==2),]
DIA1<-t(as.matrix(c(DIA1[1,1:8],colSums(DIA1[,9:35])),1))
DIA2<-t(as.matrix(c(DIA2[1,1:8],colSums(DIA2[,9:35])),1))



## Diseases of the respiratory system	
Respi<-c(paste("B",c(31:32),sep=""))

RES1<-A[(A$Cause%in%Respi)&(A$Sex==1),]
RES2<-A[(A$Cause%in%Respi)&(A$Sex==2),]
RES1<-t(as.matrix(c(RES1[1,1:8],colSums(RES1[,9:35])),1))
RES2<-t(as.matrix(c(RES2[1,1:8],colSums(RES2[,9:35])),1))


## External causes of morbidity and mortality	- homicides
Viole<-c(paste("B",c(47:54,56),sep=""))

VIOL1<-A[(A$Cause%in%Viole)&(A$Sex==1),]
VIOL2<-A[(A$Cause%in%Viole)&(A$Sex==2),]
VIOL1<-t(as.matrix(c(VIOL1[1,1:8],colSums(VIOL1[,9:35])),1))
VIOL2<-t(as.matrix(c(VIOL2[1,1:8],colSums(VIOL2[,9:35])),1))


## Assault - homicide
hom<-c("B55")

HOM1<-A[(A$Cause%in%hom)&(A$Sex==1),]
HOM2<-A[(A$Cause%in%hom)&(A$Sex==2),]
HOM1<-t(as.matrix(c(HOM1[1,1:8],colSums(HOM1[,9:35])),1))
HOM2<-t(as.matrix(c(HOM2[1,1:8],colSums(HOM2[,9:35])),1))



###############################






## Mental and behavioural disorders 
men<-c("B21")

MENT1<-A[(A$Cause%in%men)&(A$Sex==1),]
MENT2<-A[(A$Cause%in%men)&(A$Sex==2),]
MENT1<-t(as.matrix(c(MENT1[1,1:8],colSums(MENT1[,9:35])),1))
MENT2<-t(as.matrix(c(MENT2[1,1:8],colSums(MENT2[,9:35])),1))


## Diseases of the nervous system	
alzh<-c(paste("B",c(22:24),sep=""))

ALZH1<-A[(A$Cause%in%alzh)&(A$Sex==1),]
ALZH2<-A[(A$Cause%in%alzh)&(A$Sex==2),]
ALZH1<-t(as.matrix(c(ALZH1[1,1:8],colSums(ALZH1[,9:35])),1))
ALZH2<-t(as.matrix(c(ALZH2[1,1:8],colSums(ALZH2[,9:35])),1))


## Diseases of the digestive system 
dig<-c(paste("B",c(33:34),sep=""))

DIG1<-A[(A$Cause%in%dig)&(A$Sex==1),]
DIG2<-A[(A$Cause%in%dig)&(A$Sex==2),]
DIG1<-t(as.matrix(c(DIG1[1,1:8],colSums(DIG1[,9:35])),1))
DIG2<-t(as.matrix(c(DIG2[1,1:8],colSums(DIG2[,9:35])),1))


## Diseases of the genitourinary system	
genit<-c(paste("B",c(35:37),sep=""))

GENIT1<-A[(A$Cause%in%genit)&(A$Sex==1),]
GENIT2<-A[(A$Cause%in%genit)&(A$Sex==2),]
GENIT1<-t(as.matrix(c(GENIT1[1,1:8],colSums(GENIT1[,9:35])),1))
GENIT2<-t(as.matrix(c(GENIT2[1,1:8],colSums(GENIT2[,9:35])),1))


# P00-P96	Certain conditions originating in the perinatal period
# Q00-Q99	Congenital malformations, deformations and chromosomal abnormalities
                            
congenit<-c(paste("B",c(44:45),sep=""))

CONGENIT1<-A[(A$Cause%in%congenit)&(A$Sex==1),]
CONGENIT2<-A[(A$Cause%in%congenit)&(A$Sex==2),]
CONGENIT1<-t(as.matrix(c(CONGENIT1[1,1:8],colSums(CONGENIT1[,9:35])),1))
CONGENIT2<-t(as.matrix(c(CONGENIT2[1,1:8],colSums(CONGENIT2[,9:35])),1))




MT1<-rbind(MT1,TT1,INF1,NEO1,CARD1,SYM1,MENT1,ALZH1,DIA1,DIG1,GENIT1,CONGENIT1,RES1,VIOL1,HOM1)
MT2<-rbind(MT2,TT2,INF2,NEO2,CARD2,SYM2,MENT2,ALZH2,DIA2,DIG2,GENIT2,CONGENIT2,RES2,VIOL2,HOM2)

}


## now changing to the format of the Berkeley Mortality Database
# Year cause age female male total

AM<-MT1
AF<-MT2

## Number of Causes of death including Total:
M<-14

N<-dim(AM)[1]
Age<-c("Total",0,1,2,3,4,paste(seq(5,90,by=5),"-",seq(9,94,by=5)),"95+","UNK")
Age<-rep(rep(Age,M),N/M)
Zy<-AF$Year
#if(i==16){Zy<-unlist(AF$Year)}
Year<-rep(Zy,rep(26,N))
Cause<-rep(rep(c(1:M),rep(26,M)),N/M)

DF<-c()
DM<-c()

for (x in 1:N){
Zm<-AM[x,10:35]
#if(i==16){Zm<-unlist(AM[x,10:35])}

Zf<-AF[x,10:35]
#if(i==16){Zf<-unlist(AF[x,10:35])}

DM<-c(DM,t(Zm))
DF<-c(DF,t(Zf))
}

DM<-ifelse(is.na(DM),0,DM)
DF<-ifelse(is.na(DF),0,DF)
Year<-ifelse(is.na(Year),0,Year)


Male<-DM
Female<-DF

ALL2<-cbind(Year,Cause,Age,Female,Male)
dimnames(ALL2)[[1]]<-rep(B[i],dim(ALL2)[1])


Name1<-paste("ICD9-",B[i],".txt",sep="")
write.table(as.matrix(ALL2),Name1,sep=",",col.names=NA)
}









#########################################



Format<-c()

for( i in which(B%in%aa)){

D<-AA[(AA$Country==B[i]),]


MT1<-c()
MT2<-c()

AL<-D

Years<-as.numeric(names(table(AL$Year)))

for (t in 1:length(Years)){

A<-AL[AL$Year==Years[t],]

## ALL deaths
Frmat<-A$Frmat[1] 

newF<-c(B[i],Years[t],Frmat)

Format<-rbind(Format,newF)
}
}




write.table(Format,"FormatData.txt",sep=",",col.names=NA)

getwd()

####    comparison with HMD
setwd(folder)
USA<-read.table("ICD9-2450.txt",header=T,sep = ",")

USAT<-USA[(USA$Age=="Total")&(USA$Cause==1),]

setwd(CHEK)
DEA<-read.table("Deaths_1x1.txt",header=TRUE,fill=TRUE,skip=2)

DEA2<-cbind(c(min(DEA$Year):max(DEA$Year)),colSums(matrix(DEA[,3],111)),colSums(matrix(DEA[,4],111)))[58:66,]

USAT[,5:6]-DEA2[,2:3]
### totals match nicely!!!
