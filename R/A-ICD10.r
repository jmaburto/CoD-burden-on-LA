########################################################################
## Vladimir Canudas-Romo
## Demography 
## Sample program for reading data from WHO mortality database ICD10 
## and making it fit in the same format for all countries
########################################################################                                                   

## laptop

folder<-"C:/Users/vcanudas/DATA/WHOhmd/ICD10"
DAT<-"C:/Users/vcanudas/DATA/WHOhmd/DataCoD"
CHEK<-"C:/Users/vcanudas/DATA/HMD/USA/STATS"
setwd(DAT)

## WHO codes B


C<-read.csv("Availability.csv",header=TRUE,sep=";",fill=TRUE,skip=0)
C<-C[C$Icd=="Icd10",]
B<-names(table(C$Country))

AA<-read.csv("Morticd10_p1.csv",header=TRUE,sep=";",fill=TRUE,skip=0)

aa<-names(table(AA$Country))

AA2<-read.csv("Morticd10_p2.csv",header=TRUE,sep=";",fill=TRUE,skip=0)

aa2<-names(table(AA2$Country))

                                                            
## KU
setwd(folder) 



#########################################
for( i in which((B%in%aa)|(B%in%aa2)) ){
## to check USA = 2450
  
D<-rbind(AA[(AA$Country==B[i]),],AA2[(AA2$Country==B[i]),])

MT1<-c()
MT2<-c()

AL<-D

Years<-as.numeric(names(table(AL$Year)))

for (t in 1:length(Years)){

A<-AL[AL$Year==Years[t],]

## ALL deaths
TT<- c("AAA",1000,"CH00")

TT1<-A[(A$Cause%in%TT)&(A$Sex==1),]  
TT1<-TT1[1:35]
TT2<-A[(A$Cause%in%TT)&(A$Sex==2),]
TT2<-TT2[1:35]


## Certain infectious and parasitic diseases		A00-B99
Infec<-paste("A",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep="")
Infec<-c(1001,"CH01",Infec,paste("B",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

INF1<-A[(A$Cause%in%Infec)&(A$Sex==1),]
INF2<-A[(A$Cause%in%Infec)&(A$Sex==2),]
INF1<-t(as.array(c(INF1[1,1:8],colSums(INF1[,9:35]))))
INF2<-t(as.array(c(INF2[1,1:8],colSums(INF2[,9:35]))))


## Neoplasms				C00-D48
Neop<-paste("C",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep="")
Neop<-c(1026,"CH02",Neop,paste("D",c("00","000",1:48,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:489)),sep=""))

NEO1<-A[(A$Cause%in%Neop)&(A$Sex==1),]
NEO2<-A[(A$Cause%in%Neop)&(A$Sex==2),]
NEO1<-t(as.matrix(c(NEO1[1,1:8],colSums(NEO1[,9:35])),1))
NEO2<-t(as.matrix(c(NEO2[1,1:8],colSums(NEO2[,9:35])),1))


## Diseases of the circulatory system		I00-I99
Cardio<-c(1064,"CH09",paste("I",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

CARD1<-A[(A$Cause%in%Cardio)&(A$Sex==1),]
CARD2<-A[(A$Cause%in%Cardio)&(A$Sex==2),]
CARD1<-t(as.matrix(c(CARD1[1,1:8],colSums(CARD1[,9:35])),1))
CARD2<-t(as.matrix(c(CARD2[1,1:8],colSums(CARD2[,9:35])),1))




## Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified
##						R00-R99
sym<-c(1097,"CH18",paste("R",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

SYM1<-A[(A$Cause%in%sym)&(A$Sex==1),]
SYM2<-A[(A$Cause%in%sym)&(A$Sex==2),]
SYM1<-t(as.matrix(c(SYM1[1,1:8],colSums(SYM1[,9:35])),1))
SYM2<-t(as.matrix(c(SYM2[1,1:8],colSums(SYM2[,9:35])),1))


## Mental and behavioural disorders F01-F99
men<-c(1055,"CH05",paste("F",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

MENT1<-A[(A$Cause%in%men)&(A$Sex==1),]
MENT2<-A[(A$Cause%in%men)&(A$Sex==2),]
MENT1<-t(as.matrix(c(MENT1[1,1:8],colSums(MENT1[,9:35])),1))
MENT2<-t(as.matrix(c(MENT2[1,1:8],colSums(MENT2[,9:35])),1))


## Diseases of the nervous system	G00-G98

alzh<-c(1058,"CH06",paste("G",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

ALZH1<-A[(A$Cause%in%alzh)&(A$Sex==1),]
ALZH2<-A[(A$Cause%in%alzh)&(A$Sex==2),]
ALZH1<-t(as.matrix(c(ALZH1[1,1:8],colSums(ALZH1[,9:35])),1))
ALZH2<-t(as.matrix(c(ALZH2[1,1:8],colSums(ALZH2[,9:35])),1))




## Endocrine, nutritional and metabolic diseases 	E00-E88 

dia<-c(1051,"CH04",paste("E",c("00","000",1:88,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:889)),sep=""))

DIA1<-A[(A$Cause%in%dia)&(A$Sex==1),]
DIA2<-A[(A$Cause%in%dia)&(A$Sex==2),]
DIA1<-t(as.matrix(c(DIA1[1,1:8],colSums(DIA1[,9:35])),1))
DIA2<-t(as.matrix(c(DIA2[1,1:8],colSums(DIA2[,9:35])),1))


## Diseases of the digestive system K00-K92

dig<-c(1078,"CH11",paste("K",c("00","000",1:92,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:92),sep=""),100:929)),sep=""))

DIG1<-A[(A$Cause%in%dig)&(A$Sex==1),]
DIG2<-A[(A$Cause%in%dig)&(A$Sex==2),]
DIG1<-t(as.matrix(c(DIG1[1,1:8],colSums(DIG1[,9:35])),1))
DIG2<-t(as.matrix(c(DIG2[1,1:8],colSums(DIG2[,9:35])),1))




## Diseases of the genitourinary system	N00-N98
genit<-c(1084,"CH14",paste("N",c("00","000",1:98,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:98),sep=""),100:989)),sep=""))

GENIT1<-A[(A$Cause%in%genit)&(A$Sex==1),]
GENIT2<-A[(A$Cause%in%genit)&(A$Sex==2),]
GENIT1<-t(as.matrix(c(GENIT1[1,1:8],colSums(GENIT1[,9:35])),1))
GENIT2<-t(as.matrix(c(GENIT2[1,1:8],colSums(GENIT2[,9:35])),1))


# P00-P96	Certain conditions originating in the perinatal period
# Q00-Q99	Congenital malformations, deformations and chromosomal abnormalities
                            
congenit<-c(1092,1093,"CH16","CH17",paste("P",c("00","000",1:96,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:96),sep=""),100:969)),sep=""))
congenit<-c(congenit,paste("Q",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))

CONGENIT1<-A[(A$Cause%in%congenit)&(A$Sex==1),]
CONGENIT2<-A[(A$Cause%in%congenit)&(A$Sex==2),]
CONGENIT1<-t(as.matrix(c(CONGENIT1[1,1:8],colSums(CONGENIT1[,9:35])),1))
CONGENIT2<-t(as.matrix(c(CONGENIT2[1,1:8],colSums(CONGENIT2[,9:35])),1))

## Diseases of the respiratory system	J00-J98
Respi<-c(1072,"CH10",paste("J",c("00","000",1:98,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:98),sep=""),100:989)),sep=""))

RES1<-A[(A$Cause%in%Respi)&(A$Sex==1),]
RES2<-A[(A$Cause%in%Respi)&(A$Sex==2),]
RES1<-t(as.matrix(c(RES1[1,1:8],colSums(RES1[,9:35])),1))
RES2<-t(as.matrix(c(RES2[1,1:8],colSums(RES2[,9:35])),1))




## External causes of morbidity and mortality	V01-Y89 minus homicide
Viole<-paste("V",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep="")
Viole<-c(Viole,paste("W",c("00","000",1:99,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:999)),sep=""))
Viole<-c(Viole,paste("X",c("00","000",1:84,paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""),100:849)),sep=""))
Viole<-c(1095,"CH20",Viole,paste("Y",c("00","000",1:89,100:899),sep=""))

VIOL1<-A[(A$Cause%in%Viole)&(A$Sex==1),]
VIOL2<-A[(A$Cause%in%Viole)&(A$Sex==2),]
VIOL1<-t(as.matrix(c(VIOL1[1,1:8],colSums(VIOL1[,9:35])),1))
VIOL2<-t(as.matrix(c(VIOL2[1,1:8],colSums(VIOL2[,9:35])),1))

## X85-Y09 Assault - homicide
hom<-c(paste("X",c(85:99,850:999),sep=""))
hom<-c(hom,paste("Y",c("00","000",paste("0",1:9,sep=""),c(paste("0",c(paste("0",1:9,sep=""),10:99),sep=""))),sep=""))

HOM1<-A[(A$Cause%in%hom)&(A$Sex==1),]
HOM2<-A[(A$Cause%in%hom)&(A$Sex==2),]
HOM1<-t(as.matrix(c(HOM1[1,1:8],colSums(HOM1[,9:35])),1))
HOM2<-t(as.matrix(c(HOM2[1,1:8],colSums(HOM2[,9:35])),1))

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


Name1<-paste("ICD10-",B[i],".txt",sep="")
write.table(as.matrix(ALL2),Name1,sep=",",col.names=NA)
}







#########################################
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



for( i in which(B%in%aa2)){

D<-AA2[(AA2$Country==B[i]),]


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




####    comparison with HMD
setwd(folder)    
USA<-read.table("ICD10-2450.txt",header=T,sep = ",")

USAT<-USA[(USA$Age=="Total")&(USA$Cause==1),]

setwd(CHEK)
DEA<-read.table("Deaths_1x1.txt",header=TRUE,fill=TRUE,skip=2)

DEA2<-cbind(c(min(DEA$Year):max(DEA$Year)),colSums(matrix(DEA[,3],111)),colSums(matrix(DEA[,4],111)))[67:81,]

USAT[-16,5:6]-DEA2[,2:3]
### totals don't  match perfectly but good enough!!!


setwd(DAT)
USACoD<-read.csv("USAwonder.csv",header=T,sep = ";")
USACoD2<-read.csv("USAwonder2.csv",header=T,sep = ";")
y<-2010
USACoDY<-USACoD[USACoD$Year==y,]
USACoDY2<-USACoD2[USACoD2$Year==y,]
USAy2f<-USACoDY[c(1,2,6,12,4,5,3,8,9,10,11,7,13),]
USAy2m<-USACoDY[c(1,2,6,12,4,5,3,8,9,10,11,7,13)+14,]

## WHO
## TT2 1, INF2 2,NEO2 3,CARD2 4,SYM2 5,MENT2 6,ALZH2 7,
## DIA2 8,DIG2 9,GENIT2 10,CONGENIT2 11,RES2 12,VIOL2 13)
## Wonder
## A00-B99 2, C00-D48 3, E00-E88 8, F01-F99 6, G00-G98 7, 
## I00-I99 4, J00-J98 12, K00-K92 9, N00-N98 10, P00-P96 11, 
## Q00-Q99 11, R00-R99 5, V01-Y89 13

USAy<-USA[(USA$Age=="Total")&(USA$Year==y),]
F<-c(USAy2f[1:9,5],sum(USAy2f[10:11,5]),USAy2f[12:13,5],USACoDY2[1,5])
M<-c(USAy2m[1:9,5],sum(USAy2m[10:11,5]),USAy2m[12:13,5],USACoDY2[4,5])
N<-c(as.character(USAy2f[-10,3]),"homicide")
cbind(N,USAy[-1,5]-F+c(rep(0,11),USACoDY2[1,5],0),USAy[-1,6]-M+c(rep(0,11),USACoDY2[4,5],0))
