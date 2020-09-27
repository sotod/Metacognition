rm(list=ls())
setwd('~/Downloads/')
#library(readxl) # THIS LIBRARY IS TO BE LOADED FOR OPENING EXCEL files only 
#datos<-read_excel('Sub13.xlsx') #READING EXCEL files only 

datos<-read.csv('PT6-4.csv') 

# tgtfield
# TiltResponse.RESP
# TiltResponse.CRESP
# ConfResponse.RESP

datos$tgtori <-as.character(datos$tgtori) 
datos$tgtori[datos$tgtori=='left'] <- 1
datos$tgtori[datos$tgtori=='right'] <- 2
datos$tgtori <-as.numeric(datos$tgtori)

datos$TiltResponse.RESP <-as.character(datos$TiltResponse.RESP) 
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='n'] <- 1
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='m'] <- 2
datos$TiltResponse.RESP <-as.numeric(datos$TiltResponse.RESP)

datos$TiltResponse.CRESP <-as.character(datos$TiltResponse.CRESP) 
datos$TiltResponse.CRESP[datos$TiltResponse.CRESP=='n'] <- 1
datos$TiltResponse.CRESP[datos$TiltResponse.CRESP=='m'] <- 2
datos$TiltResponse.CRESP <-as.numeric(datos$TiltResponse.CRESP)



datos$TiltResponse.RESP <-as.character(datos$TiltResponse.RESP) 
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='n'] <- 1
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='m'] <- 2
datos$TiltResponse.RESP <-as.numeric(datos$TiltResponse.RESP)
#SELECT YOUR CONDITION TO COMPUTE SENSITIVITY
# this can be tgtfield=="left" or tgtfield=="right" for the patients study
# which is changed below where it says datos$tgtfield=="left"
# E.g. if you changed datos$tgtfield to =="righ" then the code will print sensitivity
# for the condition in which targets where presented on the right
datos<-subset(datos, datos$tgtfield=="right", select=c(tgtori, TiltResponse.RESP, TiltResponse.CRESP, ConfResponse.RESP))

# if this is a reward experiment then put a # above just before datos and work 
# with the next line (but then remove the # below first)
# change datos$reward== to "low" "medium" or "high" depending on the condition you like to assess
#datos<-subset(datos, datos$reward=="high", select=c(tgtori, TiltResponse.RESP, TiltResponse.CRESP, ConfResponse.RESP))




datos$typ1<-ifelse(datos$TiltResponse.CRESP=="1" & datos$TiltResponse.RESP=="1", 1,ifelse(datos$TiltResponse.CRESP=="2" & datos$TiltResponse.RESP=="1", 2, ifelse(datos$TiltResponse.CRESP=="1" & datos$TiltResponse.RESP=="2", 3, ifelse(datos$TiltResponse.CRESP=="2" & datos$TiltResponse.RESP=="2", 4, 0)))) 
datos$typ2<-ifelse(datos$ConfResponse.RESP=="1", 1,ifelse(datos$ConfResponse.RESP=="2", 2, ifelse(datos$ConfResponse.RESP=="3", 3,ifelse(datos$ConfResponse.RESP=="4", 4,ifelse(datos$ConfResponse.RESP=="5", 5, ifelse(datos$ConfResponse.RESP=="6", 6, 0)))))) 

aroc2 <- matrix(NA, ncol=1, nrow=1) 

nrats=4 #there are 6 conf ratings Ps should use the full scale
k = nrats+1 
h2=0
fa2=0
m2=0
cr2=0
ka=0
kb=0
# getting vectors 
for(con in 1:nrats) { 
  h2[k-1]=table(datos$typ2==con & (datos$typ1==1 | datos$typ1==4))["TRUE"] # TRUE only selects cells meeting the RULE
  fa2[k-1]=table(datos$typ2==con & (datos$typ1==2 | datos$typ1==3))["TRUE"]   
  h2[is.na(h2)] <- 0
  fa2[is.na(fa2)] <- 0
  k=k-1
}
h2=h2+0.5 # add 0.5 BECAUSE??????????
fa2=fa2+0.5
# getting cumul probs
h2=(h2/sum(h2))
fa2=(fa2/sum(fa2)) 
# add 0 to vector 
h2=c(0, h2)
fa2=c(0, fa2)

cumh2=cumsum(h2)
cumfa2=cumsum(fa2) 

# Aroc and Bk from area under curve - see Kornbrot pg 398
fin=floor(nrats/2)#round to lowest
ka<-rep()#pre-allocate vble
kb<-rep()#pre-allocate vble

j=1
for (n in 1:fin) {
  ka[j] <- (cumh2[n+1] - cumfa2[n])^2 - (cumh2[n] - cumfa2[n+1])^2 
  j=j+1
}
j=1
fin=fin+1
for (n in fin:nrats) {
  kb[j] <- (cumh2[n+1] - cumfa2[n])^2 - (cumh2[n] - cumfa2[n+1])^2 
  j=j+1
}

aroc2 = 0.5 + (0.25*sum(ka)) + (0.25*sum(kb))

broc = log((0.25*sum(ka))/(0.25*sum(kb)));

aroc2<-as.numeric(aroc2) 

print(aroc2)

print(broc)
 

