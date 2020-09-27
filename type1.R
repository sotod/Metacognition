rm(list=ls())
setwd('~/Downloads/')
datos<-read.csv('PT6-4.csv') 

# datos$typ1 - type I response code. 1 = type 1 hit (correct), 2 = type 1 FA (incorrect), % 3 - type 1 miss (incorrect), 4 = type 1 correct rejection (correct)
# datos$typ1 - vector of confidence ratings from 1:Nratings
#  Nratings - how many confidence levels available for use (if Nratings is
#  an odd number, Aroc works ok but Broc is biased)

# RELEVANT VARIABLES ARE
# tgtfield
# TiltResponse.RESP
# TiltResponse.CRESP
# ConfResponse.RESP

#word stimulius
datos$tgtori <-as.character(datos$tgtori) 
datos$tgtori[datos$tgtori=='left'] <- 1
datos$tgtori[datos$tgtori=='right'] <- 2
datos$tgtori <-as.numeric(datos$tgtori)

#P response to word
datos$TiltResponse.RESP <-as.character(datos$TiltResponse.RESP) 
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='n'] <- 1
datos$TiltResponse.RESP[datos$TiltResponse.RESP=='m'] <- 2
datos$TiltResponse.RESP <-as.numeric(datos$TiltResponse.RESP)

# Expected Correct response to word
datos$TiltResponse.CRESP <-as.character(datos$TiltResponse.CRESP) 
datos$TiltResponse.CRESP[datos$TiltResponse.CRESP=='n'] <- 1
datos$TiltResponse.CRESP[datos$TiltResponse.CRESP=='m'] <- 2
datos$TiltResponse.CRESP <-as.numeric(datos$TiltResponse.CRESP)

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

# 1 = type 1 hit (correct), 2 = type 1 FA (incorrect), % 3 - type 1 miss (incorrect), 4 = type 1 correct rejection (correct)
# datos$typ1 - vector of confidence ratings from 1:Nratings
datos$typ1<-ifelse(datos$TiltResponse.CRESP=="1" & datos$TiltResponse.RESP=="1", 1,ifelse(datos$TiltResponse.CRESP=="2" & datos$TiltResponse.RESP=="1", 2, ifelse(datos$TiltResponse.CRESP=="1" & datos$TiltResponse.RESP=="2", 3, ifelse(datos$TiltResponse.CRESP=="2" & datos$TiltResponse.RESP=="2", 4, 0)))) 
datos$typ2<-ifelse(datos$ConfResponse.RESP=="1", 1,ifelse(datos$ConfResponse.RESP=="2", 2, ifelse(datos$ConfResponse.RESP=="3", 3,ifelse(datos$ConfResponse.RESP=="4", 4,ifelse(datos$ConfResponse.RESP=="5", 5, ifelse(datos$ConfResponse.RESP=="6", 6, 0)))))) 
                                                                                    
aroc1 <- matrix(NA, ncol=1, nrow=1) 

h<-rep()  # pre-allocate vble
fa<-rep() # pre-allocate vble
m<-rep()  # pre-allocate vble
cr<-rep() # pre-allocate vble

nrats = 4 # there are 6 conf ratings Ps should use the full scale
k = nrats+1 
h=0
fa=0
m=0
cr=0
ka=0
kb=0
j=1

# getting vectors  
for(con in 1:nrats) { 
h[k-1]=table(datos$typ2==con & datos$typ1==1)["TRUE"] # TRUE only selects cells meeting the RULE
fa[k-1]=table(datos$typ2==con & datos$typ1==2)["TRUE"]  
m[j]=table(datos$typ2==con & datos$typ1==3)["TRUE"]   
cr[j]=table(datos$typ2==con & datos$typ1==4)["TRUE"]  
h[is.na(h)] <- 0
fa[is.na(fa)] <- 0
m[is.na(m)] <- 0
cr[is.na(cr)] <- 0
k=k-1
j=j+1
}
  
signal=sum(h)+sum(m) # number of signal present trials
noise=sum(fa)+sum(cr)# number of noise present trials
  
h1<-rep()#pre-allocate vble 
fa1<-rep()#pre-allocate vble 
h1<-rep()#pre-allocate vble 
fa1<-rep()#pre-allocate vble 
  
h1[1]=0
fa1[1]=0
  
# getting cumul probs
h1[2:5]=h/signal  #change according to number of ratings i.e. 4r: then 2:5, 3r: then 2:4
h1[6:9]=m/signal #change according to number of ratings i.e. 4r: then 6-9, 3r: then 5:8
fa1[2:5]=fa/noise
fa1[6:9]=cr/noise

h1=(h1/sum(h1))
fa1=(fa1/sum(fa1))
cumh1=cumsum(h1)
cumfa1=cumsum(fa1)
  
# Aroc and Bk from area under curve - see Kornbrot pg 398
ka<-rep()#pre-allocate vble
kb<-rep()#pre-allocate vble

j=1
for (n in 1:nrats) {
ka[j] <- (cumh1[n+1] - cumfa1[n])^2 - (cumh1[n] - cumfa1[n+1])^2
j=j+1
}
  
j=1
for (n in (nrats+1):(nrats*2)) {
kb[j] <- (cumh1[n+1] - cumfa1[n])^2 - (cumh1[n] - cumfa1[n+1])^2
j=j+1
}
  
aroc1 = 0.5 + (0.25*sum(ka)) + (0.25*sum(kb))
# broc = log((0.25*sum(ka))/(0.25*sum(kb)));

aroc1<-as.numeric(aroc1) 
print(aroc1)
 

