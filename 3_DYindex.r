library(MASS)
library(zoo)
library(quantmod)
library(tseries)
library(lmtest)
library(frequencyConnectedness)
library(vars)

#### table generator function for latex 
plot.tab.tex <- function(data){
  
  data <- as.matrix(data)
  
  c <- ncol(data)
  r <- nrow(data)
  
  cat("\\begin{tabular}{l")
  for(j in 1:c){cat("r")}
  cat("} \n")
  
  cat("  &  ")
  for (j in 1:c){
    if ( j < c){
      cat(colnames(data)[j])
      cat("  &  ")
    } else if (j == c){
      cat(colnames(data)[j])
      cat(" \\\\ \\hline \\hline\n")}
  }
  
  for(i in 1:r){
    cat(rownames(data)[i])
    cat("  &  ")
    for (j in 1:c){
      if ( j < c){
        #cat("$")
        cat(data[i,j])
        #cat("$")
        cat("  &  ")
      } else if (j == c & !((i/2) == floor(i/2))){
        cat(data[i,j])
        cat(" \\\\ \n")
      }else if (j == c & ((i/2) == floor(i/2))){
        cat(data[i,j])
        cat(" \\\\ \n")}
    }
  }
  cat("\\end{tabular}")
}
wdir <- "C:/Users/49151/Desktop"
setwd(wdir)
DAT1 = read.csv("report1.csv")# replace "report2.csv" for Forex dataset
DATE= as.Date(as.character(DAT1[,1]),"%d/%m/%Y")  

Y=abs(100*DAT1[,-1]) #change here for forex market  DATA1[,-c(1,4)] 

colnames(Y)=c("Nasdaq","DowJones","EuroStox","FTSE100",
             "CAC40", "DAX","HongKong", "Shanghai","Nikkei225",
            "Allord","KSE", "Kospi", "SET","Sensex")
  # change here for forex c("GBP","EUR","HKD","CNY","JPY","AUD", "THB", "INR","TRY")

View(Y)  
k = ncol(Y)
################## DY 2012 - Table of  Volatility Spillover  

#####create function for  the forecast error variance decomposition
FEVD <- function(model, n.ahead=10,normalize=TRUE,standardize=TRUE) {
  if (class(model) != "varest") {
    return("The model class is not varest!")
  }
  A <- Phi(model, (n.ahead-1))
  epsilon <- residuals(model)
  Sigma <- t(epsilon)%*%epsilon / (model$obs)
  gi <- array(0, dim(A))
  sigmas <- sqrt(diag(Sigma))
  for (j in 1:dim(A)[3]) {
    gi[,,j] <- t(A[,,j]%*%Sigma%*%solve(diag(sqrt(diag(Sigma)))))
  }
  
  if (standardize==TRUE){
    girf=array(NA, c(dim(gi)[1],dim(gi)[2], (dim(gi)[3])))
    for (i in 1:dim(gi)[3]){
      girf[,,i]=((gi[,,i])%*%solve(diag(diag(gi[,,1]))))
    }
    gi = girf
  }
  
  num <- apply(gi^2,1:2,sum)
  den <- c(apply(num,1,sum))
  fevd <- t(num)/den
  if (normalize==TRUE) {
    fevd=(fevd/apply(fevd, 1, sum))
  } else {
    fevd=(fevd)
  }
  return = list(fevd=fevd, girf=gi)
}

############### create function  for Dynamics of Total, Directional, Net  spillovers
SM<-function(A){ 
  k = dim(A)[1]
  SOFM = apply(A,1:2,mean)*100
  VSI = (sum(rowSums(SOFM-diag(diag(SOFM))))/k)# or similarly (sum(colSums(SOFM-diag(diag(SOFM))))/k)
  INC = colSums(SOFM)
  TO = colSums(SOFM-diag(diag(SOFM)))
  FROM = rowSums(SOFM-diag(diag(SOFM)))
  NET = TO-FROM
  NPSO = t(SOFM)-SOFM
  
  ALL = rbind(rbind(rbind(cbind(SOFM,FROM),c(TO,sum(TO))),c(INC,NA)),c(NET,VSI))
  
  colnames(ALL) = c(rownames(A),"DFO")
  rownames(ALL) = c(rownames(A),"DTO","DIO","NDS")
  ALL = format(round(ALL,2),nsmall=2) 
  ALL[nrow(ALL)-1,ncol(ALL)] = "SI" 
  return = list(SOFM=SOFM,VSI=VSI,TO=TO,FROM=FROM,NET=NET,ALL=ALL,NPSO=INC)  
}
library(vars)
nlag = 2  # VAR(2)
nfore = 10 # 10-step ahead forecast
A_var = VAR(Y,p=nlag,type ="const")
A_ALL = FEVD(A_var, n.ahead=nfore)$fevd
colnames(A_ALL) = rownames(A_ALL) = colnames(Y)
A_var = SM(A_ALL)
####### TABLE OF VOLATILITY SPILLOVER ###################### 
View(A_var$ALL)

############# DTNAMIC CONNECTEDNESS MEASURES ###########################
t = nrow(Y)
k=ncol(Y)
space =150 # 150 weeks rolling window estimation
A = array(NA, c(k, k, (t-space)))
colnames(A) = rownames(A) = colnames(Y)
for (i in 1:dim(A)[3]) {
  varst = VAR(Y[i:(space+i-1),], p=nlag, type="const")#iterate for each rolling sample in matrix A
  A[,,i] = FEVD(varst, n.ahead=nfore)$fevd
}

#Create matrix for directional spillovers 
to = matrix(NA, ncol=k, nrow=(t-space))
from = matrix(NA, ncol=k, nrow=(t-space))
net = matrix(NA, ncol=k, nrow=(t-space))
npso = array(NA,c(k,k,(t-space)))
total = matrix(NA,ncol=1,nrow=(t-space))
for (i in 1:dim(A)[3]){#iterate to each Dynamics of the matrix  
  vd = SM(A[,,i])
  to[i,] = vd$TO/k
  from[i,] = vd$FROM/k
  net[i,] = vd$NET/k
  npso[,,i] = c(t(vd$SOFM)-vd$SOFM)/k
  total[i,] = vd$VSI
}

### TOTAL VOLATILITY SPILLOVERS
date = DATE[-c(1:space)]
par(mfrow = c(1,1), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
plot(date,total, type="l",xaxs="i",col="grey20", las=1, main="",ylab="",ylim=c(floor(min(total)),ceiling(max(total))),yaxs="i",xlab="",tck=0.01)
grid(NA,NULL,lty=1)
polygon(c(date,rev(date)),c(c(rep(0,nrow(total))),rev(total)),col="grey20", border="grey20")
sbox()
### DIRECTIONAL VOLATILITY SPILLOVERS (TO)
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,to[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1,
       main=paste(colnames(Y)[i],"TO all others"),ylim=c(floor(min(to)),ceiling(max(to))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(to))),rev(to[,i])),col="grey20", border="grey20")
  box()
}

### DIRECTIONAL VOLATILITY SPILLOVERS (FROM) 
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,from[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste(colnames(Y)[i],"FROM all others"),ylim=c(floor(min(from)),ceiling(max(from))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(from))),rev(from[,i])),col="grey20", border="grey20")
  box()
}

### NET VOLATILITY SPILLOVERS
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,net[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste("NET",colnames(Y)[i]),ylim=c(floor(min(net)),ceiling(max(net))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(net))),rev(net[,i])),col="grey20", border="grey20")
  box()
}
### NET PAIRWISE VOLATILITY SPILLOVERS
graphics.off()
nps = array(NA,c((t-space),k/2*(k-1)))
colnames(nps) = 1:ncol(nps)
jk = 1
for (i in 1:k) {
  for (j in 1:k) {
    {if (j<=i) {next} else
      nps[,jk] = npso[i,j,]
      colnames(nps)[jk] = paste0(colnames(Y)[i],"-",colnames(Y)[j])
      jk = jk + 1
    }
  }
}
par(mfrow = c(ceiling(ncol(nps)/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:ncol(nps)) {
  plot(date,nps[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=colnames(nps)[i],tck=0.02,yaxs="i",ylim=c(floor(min(nps)),ceiling(max(nps))))
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(nps))),rev(nps[,i])),col="grey20", border="grey20")
  box()
}
############END##############################

####################################
###  Alternatively we could use the packege "frequeConnectedness" #####
library(frequencyConnectedness)
est <- VAR(W, p = 2, type = "const")
sp <- spilloverDY12(est, n.ahead = 10, no.corr = F)
overall(sp)
to(sp)
plotOverall(sp)
params_est = list(p = 2, type = "const")
sp <- spilloverRollingDY12(Y[,c(1,2,4,5)], n.ahead = 10, no.corr = F, "VAR", params_est = params_est, window = 150)
plotOverall(sp)
plotFrom(sp)
plotTo(sp)
plotNet(sp)
plotPairwise(sp,type= "l",col="grey20", las=1, main="",ylab="",lwd=2)
####################END################

########################## Stock and currency ####################################### 
###### One can also show the spillovers across markets from stock to forex,
###### where we get strong contagious effect from stock to currency markets,and less
###### insignificant spilover from currency to stock markets.
###### the following lines could prove our findings.
###### DAT2 = read.csv("report2.csv")
###### D2 = as.Date(as.character(DAT2[,1]),"%d/%m/%Y")
###### R2<-cbind(DAT1[,c(3,4,9,10,11)], DAT2[,c(2,3,4,6,7,8)])
##### Y=abs(100*R2) and replicate all from line 61
############################## END    ##################################
