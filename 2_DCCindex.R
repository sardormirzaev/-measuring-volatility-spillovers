library(tseries)
library(rugarch)
library(rmgarch)
library(quantmod)
 
# Please, re-run all codes for forex market volatity in all 3 R-files
# by changing here : read.csv("report2.csv") 

wdir <- "C:/Users/49151/Desktop"
setwd(wdir)
DAT1 = read.csv("report1.csv")# replace "report2.csv" for Forex dataset
DATE= as.Date(as.character(DAT1[,1]),"%d/%m/%Y")  

Y=abs(100*DAT1[,-1]) #change here for forex market  DATA1[,-c(1,4)] 
View(Y)
##############figures of DCC-AR-GARCH model
colnames(Y)=c("Nasdaq","DowJones","EuroStox","FTSE100",
              "CAC40", "DAX","HongKong", "Shanghai","Nikkei225",
              "Allord","KSE", "Kospi", "SET","Sensex")
      # change here for forex  c("GBP","EUR","CNY","JPY","AUD", "THB", "INR","TRY")

 ##########################  SPILOVERS ##############################################
N=nrow(Y)
dim(Y)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE), 
                   distribution.model="std",fixed.pars=list(omega=0))

uspec.n = multispec(replicate(14, spec)) ###for forex   change -> replicate(8, spec)
multf = multifit(uspec.n, Y)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvt')
fit1 = dccfit(spec1, Y, fit.control = list(eval.se = TRUE), fit = multf )#VAR.fit = A_var
#fit1@fit$solver$sol$pars
####TABLE: DCC-parameters estimation 
param<-fit1@mfit$matcoef
param

cor1 = rcor(fit1)  # extracts the correlation matrix
#cor1[,,dim(cor1)[3]]

cor11=cor1[1,,] # extract colomn  of dynamic correlation of each 
                # variable of the table (14x14)
                # change here for nikkei cor1[9,,]

###### change here for forex markets 
cor11=cor1[1,,]#  extract colomn  of dynamic correlation of each 
               #  variable of the table (8x8)
#graphics.off()
cor111<-cor11[,]
k=nrow(cor111)

#####Figures of  DCC-correlation spillovers 

par(mfrow =  c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
  for (i in 1:k) {
    plot(lndate,cor111[i,],xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1,
         main=paste("DJI with",rownames(cor111)[i])) 
          # change  for Nikkei with main=paste("Nikkei with"...
    grid(NA,NULL,lty=1)
    polygon(c(lndate,rev(lndate)),c(c(rep(0,ncol(cor111))),rev(cor111[i,])),col="grey20", border="grey20")
    box()
  }
######for FOrex we change the title--> main=paste("GBP with",rownames(cor111)[i]))

############################ END #################################################