library(moments)
library(tseries)
library(reshape2)

#### table generator funtion  for latex 
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

# Please, set your working directory
wdir <- "C:/Users/49151/Desktop"
setwd(wdir)

# Please, re-run all codes for forex market volatity in all 3 R-files
# by changing here : read.csv("report2.csv") 
DAT1<-read.csv("report1.csv") 
DATE<- as.Date(as.character(DAT1[,1]),"%d/%m/%Y")

Y <- abs(100*DAT1[,-1])
#View(Y)
colnames(Y)=c("Nasdaq","DowJones","EuroStox","FTSE100",
              "CAC40", "DAX","HongKong", "Shanghai","Nikkei225",
             "Allord","KSE", "Kospi", "SET","Sensex")
# chage here for forex  c("GBP","EUR","HKD","CNY","JPY","AUD", "THB", "INR","TRY")

### Table 1 SUMMARY STATISTICS
tab1 = rbind(apply(Y,2,mean),
             apply(Y,2,median),
             apply(Y,2,max),
             apply(Y,2,min),
             apply(Y,2,sd),
             apply(Y,2,skewness),
             apply(Y,2,kurtosis))
rownames(tab1) = c("Mean","Median", "Max", "Min","Std.Dev","Skew","Kurt")#"Maximum","Minimum"
#plot.tab.tex(t(round(tab1,4)))
View(t(tab1))

####FIGURES OF VOLATILITIES 

plotting<-ts(data=Y[,1:6])
plot(plotting, type="l", main="", col="grey20",xlab="",lwd=2)

plotting<-ts(data=Y[,7:14])#for forex change Y[,7:9]
plot(plotting, type="l", main="", col="grey20",xlab="",lwd=2)






