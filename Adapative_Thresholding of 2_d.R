#Adapative_Thresholding of 2 dimmensions
#Using the sample data given on the website(volume/label)
#Import the data
rm(list = ls())  #Removing existing variable  
library(R.matlab)
volume <- readMat('/Users/apple/Desktop/data2020-master/sample.mat')$vol 
label  <- readMat('/Users/apple/Desktop/data2020-master/true_label.mat')$md
label[label!=0] = 1

#Define AdapativeThresholding function
AdapativeThresholding <- function(IN,w,h){ 
  #IN is the digital target data 
  #w is the row number
  #h is the column number
  S <- matrix(data=IN,nrow = w,ncol =h) #Convert into matrix
  intImg <- matrix(data = 0,nrow = w,ncol = h) 
  out <- matrix(data = 0,nrow = w,ncol = h)
  for (i in 1:w) {
    SUM <-  0
    for (j in 1:h) {
      SUM <-  SUM + S[i,j]
      if(i == 1)
        intImg[i,j] <- SUM
      else
        intImg[i,j] <- intImg[i-1,j]+SUM
    }
  }
  
  for (i in 1:w) {
    for (j in 1:h) {
      x1 <- max(i-s/2,1)
      x2 <- min(i+s/2,w)
      y1 <- max(j-s/2,1)
      y2 <- min(j+s/2,h)
      count <- (x2-x1)*(y2-y1)
      sum <- 0
      if(x1==1 & y1==1)
        sum1 <- intImg[x2,y2]
      if(x1==1 & y1>1)
        sum1 <- intImg[x2,y2]-intImg[x2,y1-1]
      if(x1>1 & y1==1)
        sum1 <- intImg[x2,y2]-intImg[x1-1,y2]
      if(x1>1 & y1>1)
        sum1 <- intImg[x2,y2]-intImg[x1-1,y2]-intImg[x2,y1-1]+intImg[x1-1,y1-1]
      
      if(S[i,j]*count <= sum1 *(1-t/100))
        out[i,j] <- 0
      else
        out[i,j] <- 1
    }
  }
  return(out)
}

w=100
h=10000
s <- 10 #Define s=w/10
t <- 95 # The percent lower than the average 
MyResult <- AdapativeThresholding(volume,w,h) #Implement

Volume <- matrix(data=volume,nrow=w,ncol=h)
Label <- matrix(data=label,nrow=w,ncol=h)

N=w*h
TPR <- sum(Label[,]==0&MyResult[,]==0)/N #true positive rate
FPR <- sum(Label[,]==1&MyResult[,]==0)/N #false positive rate
TNR <- sum(Label[,]==0&MyResult[,]==1)/N #ture negative rate
FNR <- sum(Label[,]==1&MyResult[,]==1)/N #false negative rate

TPR #0.714646
FPR #0.062127
TNR #0.064969
FNR #0.158258 






