#Adapative_Thresholding of 3 dimmensions
#Using the sample data given on the website(volume/label)
#Import the data
rm(list = ls())  #Removing existing variable  
library(R.matlab)
volume <- readMat('/Users/apple/Desktop/data2020-master/sample.mat')$vol 
label  <- readMat('/Users/apple/Desktop/data2020-master/true_label.mat')$md
label[label!=0] = 1
w = length(volume[,1,1])
l = length(volume[1,,1])
h = length(volume[1,1,])

#Define AdapativeThresholding function
AdapativeThresholding3 <- function(IN,w,l,h){ 
  #IN is the digital target data 
  S <- array(data=IN,dim = c(w,l,h)) #Convert into matrix
  intImg <- array(data = 0,dim = c(w,l,h)) 
  out <- array(data = 0,dim = c(w,l,h))
  SUM <- 0
  for (i in 1:w) {
    for (j in 1:l) {
      for (k in 1:h){
      SUM <-  SUM + S[i,j,k]
      if(i == 1 )
        intImg[i,j,k] <- SUM
      else
        intImg[i,j,k] <- intImg[i-1,j,k]+SUM
      }
    }
  }
  
  for (i in 1:w) {
    for (j in 1:l) {
      for(k in 1:h) {
      x1 <- max(i-s/2,1)
      x2 <- min(i+s/2,w)
      y1 <- max(j-s/2,1)
      y2 <- min(j+s/2,l)
      z1 <- max(k-s/2,1)
      z2 <- min(k+s/2,h)
      count <- (x2-x1)*(y2-y1)*(z2-z1)
      sum <- 0
      if(x1==1 & y1==1 &z1==1)
        sum1 <- intImg[x2,y2,z2]
      if(x1==1 & y1>1 &z1 ==1)
        sum1 <- intImg[x2,y2,z2]-intImg[x2,y1-1,z2]
      if(x1>1 & y1==1 &z1 ==1)
        sum1 <- intImg[x2,y2,z2]-intImg[x1-1,y2,z2]
      if(x1==1 & y1==1 &z1 >1)
        sum1 <- intImg[x2,y2,z2]-intImg[x2,y2,z1-1]
      if(x1==1 & y1>1 &z1 >1)
        sum1 <- intImg[x2,y2,z2]-intImg[x2,y1-1,z2]-intImg[x2,y2,z1-1]+intImg[x2,y1-1,z1-1]
      if(x1>1 & y1>1 &z1==1)
        sum1 <- intImg[x2,y2,z2]-intImg[x2,y1-1,z2]-intImg[x1-1,y2,z2]+intImg[x1-1,y1-1,z2]
      if(x1>1 & y1==1 &z1>1)
        sum1 <- intImg[x2,y2,z2]-intImg[x2,y2,z1-1]-intImg[x1-1,y2,z2]+intImg[x1-1,y2,z1-1]
      if(x1>1 & y1>1 & z1>1)
        sum1 = intImg[x2,y2,z2] - intImg[x1-1,y2,z2] - intImg[x2,y1-1,z2] - intImg[x2,y2,z1-1] + intImg[x1-1,y1-1,z2] + intImg[x1-1,y2,z1-1] + intImg[x2,y1-1,z1-1] - intImg[x1-1,y1-1,z1-1]
      
      if(S[i,j,k]*count <= sum1 *(1-t/100))
        out[i,j,k] <- 0
      else
        out[i,j,k] <- 1
      }
    }
  }
  return(out)
}

s <- 10 #Define s=w/10
t <- 95 # The percent lower than the average 
MyResult2 <- AdapativeThresholding3(volume,w,l,h) #Implement

Volume3 <- array(data=volume,dim = c(w,l,h))
Label3 <- array(data=label,dim = c(w,l,h))

N=w*l*h
TPR <- sum(Label3[,,]==0&MyResult[,,]==0)/N #true positive rate
FPR <- sum(Label3[,,]==1&MyResult[,,]==0)/N #false positive rate
TNR <- sum(Label3[,,]==0&MyResult[,,]==1)/N #ture negative rate
FNR <- sum(Label3[,,]==1&MyResult[,,]==1)/N #false negative rate

TPR #0.710817
FPR #0.05708
TNR #0.068798
FNR #0.163305







