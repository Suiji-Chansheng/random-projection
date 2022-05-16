library("RPEnsemble")
library("parallel")
library("MASS")
library("randomForest")
#随机森林
CompSim <- function(Model.No, n.train = 50, p = 50, s0 = 1, prior = 1/2,n.test=15, n.reps = 100)
{
  for (i in 1:n.reps)
  {
    
    set.seed(100 + i)
    
    print(i)
    
    #Generate data
    data.train <- RPModel(Model.No, n.train, p, prior)
    data.test <- RPModel(Model.No, n.test, p, prior)
    
    n.min <- min(table(data.train$y))
    n1 <- table(data.train$y)[[1]]
    n2 <- table(data.train$y)[[2]]

  
    errRandForest <- NA ; tRF <- NA
    tRF <- system.time(RandForest <- randomForest(x = data.train$x, y =  factor(data.train$y), xtest = data.test$x, ytest = factor(data.test$y), ntree=1000, mtry=sqrt(p), replace=TRUE, classwt=c(n1/n.train, n2/n.train), cutoff = c(0.5,0.5), sampsize = n.train, nodesize = 1, keep.forest= TRUE))[1:3] + system.time(errRandForest <- 100*mean(as.numeric(predict(RandForest, newdata = data.test$x)) != data.test$y, na.rm = TRUE))[1:3]
    

    if (i==1) Risk <-  c(errRandForest)
    else Risk <- rbind(Risk,  c(errRandForest))
    if (i==1) Time <-  rbind(tRF)
    else Time <- Time + rbind(tRF)
  }
  return(list(Risk = Risk, Time = Time))
}



ddd<-CompSim2(1, n.train = 1000, p = 100, s0 = 1, prior = 1/2, n.test=100,n.reps = 100, k = c(3,5))
#随机投影集成的随机森林
MainSim <- function(Model.No, n.train = 50, p , s0 = 1, prior = 1/2, n.test = 10, n.reps = 100, d = 5, B1 = 100, B2=100,  k = c(3,5))
{
  psnice(value = 19)
  for (i in 1:n.reps)
  {
    print(i)
    set.seed(100 + i)
    data.train <- RPModel(Model.No, n.train, p, prior)
    data.test <- RPModel(Model.No, n.test, p, prior)
    
    OutRF <-  RPParallel(XTrain =  data.train$x , YTrain = data.train$y , XTest = data.test$x, d = d, B1 = B1, B2 = B2, base = "Other", projmethod = "Gaussian",clustertype = "Default")
    errRF <- 100*mean(RPEnsembleClass(OutRF, n = n.train,n.test=10 ,p1 = sum(data.train$y == 1)/n.train, samplesplit = FALSE, alpha = RPalpha(OutRF, Y = data.train$y, p1 = sum(data.train$y == 1)/n.train))!= data.test$y)
    
    if (i==1) Risk <- c(errRF)
    else Risk <- rbind(Risk, c(errRF))
  }
  return(Risk)
}



#标准差的估计

sderror<-function(R,Nreps=100,ntest){
  sdd<-1/sqrt(Nreps)*sqrt(mean(R)*(100-mean(R))/ntest+((ntest-1)/(ntest*Nreps))*sum((R-mean(R))^2))
  return(sdd)
}







