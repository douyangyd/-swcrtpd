newpower <- function(M,allocation, cal_X){
  ### cal_X is the list of Xs and Ys used to run simulation
  ### all allocations were evaluated on the same 10000 dataset to reduce simulation errors
  fn1 <- function(x) x$clus.size 
  fn2 <- function(x) x$transitp
  fn3 <- function(x) x$y_trt 
  fn4 <- function(x) x$y_ctr
  fn5 <- function(x) x$ID
  clus.size.list<- sapply(cal_X,fn1,simplify=TRUE)
  transit.p <- sapply(cal_X,fn2,simplify=TRUE)
  y_trt <- sapply(cal_X,fn3,simplify=TRUE)
  y_crt <- sapply(cal_X,fn4,simplify=TRUE)
  clus.id <- sapply(cal_X,fn5,simplify=TRUE)
  output <- matrix(0, nrow=10000,ncol=5)
  con <- c()
  int <- c()
  cor <- c()
  Tx.temp <- allocation
  for(i in 1:10000){
    trt <- rep(Tx.temp, clus.size.list[,i]) # time to transit
    it <- as.numeric(transit.p[[i]] >= trt)
    tots <- sum(clus.size.list[,i])
    con[i] <- tots-sum(it) # size in control across all periods and clusters
    int[i] <- sum(it) # size in intervention across all periods and clusters
    cor[i] <- cor(transit.p[[i]],it) # correlation between time and intervention status
    dat_trt = data.frame(clus.id=clus.id[[i]],trt=trt,transit.p=transit.p[[i]],it=rep(1,tots),y=y_trt[[i]])
    dat_ctr = data.frame(clus.id=clus.id[[i]],trt=trt,transit.p=transit.p[[i]],it=rep(0,tots),y=y_crt[[i]])
    analytic.dat <- rbind(dat_ctr[which(it==0),],dat_trt[which(it==1),])
    fit = lme(y ~ transit.p + it, random=~1|clus.id, data=analytic.dat)
    output[i,] <- c(as.vector(summary(fit)$tTable[3,]))
  }
  return(list("coefs"=output,"consize"=con,"intsize"=int,"cor"=cor,"tot"=tots))
}

## Parallel computing to evaluate power
result <- foreach(i=1:nrow(Tx),.packages = 'nlme') %dopar% {try(newpower(
  M=c(24,16,8),
  allocation=Tx[i,],
  XYmtr),TRUE)}
