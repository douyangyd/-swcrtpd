library(arrangements)

### The code was created by Liang Xu
### 48 clusters with 24 small, 16 medium and 8 large clusters 
temp <- c(24,16,8) # # number of cluster for each type (S,M,L) (only for three size group)
M <- temp
N = rep(12,4) # #of clus transit per period
p = length(N) 


X0=permutations(c(0:min(temp[3],sum(temp)/p)),k=(p-1),replace=T)
X1<-cbind(X0,apply(X0,1,function(x){ifelse(sum(x)<=temp[3]&&sum(x)>=temp[3]-sum(temp)/p,temp[3]-sum(x),NA)}))
X1<-X1[!rowSums(!is.finite(X1)),] #X1 gives all the ways the large clusters can be assigned

X2=permutations(c(0:min(temp[2],sum(temp)/p)),k=(p-1),replace=T)
X3<-cbind(X2,apply(X2,1,function(x){ifelse(sum(x)<=temp[2]&&sum(x)>=temp[2]-sum(temp)/p,temp[2]-sum(x),NA)}))
X3<-X3[!rowSums(!is.finite(X3)),] #X3 gives all the ways the medium clusters can be assigned

X6<-matrix(0,ncol=2*p)
for (i in 1:dim(X1)[1]){
  X4<-matrix(X1[i,],nrow = dim(X3)[1],ncol=p,byrow = T)
  X5<-cbind(X4,X3,apply(X3+X4,1,function(x){ifelse(max(x)<=sum(temp)/p&&sum(x)==(sum(temp)-temp[1]),sum(x),NA)}))
  X5<-X5[!rowSums(!is.finite(X5)),]
  X6=rbind(X6,X5[,1:(2*p)]) #X6 is a dummy matrix that contains all the possible unique allocations; columns 1 to p for large clusters, and columns (p+1) to 2p for medium clusters
}

X6<-X6[2:(dim(X6)[1]),]

Xs=matrix(sum(temp)/p,ncol=p,nrow=dim(X6)[1])-X6[,1:p]-X6[,(p+1):(2*p)] #allocations for small clusters
Xl=X6[,1:p] #allocations for medium clusters
Xm=X6[,(p+1):(2*p)] #allocations for large clusters

allocs <- list()
for(i in 1:dim(X6)[1]){
  allocs[[i]] <- rbind(Xl[i,],Xm[i,],Xs[i,])
}

allocs <- array(unlist(allocs), dim = c(length(temp), p, dim(X6)[1])) 


rand <-function(M,N,allocs,k){
  ord <-c()
  for (i in 1:length(M)){
    deno <-c()
    for (j in 1:length(N)){
      temp <-c()
      temp <- rep(j,allocs[i,j,k])
      deno <- c(deno,temp)
    }
    ord <- c(ord,deno)
  }
  return(ord)
}


multi <- function(M,N,allocs,k){
  # k is the kth unique allocation you want to evaluate
  temp <-c()
  final <-c()
  for (i in 1:length(M)){
    for (j in 1:length(N)){
      temp[j] <- factorial(allocs[i,j,k])
      deno <- prod(temp)
    }
    final[i] <- factorial(M[i])/deno
  }
  return(prod(final))
}

Tx <- matrix(ncol=sum(M), nrow=dim(X6)[1])
for(i in 1:dim(X6)[1]){
  Tx[i,]<-rand(M,N,allocs,i)}
# allocations

v <- c()
for(i in 1:dim(X6)[1]){
  v[i]<-multi(M,N,allocs,i)}
w<-v/sum(v)## weights




##===================================================







### for two/ more than three size groups. Would recommend below function for more efficient generation
### 24 clusters with 6 large and 18 small clusters
M = c(6,18) ## of clus transit per period (L,S)
N = rep(6,4) 
p = length(N)


## -------------------------------------------------------------------------
## Part one: List all the unique allocations
## -------------------------------------------------------------------------


alloc1 = function(M.now, period=1, stem=vector(), store.alloc=F) {
  #print("new")
  #print(paste("M.now = ", M.now))
  if (period ==  J) {
    complete.alloc = cbind(stem, M.now)
    #print(M)
    if (all(apply(complete.alloc, 1, sum) == M)) {
      count <<- count + 1
      print(paste("count = ", count))
      if (store.alloc) {
        allocs <<- c(allocs, complete.alloc)
      }
    }
  } else {
    min.type = pmax(0, N[period] - sum(M.now) + M.now)
    max.type = pmin(M.now, N[period])
    #print(paste("min.type = ", min.type))
    #print(paste("max.type = ", max.type))
    n.per.type = max.type - min.type + 1
    n.alloc = vector()
    #print(paste("n.per.type", paste(n.per.type)))
    #print(prod(n.per.type))
    for (j in 0:(prod(n.per.type) - 1)) {
      #print(paste("j =", j))
      for (i in 1:I) {
        if (i == 1) n.alloc[i] = 
            j %/% prod(n.per.type[(i+1):I]) + min.type[i] else
              if (i == I) n.alloc[i] = 
                  j %% (n.per.type[I]) + min.type[i] else
                    n.alloc[i] = 
                      ((j %/% prod(n.per.type[(i+1):I])) %%
                         n.per.type[i]) + min.type[i]
      }
      #print(n.alloc)
      if (sum(n.alloc) == N[period]) alloc1(
        M.now = M.now - n.alloc, 
        period = period + 1, 
        stem = cbind(stem, n.alloc), 
        store.alloc = store.alloc) 
    }
  }
}


# Creat the allocation for your situation
I = length(M)
J = length(N)
count = 0
allocs = vector()
alloc1(M, store.alloc=T)
dim(allocs) = c(I, J, count)

rand <-function(M,N,allocs,k){
  ord <-c()
  for (i in 1:length(M)){
    deno <-c()
    for (j in 1:length(N)){
      temp <-c()
      temp <- rep(j,allocs[i,j,k])
      deno <- c(deno,temp)
    }
    ord <- c(ord,deno)
  }
  return(ord)
}
Tx <- matrix(ncol=sum(M), nrow=count)
for(i in 1:count){
  Tx[i,]<-rand(M,N,allocs,i)}

multi <- function(M,N,allocs,k){
  # k is the kth unique allocation you want to evaluate
  temp <-c()
  final <-c()
  for (i in 1:length(M)){
    for (j in 1:length(N)){
      temp[j] <- factorial(allocs[i,j,k])
      deno <- prod(temp)
    }
    final[i] <- factorial(M[i])/deno
  }
  return(prod(final))
}


# compute the multiplicities
v <- c()
for(i in 1:count){
  v[i]<-multi(M,N,allocs,i)}
w<-v/sum(v)
