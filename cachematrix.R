## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function will create a special matrix which cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){x}
  setInv<-function(inv){m<<-inv}
  getInv<-function(){m}
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function
##This function finds the inverse if its not cached, else returs the inverse from cache

cacheSolve <- function(x, ...) {
  m<-x$getInv()
  if(!is.null(m)){
    message("getting cached Inverse data")
    return(m)
  }
  
  dat<-x$get()
  m<-solve(dat,...)
  x$setInv(m)
  
  m      ## Return a matrix that is the inverse of 'x'
}
