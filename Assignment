##makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse.matrix<-NULL
  set<-function(y){
    x<<-y
    inverse.matrix<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse.matrix) inverse.matrix<<-inverse.matrix
  getInverse<-function() inverse.matrix
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
##cacheSolve calculates the inverse of the special “matrix” created with makeCacheMatrix. 
##However, it first checks to see if the inverse has already been calculated. If so, 
##it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the value of the inverse in the cache via the setInverse function. 
cacheSolve <- function(x, ...) {
  inverse.matrix<-x$getInverse()
  if(!is.null(inverse.matrix)){
    message("getting cached data")
    return(inverse.matrix)
  }
  data<-x$get()
  inverse.matrix<-solve(data,...)
  x$setInverse(inverse.matrix)
  inverse.matrix
}
