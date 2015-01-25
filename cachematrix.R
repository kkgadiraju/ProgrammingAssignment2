##Usage: First call makeCacheMatrix() with a matrix as an argument
##Then call cacheSolve with the return value of makeCacheMatrix
##Once the value has been stored, every other time it is called, data is pulled out of cache

## Creates a cache matrix and stores it in a variable named matrix
##returns a list of functions which can be called from another function

makeCacheMatrix <- function(x = matrix()) {
          matrix<-NULL
          set<-function(y){
            x<<-y
            matrix<<-NULL
          }
          get<-function() x
          setInverse<-function(inv)matrix<<-inv
          getInverse<-function() matrix
          list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Check if matrix is in cache, if value returned is not null, print it. Otherwise, calcualate its inverse and store it

cacheSolve <- function(x, ...) {
      
  matrix<-x$getInverse()
  if(!is.null(matrix)){
    print("Getting cached data")
    return(matrix)
  }
  myMatrix<-x$get()
  matrix<-solve(myMatrix,...)
  x$setInverse(matrix)
  matrix
}
