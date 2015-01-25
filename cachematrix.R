##Usage: First call makeCacheMatrix() with a matrix as an argument
##Then call cacheSolve with the return value of makeCacheMatrix
##Once the value has been stored, every other time it is called, data is pulled out of cache

## Creates a cache matrix and stores it in a variable named matrix
##returns a list of functions which can be called from another function and can be used to set the value of the matrix,
## set the value of its inverse, or get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
          matrixInverse<-NULL
          set<-function(y){
            x<<-y
            matrixInverse<<-NULL
          }
          get<-function() x
          setInverse<-function(inv)matrixInverse<<-inv
          getInverse<-function() matrixInverse
          list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Check if matrix is in cache, if value returned is not null, print it. Otherwise, calcualate its inverse and store it
## If matrix has been altered, it is done by x$set() function, which will automatically set its corresponding inverse to null
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
