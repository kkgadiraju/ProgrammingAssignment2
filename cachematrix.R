## Put comments here that give an overall description of what your
## functions do

## Creates a matrix 

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
