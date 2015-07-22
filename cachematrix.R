##This assignment is regarding lexical scoping and caching functions that may
##require a long compution time.

## I have written the following functions that cache the inverse of a matrix:
##1. makeCacheMatrix - This function creates a special"matrix" object that can 
## cache its inverse.
##2.cacheSolve - This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve hould retrieve the inverse
##from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    X <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
