## Put comments here that give an overall description of what your
## functions do

## -Turns Matrix into a list of functions for storage
## -Allows for storage and retrieval (get/set)
##   of original and inverse of matrix

makeCacheMatrix<- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Does the actual caching
## Checks if existing inverse value
## if not, stores newly solved one

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
