## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  ## modify the reference of matrix
  set  <- function(newReference){
    x <<- newReference
    inverse <<- NULL 
  }
  ## get the matrix
  get  <- function() x
  ## cache the inverse
  setinverse  <- function(inverse) inverse  <<- inverse
  ## get the cached inverse
  getinverse  <- function() inverse
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Computes the inverse. If the inverse has been cached returns the cached value
 cacheSolve<- function(x, ...) {
  i  <- x$getinverse()
  ## If matrix is cached already return cached data
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## calculate the inverse
  data  <- x$get()
  i  <- solve(data, ...)
  ## cache inverse
  x$setinverse(i)
  i
}
