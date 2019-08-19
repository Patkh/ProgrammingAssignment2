## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly. The pair of functions
## makeCacheMatrix and cacheSolve help in achieving this.

## The first function, makeCacheMatrix creates a special "matrix", which is really a 
## list containing functions to
##  a) set the value of the matrix
##  b) get the value of the matrix
##  c) set the inverse of the matrix
##  d) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(mat) m <<- mat
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
  
}

## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse of the matrix
## has already been calculated. If so, it gets the inverse of the matrix from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets
## the value of the matrix inverse in the cache via the setMatrixInverse function.
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  #solve function computes the inverse of the matrix
  m <- solve(data)
  x$setMatrixInverse(m)
  m
  
}
