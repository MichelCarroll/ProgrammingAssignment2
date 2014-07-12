## File written by Michel Carroll, 2014

## These two functions provide an effective way of computing the inverse
## of a matrix while transparently caching the results of the operation
## to avoid unnecessary overhead

## The 'makeCacheMatrix' function returns a list that acts as a wrapper object
## containing the matrix object, and accessor/setter functions for both the 
## matrix and the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NA
  locMatrix <- x
  
  getMatrix <- function() {
    locMatrix
  }
  
  setMatrix <- function(mat) {
    locMatrix <<- mat
    inverse <<- NA
  }
  
  getCachedInverse <- function() {
    inverse
  }
  
  setCachedInverse <- function(inv) {
    inverse <<- inv
  }
  
  list(
    getMatrix = getMatrix,
    setMatrix = setMatrix,
    getCachedInverse = getCachedInverse,
    setCachedInverse = setCachedInverse
  )
}


## The 'cacheSolve' function returns the inverse of a matrix which has
## been wrapped by the 'makeCacheMatrix' function (therefore it's 
## dependent on it). It assumes that the contained matrix is invertible.

cacheSolve <- function(x, ...) {
  
  if(!is.matrix(x$getCachedInverse())) 
  {
    inverse <- solve(x$getMatrix(), ...)
    x$setCachedInverse(inverse)
  }
  
  x$getCachedInverse()
  
}
