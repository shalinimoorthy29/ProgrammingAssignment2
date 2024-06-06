## Caching the Inverse of a Matrix:
#Matrix inversion can be a costly computation. Therefore, there may be an 
#advantage to caching the inverse of a matrix rather than carrying out
#repeated computing. 
#Together, makeCacheMatrix creates a storage object for matrices and their 
#inverses, while cacheSolve computes the inverse of a matrix if not already 
#cached, enhancing computational efficiency by avoiding redundant computations.

## This function will take a matrix as input 
#and return a list containing functions to set and get the matrix, 
#as well as functions to set and get the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function will take a matrix object created by makeCacheMatrix 
#and compute the inverse if it's not already cached, otherwise, 
#it retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}