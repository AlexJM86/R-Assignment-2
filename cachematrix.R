## Matrix inversion is usually a costly computation and there can be some
## benefit to caching the inverse of the matrix rather than computing it repeatedly.
## The following two functions cache the inverse of a matrix.


## 1. This function creates a special 'matrix' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y)  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## 2. This function computes the inverse of the special 'matrix' returned by the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}