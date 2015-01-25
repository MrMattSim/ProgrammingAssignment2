## Coursera, R Programming, Assignment 2, January 25, 2015
## Functions for caching the solution of a square invertible matrix.

## Creates a custom matrix object that stores its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Checks a custom matrix object for an existing solution to "solve(x)" 
## (ie. the inverse of a square invertible matrix). 
## If it exists, return the stored solution and skip the calculation.
## Otherwise, calculate and store the solution.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
