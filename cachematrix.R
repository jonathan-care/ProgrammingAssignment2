## This file contains two functions. They are used for calculating the inverse of a square
## matrix, caching a coping of this inverse to avoid costly compuatation, and returning the 
## cached copy if no change has been made to the input matrix, or computing and returning the
## new inverse if the input matrix has changed.

## The makeChacheMatrix function is essentially a list with four functions as the elements.
## The function takes a matrix as an input and provides the functionality to return the input
## matrix, change the matrix using set, geting or changing the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function takes as an argument the output or stored value of the makeCacheMatrix
## function and returns the inverse of the input matrix by one of two methods. If the inverse
## has already been calculate the function returns the stored inverse and avoids recalculating
## the inverse. If the input matrix has changed or the inverse has not yet been calculated, the
## function calculates and returns the inverse of the input matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
