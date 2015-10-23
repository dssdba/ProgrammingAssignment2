## R Programming class
## Programming Assignment 2
## Author: Rick Hutchison
## Created: 10/23/2015
## 
## This is a set of functions used to create and cache the 
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square matrix
  ## returns a list of the functions:
  ## set    - sets the value of the matrix
  ## get    - gets the value of the matrix
  ## setinv - sets the value of the inverse of the matrix
  ## getinv - gets the value of the inverse of the matrix
  
  ## initialize the inverse matrix locally
  invmat <- NULL
  
  ## initialize matrix and inverse matrix in parent(global) environment
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) invmat <<- inverse
  
  getinv <- function() invmat
  
  # return our new functions list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## compute the inverse of the matrix returned by makeCacheMatrix.
  ## If the inverse has already been calculated (and the matrix has not changed), then
  ## cachesolve should retrieve the inverse from the cache.
  
  invmat <- x$getinv()
  
  # if the inverse of the matrix has already been created, return the cached inverse
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  
  # compute the inverse matrix via the solve function and return it
  data <- x$get()
  
  invmat <- solve(data, ...)
  
  x$setinv(invmat)
  
  invmat
}
