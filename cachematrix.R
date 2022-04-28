##Programming Assignment 2 - Two Functions

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse
  i <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Return the matrix
  get <- function() m 
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse 
  
  ## Return the inverse of the matrix
  getInverse <- function() i 
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Returns the inverse if already
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Gets the matrix from object
  data <- x$get()
  
  ## solve(X) returns the inverse of a square matrix
  m <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return matrix
  m
  
}
