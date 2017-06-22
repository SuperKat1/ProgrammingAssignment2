## These functions create a cache of inverse of a matrix, computes the inverse of a matrix and also uses inverse stored in the
## cache matrix if no changes are detected. 

## makeCacheMatrix creates a special "matrix" object that can cache it's inverse. It does 4 steps:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse
## 4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y)  {
    x <<- y
    inverse <<- NULL
  } 
  getMatrix <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve computes the inverse of the special "matrix" returned by make CacheMatrix. If the inverse has already been calculated
## and the matrix has not changed then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMatrix()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
  }