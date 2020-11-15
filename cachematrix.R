## Pair of functions to compute the Inverse of a Matrix with the singularity 
## that it first checks if the calculation was already performed and the matrix
## has not changed, in which case the inverse is retrieved from cache instead of
## repeating the calculation.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL
  set <- function(y) {
          x <<- y
          matrixInv <<- NULL
         }
  get <- function() x
  setinverse <- function(inverse) matrixInv <<- inverse
  getinverse <- function() matrixInv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## chacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then the inverse is retrieved form the cache instead of 
## being calculated.

cacheSolve <- function(x, ...) {
  matrixInv <- x$getinverse()
  if(!is.null(matrixInv)) {
    message("getting cached data")
    return(matrixInv)
  }
  dataMatrix <- x$get()
  matrixInv <- solve(dataMatrix, ...)
  x$setinverse(matrixInv)
  matrixInv
}
