## Caching the inverse of a matrix:
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. This script therefore
## contains a pair of functions, which enable you to create an object in which a matrix
## can be stored, which can subsequently be used for the computation of its inverse,
## which is simultaneously cached for future use.

## This first function, 'makeCacheMatrix', creates a matrix object which is used in the next
## function to compute and cache the inverse of your matrix. Note that you should input the 
## matrix of which you would like to compute and cache the inverse as the variable 'x'.

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <-  function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) I <<- inverse
      getinverse <- function() I
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function, 'cacheSolve', computes the inverse of the matrix used as input
## for 'makeCacheMatrix'. If this inverse has previously been calculated with the function,
## while the input matrix remains the same, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      I <- x$getinverse()
      if(!is.null(I)) {
            message("getting cached inverse")
            return(I)
      }
      matdat <- x$get()
      I <- solve(matdat)
      x$setinverse(I)
      I
}
