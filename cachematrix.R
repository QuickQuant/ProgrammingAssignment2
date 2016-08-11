## functions for caching a matrix inverse operation

## initializes matrix with its corresponding functions

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(y) {
    x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cachedInv <<- inv
  getinv <- function() cachedInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## determines if there is a calculation in the cache. Retrieves one and returns it if so. Otherwise the calculation is completed, the result is cached, then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedInv <- x$getinv()
  if(!is.null(cachedInv)) {
    message("getting cached data")
    return(cachedInv)
  }
  data <- x$get()
  cachedInv <- solve(data, ...)
  x$setinv(cachedInv)
  cachedInv
}
