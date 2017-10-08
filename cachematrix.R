## cachematrix.R
## 

## makeCacheMatrix converts a matrix 'x' to a 
## special object that can be passed to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function()
    x
  setInverse <- function(inverse)
    cachedInverse <<- inverse
  getInverse <- function()
    cachedInverse
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}



## cacheSolve returns the inverse of a matrix created with makeCacheMatrix
## does not calculate the inverse of the same matrix twice

cacheSolve <- function(x, ...) {
  
  invFunc <- x$getInverse()
  if (!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
