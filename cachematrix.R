## makeCacheMatrix creates a special matrix object, 
## and then cacheSolv calculates the inverse of the matrix.
## If the matrix inverse has already been calculated,  
## it will instead find it in the cache and return it,
## and not calculate it again.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(t) m <<- t
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function cacheSolve returns the inverse of a matrix A 
## created with the makeCacheMatrix function.
## If the cached inverse is available, 
## cacheSolve retrieves it, 
## while if not, 
## it computes, caches, and returns it.

cachesolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- t(data, ...)
  x$setinv(m)
  m
}
