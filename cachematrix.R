## makeCacheMatrix creates an object able to store a matrix and its inverse.
## cacheSolve returns the inverse of a CacheMatrix, using the cache if need be.


## Creates a Cache Matrix that can store a matrix and its inverse,
## eliminating the need to recalculate values.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}



## If the CacheMatrix provided has a caches inverse, return that inverse.
## Otherwise, calculates and caches the matrix inverse in the CacheMatrix objext.
cacheSolve <- function(x, ...) {
  cur_inv <- x$getinv()
  
  if (!is.null(cur_inv)) {
    message("Retrieving cached data...")
    cur_inv
  } else {
    inv <- solve(x$get())
    x$setinv(inv)
    inv
  }
}






