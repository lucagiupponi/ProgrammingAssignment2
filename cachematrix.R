## Put comments here that give an overall description of what your
## functions do

## The first function:

## 1.  sets the contents of a matrix
## 2.  gets the contents of a matrix
## 3.  caches the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) { inv <<- solve(x)}
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The second function checks for a cached value, and if not, calculates the inverse of the supplied matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(inv)
  inv}
