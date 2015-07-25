## Computes and Caches the inverse of a matrix. That is, if the matrix inverse
## has already been computed it returns the cache, if not it computes inverse.

## makeCacheMatrix is a list of 4 functions (set, get, setinv, getinv)
## makeCacheMatrix$set allows the assignment of a matrix,x, to be stored in cache
## makeCacheMatrix$get returns the cached matrix x
## makeCacheMatrix$setinv stores the computed inverse of the matrix
## makeCacheMatrix$getinv returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Returns the matrix that is the inverse of matrix x.
## If inverse is cached then returns caches value
## else computes the inverse matrix and stores in cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }

