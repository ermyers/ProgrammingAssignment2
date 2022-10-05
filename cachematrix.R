## Code to create a matrix that can cache its inverse, and to return the cached
## inverse (or calculate and return the inverse if it is not already cached.


## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # solve() computes the inverse of a square matrix
  x$setinverse(inv)
  inv
}
