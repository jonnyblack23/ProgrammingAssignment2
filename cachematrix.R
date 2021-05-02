## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse. The object is a list containing 4 functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix's inverse
## get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##inv will store the inverse of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cachSolve function myMatrixcomputes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then cacheSolve should retrieve the
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
