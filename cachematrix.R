## Put comments here that give an overall description of what your
## functions do

## This function will create a matrix object.
## This matrix object can cache its inverse. 
## Firstly, a matrix object, invrs is created
## and given a value of null.
## Second, a function named 'set' is created.
## This function takes object x for use in the 
## parent environment.
## The get function will retrieve, or 'get'
## the inverse of the matrix, x.
## The next function retrieves the inverse of
## matrix object 'invrs'.
## Finally, creating a list will allow use of 
## the extract operator, $. 

makeCacheMatrix <- function(x = matrix()) {
invrs <- NULL
set <- function(mtrx) {
  x <<- mtrx
  invrs <<- NULL
}
get <- function() (x)
setinverse <- function(solve) (invrs <<- solve) 
  getinverse <- function () (invrs)
list(set = set, get = get, setinverse =setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse for 
## the matrix created by function 'makeCacheMatrix
## by using the '$getinverse' function. 
## cacheSolve first checks to see if the matrix is null
## if valid (not null), the function retrieves matrix 
## from parent environment.
## If not valid (is null), function retrieves matrix from object
## and then uses the '$setinverse' function on object.
## Finally, the 'cacheMatrix' function returns the inverse of the object. 

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}
