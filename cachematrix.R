## Taken together, these functions take a matrix, calculate its inverse
## if not already provided. 
## If the inverse is already provided, these functions retrieve the 
## inverse from storage (i.e. cache). This saves the need to calcuate the
## inverse every time it is required.

## This function will create a matrix object.
## which can cache its own inverse. 
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
