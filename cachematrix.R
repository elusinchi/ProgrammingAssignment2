## A pair of functions that cache the inverse of a matrix so that 
## the inverse isn't recomputed if it has already been solved.


## This function creates a special "matrix" object
## that can cache its inverse.
## 
## Return a list containing the matrix and set,get,setSolve,getSolve functions
makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
}



## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## WARNING: Assume the passed matrix:
##  a) is created with makeCacheMatrix and
##  b) is invertible
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
  
}
