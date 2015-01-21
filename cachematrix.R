## A pair of functions that cache the inverse of a matrix so that 
## the inverse isn't recomputed if it has already been solved.


## This function creates a special "matrix" object
## that can cache its inverse.
## 
## Return a list containing set,get,setSolve,getSolve functions
makeCacheMatrix <- function(x = matrix()) {

  # The solved matrix will be saved in s
  # If not solved yet, s will be NULL
  s <- NULL
  
  # provide set and get functions for the matrix itself
  set <- function(y) {
    x <<- y
    s <<- NULL
    # Note the use of <<- to assign x and s in another environment than their current environment
  }
  get <- function() x
  
  # Provide set and get function for the solved matrix. 
  setSolve <- function(solve) s <<- solve
  # Note the use of <<- to assign s in another environment than its current environment
  getSolve <- function() s
  
  # return a list containing the set, get for the matrix and for the cached solved matrix
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
        
  # read the cached solved matrix
  s <- x$getSolve()
  
  # if it isn't null, it has already been solved. Excellent! Return the cached value.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # Aw.... we don't have a cached solved matrix, so let's compute it...
  data <- x$get()
  s <- solve(data, ...)
  
  # and let's not forget to record the solved matrix in the cache so it can be read the next time.
  x$setSolve(s)
  
  # and return the solved matrix
  s
  
}
