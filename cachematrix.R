## Assignment 2 programming assignment.  Submitted 2015-10-25.
## This module demonstrates how to implement objects with methods in R.

## Return a list of functions that wrap a simple R matrix and allow it to 
## 1) get and set the underlying matrix.
## 2) get and set the cached, inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
      x <<- y
      inv_matrix <<- NULL
    }
    get <- function() x
    set_inv <- function(invm) inv_matrix <<- invm
    get_inv <- function() inv_matrix
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}



## This function takes a CacheMatrix (as returned from makeCacheMatrix) x and 
## calculates, caches, and returns its inverse.
cacheSolve <- function(x, ...) {
  ## Is the inverse of this matrix already cached?
  inv_matrix <- x$get_inv()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  ## No, it was not already cached. So...
  matrix <- x$get()
  ## Calculate it.
  inv_matrix <- solve(matrix, ...)
  ## CAche it.
  x$set_inv(inv_matrix)
  ## And return it.
  inv_matrix
}
