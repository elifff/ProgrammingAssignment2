## These two functions are used to cache the inverse of a matrix which is usually a costly computation process.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.

## The first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## It creates a list containin a fuction to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverseofmatrix <- function(inverseofmatrix) inv_mat <<- inverseofmatrix
  getinverseofmatrix <- function() inv_mat
  list(set=set, get=get, setinverseofmatrix=setinverseofmatrix, getinverseofmatrix=getinverseofmatrix)
}


## The second function "cacheSolve" computes the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        inv_mat <- x$getinverseofmatrix()
        if (!is.null(inv_mat)) {
          message("getting cached data...")
          return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data)
        x$setinverseofmatrix(inv_mat)
        inv_mat
}


## Note that the matrix supplied is always assumed to be invertible.
