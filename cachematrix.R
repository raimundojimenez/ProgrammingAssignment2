## Put comments here that give an overall description of what your functions do

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
     ## This function creates a special object that stores a matrix and cache's
     ## its inverse. The first function, makeCacheMatrix creates a special
     ## "matrix", which is really a list containing a function to 
     ## 1) set the original matrix 
     ## 2) get the original matrix 
     ## 3) set the inverse matrix 
     ## 4) get theinverse matrix
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(sol) i <<- sol
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
     ## This function returns a matrix that is the inverse of 'x'
     ## It first look for a cached result. If not found then calculates the inverse matrix using the 'solve' function
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinv(i)
     i
}
