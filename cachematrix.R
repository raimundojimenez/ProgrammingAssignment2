## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## This function creates a special object that stores a matrix and cache's its inverse.
     ## The first function, makeVector creates a special "vector", which is really a list containing a function to
     ## set the original matrix
     ## get the original matrix
     ## set the inverse matrix
     ## get the inverse matrix
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
