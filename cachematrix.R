## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## create a special object that stores a matrix and cache's its inverse.
     ## The first function, makeVector creates a special "vector", which is really a list containing a function to
     ## set the value of the vector
     ## get the value of the vector
     ## set the value of the mean
     ## get the value of the mean
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
        ## Return a matrix that is the inverse of 'x'
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
