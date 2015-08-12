## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
 
    m <- x$getMatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
