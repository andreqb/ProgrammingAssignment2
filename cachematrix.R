## The following functions create and calculates the inverse of a matrix for 
##programming assignment #2 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvertmatrix <- function(matrix) m <<- matrix
      getinvertedmatrix <- function() m
      list(set = set, get = get,
           setinvertmatrix = setinvertmatrix,
           getinvertedmatrix = getinvertedmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getinvertedmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      m <- solve(data)
      x$setinvertmatrix(m)
      m
}
