## This file contains two functions which imjprove the efficiency of repeated matrix
## inversion by saving the inverse of a matrix and recalling it later as needed 

## This first function saves all the parameters necessary to create and store the
## matrix

makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize objects for holding values
      chk <- NULL
      set <- function(y) {
            x <<- y
            chk <<- NULL
      }
      
      ## Get the matrix, find it's inverse, and save parameters for later use
      get <- function() x
      setinverse <- function(inverse) chk <<- inverse
      getinverse <- function() chk
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function checks for whether the inverse has been previously cached.
## If the inverse exists, return that. If not, find matrix inverse.

cacheSolve <- function(x, ...) {
      
      ## If chk is not null (ie, it holds inverse), give message, return value, and end
      chk <- x$getinverse()
      if(!is.null(chk)) {
            message("getting cached data")
            return(chk)
      }
      
      ## We get to this section only if chk was null (ie, nothing previously cached)
      ## Solve for inverse of matrix, save it for future use, and return the inverse
      data <- x$get()
      chk <- solve(data, ...)
      x$setinverse(chk)
      chk
}
