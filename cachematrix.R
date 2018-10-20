## This script implements functions to cache the inverse of a matrix.
##
## Matrix inversion is a costly computation. The following functions
## allow to cache the inverse of a matrix so that it must not be computed again
## when it has already been computed.

## This function creates an object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) x_inverse <<- i
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a matrix. 
## If the inverse has already been computed, then the cached value is returned.
## Otherwise the inverse is computed and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inverse <- x$getinverse()
        if (is.null(x_inverse)) {
                message("compute inverse and cache it")
                x_inverse <- solve(x$get())
                x$setinverse(x_inverse)
        } else {
                message("return cached value")
        }
        x_inverse
}
