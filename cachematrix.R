## The two functions below may be used to create and cache the inverse of
## a given matrix.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve() checks for a cached inverse of the given matrix and returns it if found.
## If there is no cached inverse for the given matrix, one is calculated, cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Get the inverse if any exists
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                # Return the cached inverse
                return(inverse)
        }
        data <- x$get()
        # Calculate the inverse
        inverse <- solve(data, ...)
        # Cache the inverse to the "matrix" object
        x$setinverse(inverse)
        # Return the inverse
        inverse
}

## Below is a simple sanity check unit test
# X <- matrix(sample.int(1e+06), nrow = 1000, ncol = 1000)
# 
# M <- makeCacheMatrix(X)
# 
# cacheSolve(M)
# 
# cacheSolve(M)
