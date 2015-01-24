## Functions to cache the inverse of a matrix.


## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Return a list containing functions to set, get a matrix and to set, get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(m) inverse <<- m
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Retrieve the inverse matrix from the cache if it has already been calculated,
## otherwise calculate the inverse of matrix and store it in the cache
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m)
    x$setinverse(inverse)
    inverse
}


