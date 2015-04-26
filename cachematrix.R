## R Programming - Week 3 - Programming Assignment 2
## This R script creates a special matrix object that caches its
## inverse instead of computing it repeatedly.

## Creates a cacheable matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse with NULL value
        i <- NULL
        ## Set a new matrix and reset the inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## Returns the matrix
        get <- function() x
        ## Cache the given inverse
        setinverse <- function(inverse) i <<- inverse
        ## Return the cached inverse
        getinverse <- function() i
        ## Return the CacheMatrix object
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of a cacheable matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then the cached inverse is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## if there is an inverse cached, return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if there is no inverse, get the matrix and calculate its inverse
        data <- x$get()
        i <- solve(data)
        ## cache the inverse and return it
        x$setinverse(i)
        i
}
