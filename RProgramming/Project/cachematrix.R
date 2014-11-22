## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions that are used to cache the
## inverse of the provided matrix.  The provided functions are:
##
## 1. set - assigns the matrix whose inverse will be cached
## 2. get - returns the matrix whose inverse is being cached
## 3. setInverse - assigns the inverse of the matrix
## 4. getInverse - the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(m) {
                x <<- m
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Uses a makeCacheMatrix to return the matrix inverse. The inverse
## is returned if has already been calculated (cached).  Otherwise, 
## the inverse is calculated, the cache is updated, and the inverse 
## is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
