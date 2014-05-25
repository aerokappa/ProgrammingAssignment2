## This file contains two functions makeCacheMatrix and cacheSolve, both of
## which are modeled after the corresponding vector funtions written by
## Roger Peng for his Coursera Course.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## This function has 4 local functions:
        ## 1. set(), 2. get(), 3. setinv(). and 4. getinv()
        
        inv <- NULL ## default value of inv
        
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse ## caching the value of inv
        
        getinv <- function() inv ## getting cached value
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv() ## get cached inverse
        
        ## the following code executes if the cached inverse exists
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        ## if the inverse has not been cached yet
        data     <- x$get()
        inverse  <- solve(data) ## find the inverse of the matrix
        x$setinv(inverse) ## set the value of the computed inverse in cache
        inverse
}
