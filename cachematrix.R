## The functions below are used to cache a matrix's inverse
## Since matrix inverse function are computationally expensive, 
## it is usefull to cache the inverse of those matrices which are used repeadly.

## The function below(makeCacheMatrix) takes in a (invertible) matrix 
## and return a list of functions to set data, get data, getInverse, setInverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setInverse <- function(inv) { m <<- inv }
        getInverse <- function() { m }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function takes in an object returned by the makeCacheMatrix 
## to calculate the inverse of the matix if it does not exist (using the setInverse function)
## else retrieves the already calculated inverse from the cache and returns it.

cacheInverse <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting inverse-ed matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

