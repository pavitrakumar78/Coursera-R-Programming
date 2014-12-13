## The functions below are used to cache a matrix's inverse
## Since matrix inverse function are computationally expensive, 
## it is usefull to cache the inverse of those matrices which are used repeadly.

## The function below(makeCacheMatrix) takes in a (invertible) matrix 
## and return a list of functions to set data, get data, getInverse, setInverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # function that sets the given matrix 'y' to 'x'
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # function to retrieve the exisiting matrix
        get <- function() { x }
        # function to set the inverse of the matrix to 'm'
        setInverse <- function(inv) { m <<- inv }
        # function to return the inverse of matrix i.e 'm'
        getInverse <- function() { m }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function takes in an object returned by the makeCacheMatrix 
## to calculate the inverse of the matix if it does not exist (using the setInverse function)
## else retrieves the already calculated inverse from the cache and returns it.

cacheInverse <- function(x, ...) {
        # 'm' now holds the inverse (if it exists) returned by the getInverse() function
        m <- x$getInverse()
        if(!is.null(m)) {
                # if inverse is already calculated, then return it
                message("getting inverse-ed matrix")
                return(m)
        }
        # else, get the original matrix using the get() function
        data <- x$get()
        # calculate the inverse using R's solve() method
        message("calculating inverse...")
        m <- solve(data, ...)
        # set the inverse using setInverse() method
        x$setInverse(m)
        m
}

#EXAMPLE:
x = matrix(c(1,2,3,4,11,34,78,8,9),ncol = 3,nrow = 3)
m_c <- makeCacheMatrix(x)
# first call to cache the matrix, the inverse is created in this call
cacheInverse(m_c)
# now the nex time we want to use the inverse, we don't need to calculate it again
# since it was already calculated, the existing inverse is returned
cacheInverse(m_c)
