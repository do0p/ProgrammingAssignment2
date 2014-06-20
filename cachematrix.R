## makeCacheMatrix creates the cacheable input for the cacheSolve 
## function. 

## makeCacheMatrix creates a list of functions which can be used to 
## access the data and the cached result

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        i <<- NULL
        x <<- y
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve tries to get the cached result from the input and, if 
## the cache is empty, calculates the result and fills the cache

cacheSolve <- function(x, ...) {
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
