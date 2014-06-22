## The following two methods provide a way to calculate the inverse of a matrix
## and cache it for subsequent use as long as the original matrix is not changed.
## These methods use the features of lexical scoping to cache the inverse of the 
## matrix used.

## This function takes a matrix as a formal parameter and returns a special 
## list with methods to set and get cached inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes a special cacheMatrix list and calls its methods
## to see if the inverse of the matrix is cached.  If yes, then it returns the 
## cached inverse. If no, then the function calculates the inverse and 
## caches the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}