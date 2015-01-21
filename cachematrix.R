## Calculate the inverse of a matrix and cache the results.
## If the matrix has an inverse in the cache it will use the
## results in cache instead of calculating it again.

## This function creates a matrix object that
## has setters and getters for its inverse.
## The getter uses a variable that has been stored in the parent
## environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix. It only
## calculates the inverse if the inverse is not available in 
## cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

