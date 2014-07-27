## makeCacheMatrix - Makes an inverse caching matrix.
##
## cacheSolve - Gets the inverse of an inverse caching matrix,
##              using the cached inverse if it exists.

## Supports the setting and getting of a matrix and its inverse.
## When the matrix value setter is called, it sets the cached 
## inverse to NULL. Returns the setter and getter functions.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinv <- function(i) {
        inv <<- i
    }
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Uses the supplied inverse caching matrix to supply the
## inverse. If the cached inverse is NULL, computes the
## inverse, caches it, and returns it. Otherwise, just return
## the cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinv(i)
    i
}
