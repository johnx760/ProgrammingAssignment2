## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of
## a matrix rather than computing it repeatedly

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(M) {
       x <<- M
       INV <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) INV <<- inverse
    getinverse <- function() INV
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated
## (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Obtains cached result if available
    INV <- x$getinverse()
    if(!is.null(INV)) {
        message("getting cached data")
        return(INV)
    }
    data <- x$get()
    INV <- solve(data, ...)
    x$setinverse(INV)
    INV
}