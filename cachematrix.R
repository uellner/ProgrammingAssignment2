## makeCacheMatrix creates an special matrix object
## and store its inverse after the result has been
## saved and not modified. cacheSolve() checks if
## there is caching. If exists retrieve the inverse from cache,
## if not calculate the inverse and retrieve the result.

## makeCacheMatrix creates an empty matrix and
## returns a list of functions to manage this matrix:
makeCacheMatrix <- function(ma = matrix()){
    m <- NULL
    set <- function(x){
        m <<- NULL
        ma <<- x
    }
    get <- function() ma
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}

## cacheSolve gets a special matrix object
## and returns the inverse of matrix considering...
## the cached value from makeCacheMatrix()
cacheSolve <- function(x, ...){
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}