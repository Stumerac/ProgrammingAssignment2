## cachematrix.R creates a matrix and solves and caches the inverse for
## later use.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## sets the matrix to a new matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## retrieves the matrix
    get <- function() x
    ## solves for the matrix's inverse
    setInverse <- function(solve) m <<- solve
    ## retrieves the inverse of the matrix
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Solves/retrieves the inverse of the matrix by using the cached
## inverse if it's been cached, and by using the solve() function if
## the inverse has not been cached

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## Uses cached inverse if cached copy exists.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##solves inverse if cache does not exist
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
