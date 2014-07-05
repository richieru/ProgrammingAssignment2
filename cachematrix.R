## creates a list that gets a matrix, sets a matrix, gets the inverse and sets the inverse, caches the inverse and searches for the inverse before attempting to calculate it again.

## Creates a list of functions to be used in cacheSolve

makeCacheMatrix <- function(x) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## searches m to see if the inverse already exists, if not it will calculate the inverse of the matrix cached in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinv(m)
    m
}
        ## Return a matrix that is the inverse of 'x'

