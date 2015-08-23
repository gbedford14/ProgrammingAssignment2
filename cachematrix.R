## functions makeCacheMatrix() and cacheSolve() create a list of functions 
## containing a matrix and then caches the inverse of the matrix for
## re-use

## makeCacheMatrix takes a matrix as input and caches it within a 
## list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## function cacheSolve checks to see if the inverse has been calculated
## and returns the result of the prior calculation or calculates the inverse
## if it hasn't been already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
