## These functions are used with matricies in order to save the inverse with
## the matrix once it is calculated.  This saves computation time for large 
## matricies.

## function makeCacheMatrix creates a cached data structure to hold the
## matrix and its inverse.  Once the inverse is calculated, it can be stored
## and retrieved from cache.  It also checks if the matrix is numeric
## and square.

makeCacheMatrix <- function(x = matrix()) {
    if (!is.numeric(x)) {
        message("The input matrix must be numeric")
        invisible(x)
        return
    }
    if (nrow(x) != ncol(x)) {
        message("The number of rows and columns must be the same")
        invisible(x)
        return
    }
    # Create the caching data structure
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## function cacheSolve uses the makeCacheMatrix data structure to check 
## first if an inverse has already been calculated. If so, it returns
## the stored version. If not, it calculates the inverse and puts it
## into the data structure.  It also catches any errors on the solve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    inv <- tryCatch({solve(data, ...)},
        error=function(cond) {
        message("Unable to calculate an inverse for this matrix")
        return(NA)})
    
    x$setinv(inv)
    return(inv)
}
