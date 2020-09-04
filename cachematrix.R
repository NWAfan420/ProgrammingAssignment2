## Script to define the functions makeCacheMatrix and
## cacheSolve, which cache a matrix and its inverse to
## allow quick access to the latter without repeated
## computation.
## The provided matrix must be solvable!

## makeCacheMatrix - This function accepts one
## parameter, x = matrix(), and stores the object
## (assumed to be a matrix). It returns a list
## of four functions, set, get, setinv and getinv,
## which set the matrix, retrieve the cached matrix,
## and set/get the matrix's cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(new_inv) inv <<- new_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve - This function takes the list object
## created by makeCacheMatrix as an argument, and
## returns the inverse of the cached matrix. If this
## inverse is also cached, this result is used - else
## the matrix inverse is calculated and cached for
## future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
