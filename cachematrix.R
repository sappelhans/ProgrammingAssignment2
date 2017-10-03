## Put comments here that give an overall description of what your
## functions do

## Function to store a matrix and its inverse
## Returns a list of setter/getter functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(invMatrix) inv <<- invMatrix
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to calculate and set the inverse matrix
## on the makeCacheMatrix ojbect. If the inverse matrix
## is already cached it will skip the calculation.
## Returns the newly calculated or cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setInv(i)
    i
}
