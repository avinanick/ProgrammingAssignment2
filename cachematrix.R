## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    changed <- FALSE
    setMatrix <- function(m) {
        x <<- m
        changed <<- TRUE
    }
    setInverse <- function() {
        changed <<- FALSE
        inv <<- solve(x)
    }
    getMatrix <- function() {
        x
    }
    getInverse <- function() {
        inv
    }
    isChanged <- function() {
        changed
    }
    
    list(isChanged = isChanged, getMatrix = getMatrix,
         getInverse = getInverse, setMatrix = setMatrix,
         setInverse = setInverse)
    
}


## Write a short comment describing this function
## This function computes the inverse if it hasn't been done already
## if it has, and the matrix has not changed, returns the inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    changed <- x$isChanged()
    if(!is.null(inv) & !changed) {
        return(inv)
    }
    x$setInverse()
    inv <- x$getInverse()
    inv
}
