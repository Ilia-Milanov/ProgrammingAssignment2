## This pair of functions are for caching the result of inversing a matrix

## makeCacheMatrix function return a special matrix, which is a list with four 
## functions:
##      set – set the value of the matrix
##      print – print the value of the matrix
##      setInverse – set the value of the inverse matrix
##      printInverse – print the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(...){
        x <<- matrix(...)
        inverse <<- NULL
    }
    print <- function() x
    setInverse <- function(inv) inverse <<- inv
    printInverse <- function() inverse
    list(set=set, print=print, setInverse=setInverse, printInverse=printInverse)
}

## cacheSolve function return matrix that is the inverse of the special matrix 
## created by makeCacheMatrix. 
## If the matrix is already solved, then returns it from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$printInverse()
    if(!is.null(inv)) {
        message("using cached data")
        return(inv)
    }
    data <- x$print()
    inv <- solve(data)
    x$setInverse(inv)
    x$printInverse()
}
