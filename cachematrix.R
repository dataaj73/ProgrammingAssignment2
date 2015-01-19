## This file contains functions for calculating the inverse of an invertible
## matrix and caching the result for later use, so that it does not have to
## be recalculated again. makeCacheMatrix forms an object that saves the
## the result and cacheSolve does the actual calculation of the inverse matrix.

## The makeCacheMatrix function produces a special matrix, which is 
## actually a list of 4 functions: setmatrix, getmatrix, setinverse and 
## getinverse. These set the value of the matrix, return the value of the 
## matrix, set the value of the inverse matrix and return the value of the
## inverse matrix. Actually for the calcution of the inverse matrix only
## the setmatrix is not needed, but it may be useful to have for other
## purposes.

makeCacheMatrix <- function(x = matrix()) {
    ## inv contains the inverse matrix, it is initialized here
    inv <- NULL
    
    ## this sets the value of the matrix whose inverse is wanted
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## this returns the value of the matrix whose inverse is wanted
    getmatrix <- function() x
    
    ## this sets the value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## this returns the value of the inverse matrix
    getinverse <- function() inv
    
    ## function returns all the functions defined above as a list
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix formed by the
## makeCacheMatrix function. It checks if the inverse matrix has already been
## calculated and, if it has been calculated previously, it returns the value
## saved from the previous calculation. Otherwise the calculation of the
## inverse matrix happens.

cacheSolve <- function(x, ...) {
    ## get the inverse of the matrix x
    inv <- x$getinverse()
    
    ## check if the inverse has been calculated and if it has been return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## get the actual matrix from the makeCacheMatrix object
    m <- x$getmatrix()
    
    ## calculate the inverse
    inv <- solve(m, ...)
    
    ## set the value of the inverse matrix
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
