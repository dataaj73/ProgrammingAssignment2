## This file contains functions for calculating the inverse of an invertible
## matrix and caching the result for later use, so that it does not have to
## be recalculated again. makeCacheMatrix forms an object that saves the
## the result and cacheSolve does the actual calculation of the inverse matrix
## or returns the already made calculation.

## The makeCacheMatrix function produces a special matrix, which is 
## actually a list of 4 functions: setmatrix, getmatrix, setinverse and 
## getinverse. As their names suggest these set the value of the matrix, 
## return the value of the matrix, set the value of the inverse matrix and 
## return the value of the inverse matrix. Actually setmatrix is not needed 
## for the calculation of the inverse matrix, but it is needed for checking 
## afterwards if the calculation has already been done or not.

makeCacheMatrix <- function(x = matrix()) {
    ## inv contains the inverse matrix, it is initialized here
    inv <- NULL
    
    ## this changes the value of the matrix whose inverse is wanted and
    ## resets the inverse matrix to NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## return value of the matrix
    getmatrix <- function() x
    
    ## sets value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## return value of the inverse matrix
    getinverse <- function() inv
    
    ## return all the functions defined above as a list
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix formed by the
## makeCacheMatrix function. It checks if the inverse matrix has already been
## calculated and, if it has been calculated previously, it returns the value
## saved from the previous calculation. Otherwise the calculation of the
## inverse matrix happens.

cacheSolve <- function(x, ...) {
    ## get inverse of the matrix x
    inv <- x$getinverse()
    
    ## check if the inverse has been calculated and, if it has, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## get the actual matrix from the makeCacheMatrix object
    m <- x$getmatrix()
    
    ## calculate the inverse
    inv <- solve(m, ...)
    
    ## set value of the inverse matrix
    x$setinverse(inv)
    
    ## Return inverse matrix of 'x'
    inv
}
