## This code file contains two funcitons.  One to construct a custom Matrix 
## capable of storing its inverse and the other to retrieve that inverse if it
## exists or to create it.

## makeCacheMatrix - custom Matrix with four internal functions
## - getMatrix :- returns the Matrix
## - setMatrix :- sets the Matrix
## - getInverse :- returns the inverse of the Matrix
## - setInverse :- sets the inverse of the Matrix
makeCacheMatrix <- function(thisMatrix = matrix()) {
    thisInverse <- NULL
        
    ## function to store the Matrix
    setMatrix <- function(newMatrix) {
        ## avoids storing the Matrix and clearing the inverse if the supplied
        ## Matrix and the stored Matrix are identical
        if (dim(thisMatrix) == dim(newMatrix) && all(thisMatrix == newMatrix)){
            message("Matrix has not changed, preserving cached values")
        }
        else{
            thisMatrix <<- newMatrix
            thisInverse <<- NULL
        }
    }
    
    getMatrix <- function() thisMatrix
    
    setInverse <- function(theInverse) thisInverse <<- theInverse
    
    getInverse <- function() thisInverse
    
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         setInverse = setInverse, 
         getInverse = getInverse,)
}


## cacheSolve - Returnes the inverse of a given custom Matrix object.
cacheSolve <- function(thisMatrix, ...) {

    ## retrieves stored inverse and checks for content to avoid unnecessary
    ## recalculation 
    thisInverse <- thisMatrix$getInverse()
    if(!is.null(thisInverse)) {
        message("Retrieving cached data")
        # exit function returning existing value
        return(thisInverse)
    }
    
    ## calculates, stores and returns inverse for new Matrix
    thisInverse <- solve(thisMatrix$getMatrix(), ...)
    thisMatrix$setInverse(thisInverse)
    thisInverse
}
