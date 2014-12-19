## The below functions allow to create a special object that stores a matrix and its inverse 
## They are intended to avoid repeated computations of the inverse of a matrix 
## and thus they save computational resources  

## The function # 1
##
## the function creates a special "matrix" object that can cache its inverse
## (actually it creates two objects: (1) the original matrix (in the global environment)
## and (2) the list of four functions that provide access to (1)) 
makeCacheMatrix <- function (x = matrix()) {
    
    ## delete the inverted matrix (if it is accessible from within this function)
    matrix.inverted <- NULL
    
    ## define a function that saves the original matrix in the global environmnt
    ## and deletes the inverted matrix if it exists in the global environment
    setOriginalMatrix <- function(input) {
        matrix.stored <<- input
        matrix.inverted <<- NULL        
    }
    
    ## define a function that retrieves the original matrix from the global environment
    getOriginalMatrix <- function() {
        return (matrix.stored)
    }

    ## define a function that gets the inverted matrix as an argument and stores it
    ## in the global environment
    setInvertedMatrix <- function(input) {
        matrix.inverted <<- input
    }

    ## define a function that retrieves the inverted matrix from the global environment
    getInvertedMatrix <- function() {
        return (matrix.inverted)
    }
    
    ## save the original matrix in the global environment
    setOriginalMatrix(x)

    ## create a list of four functions that provide access to the original matrix and 
    ## the inverted matrices
    l <- list (setOriginalMatrix = setOriginalMatrix,
               getOriginalMatrix = getOriginalMatrix,
               setInvertedMatrix = setInvertedMatrix,
               getInvertedMatrix = getInvertedMatrix)
    
    ## return the list as the otput of the function
    return(l)

}

## The function # 2
##
## the function computes the inverse of the special "matrix" returned by the makeCacheMatrix function
## and saves it in the global environment 
## it takes the list created by makeCacheMatrix function as an argument
cacheSolve <- function (x, ...) {
    
    ## make an attempt to retrieve the inverted matrix that might be stored in the global environment
    ## if it exists return it
    matrix.inverted <- x$getInvertedMatrix()
    if(!is.null(matrix.inverted)) {
                message("getting cached data")
                return(matrix.inverted)
    }
    
    ## if the inverse was not computed previously 
    ## retrieve the original matrix, create the inverted matrix and save it
    ## in the global environment
    data <- x$getOriginalMatrix()
    inverse <- solve(data, ...)
    x$setInvertedMatrix(inverse)
    
    ## return the inverted matrix
    return(inverse)

} 
