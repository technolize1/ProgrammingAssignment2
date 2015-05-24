## A pair of functions that cache and inverse matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(setMatrix) {
        x <<- setMatrix
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(calculatedInverseMatrix) inverseMatrix <<- calculatedInverseMatrix
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get, setIM = setInverseMatrix, getIM = getInverseMatrix)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getIM()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setIM(inverseMatrix)
    inverseMatrix
}
