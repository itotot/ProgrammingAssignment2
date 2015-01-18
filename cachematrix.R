## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y){
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverseMatrix <- function(solve) inverse <<- solve
    getInverseMatrix <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverseMatrix()
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverseMatrix(inverse)
    inverse
}