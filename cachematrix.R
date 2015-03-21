## The functions below help reduce the effort involved in
## repeatedly computing the inverse of a square matrix by caching.
## If the inversion is performed on the same matrix, the stored
## inverse is used. If not, the inverse is recomputed.

## The makeCacheMatrix function creates a list object containing
## a matrix which can store (cache) its inverse.

makeCacheMatrix <- function(mat = matrix()) {
    storedInv <- NULL
    
    setMatrix <- function(newMat) {
        mat <<- newMat
        storedInv <<- NULL
    }
    
    getMatrix <- function() {
        mat
    }
    
    setInverse <- function(inputInv) {
        storedInv <<- inputInv
    }
    
    getInverse <- function() {
        storedInv
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function first checks if the input object already
## has a cached value for the inverse. If so, it uses the same.
## If not, it computes the inverse using the solve() function.

cacheSolve <- function(mat, ...) {
    if(class(mat) == "matrix"){
        stop("Please make the matrix cacheable first. 
             Try using the makeCacheMatrix function")
    }
    
    invMat <- mat$getInverse()
    if(!is.null(invMat)) {
        message("Using cached inverse")
        return(invMat)
    }
    
    matData <- mat$getMatrix()
    newInvMat <- solve(matData, ...)
    mat$setInverse(newInvMat)
    newInvMat
}