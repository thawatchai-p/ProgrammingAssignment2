## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        InvMat <- NULL
        setMatrix <- function(y) {
                x <<- y
                InvMat <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) InvMat <<- Solve ## solve(X) will return inverse
        getInverse <- function() InvMat
        
        ## List of matrix containing given matrix set & get and 
        ## inverse matrix get & set. 
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InvMat <- x$getInverse()
        if(!is.null(InvMat)) { ## Checking whether cache already there or not.
                return(InvMat)
        }
        data <- x$getMatrix()
        InvMat <- solve(data, ...) ## solve(X) will return inverse
        x$setInverse(inverse)
}
