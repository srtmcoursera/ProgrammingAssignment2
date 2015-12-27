## The script creates a special "matrix" object that is able to calculate is
## own inverse matrix. Calculated inverse is cached in order to avoid 
## recalculation if the contents of the matrix have not changed.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(inverse) invMatrix <<- inverse
        getInvMatrix <- function() invMatrix
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## Calculates the inverse matrix of 'x', but returns it from the cache if it
## has been already calculated and the contents of 'x' have not changed.

cacheSolve <- function(x, ...) {
        inverse <- x$getInvMatrix()
        if ( ! is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInvMatrix(inverse)
        inverse
}
