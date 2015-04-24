## Creates an inverse matrix and then stores the solution in a separate function. If the same matrix is called for,
## the stored solved matrix is returned without the need for recalculation.

## Creates an inverse of a matrix and then returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Stores an inverse matrix. If solved matrix exists it will return it,
## otherwise it will call makeCacheMatrix to create a new inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
