## These two functions are written to cache the inverse of an invertible matrix

## Input an invertible matrix and establish the necessary functions used by 
## makecachematrix() 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    list(
        set = function(y) {
            x <<- y
            i <<- NULL
        },
        get = function() {
            x
        },
        setInverse = function(inverse) {
            i <<- inverse
        },
        getInverse = function() {
            i
        }
    )
}


## Solve the matrix and cache the inverse matrix in the function makecachematrix()

cacheSolve <- function(x, ...) {
    # return the cached data if the inverse has already been computed
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    else {
        # otherwise compute the inverse and store it in the cache
        # before returning
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
    }
}