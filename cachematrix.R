## Next functions are used to produce inverse of provided matrix, 
## cache it and provide the inverse matrix from cache if demanded for second time.

## Function creates variables matrix and inverse 
## and a list of functions that set or get values to these variables
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(z) inverse <<- z
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes list created by makeCacheMatrix function and produces inverse for a matrix 
## stored in variable matrix or take inverse from cache if it exists
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
