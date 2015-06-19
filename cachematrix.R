## These two functions will cache the inverse of a matrix.
## Calculating the inverse of a matrix is an expensive operation
## and being able to cache it will be greatly benefitial for
## overall performance

## Function "makeCacheMatrix" receives as parameter a matrix and
## returns a list of functions associated with it  
## that will set the value of the matrix, 
## get the value of the matrix, set the value of the
## inverse of the matrix and get the value of the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Function "cacheSolve" receives as parameter a "matrix"
## created with the function "makeCacheMatrix".  It calculates
## the inverse of the matrix, however if the inverse has already 
## been calculated, it returns the cached value instead of
## executing the calculation again

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
