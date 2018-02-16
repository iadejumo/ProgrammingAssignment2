## The following functions work together to create and cache the 
## inverse of a matrix and hence save time in computations

## Takes a matrix as an input and creates a special matrix object
## which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function(){inv}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Take a matrix as an input and computes returns the inverse of the special matrix.
## If the inverse has alreadyben computed and the matrix remained unchanged
## then it retrieves the cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
