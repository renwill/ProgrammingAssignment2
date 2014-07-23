## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- functon() x
    getInverse <- function() i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    setInverse <- function(inverseX) i <- inverseX
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached inverse of the matrix")
        return(i)
    }
    m <- x$get()
    i <- solve(m)
    x$setInverse(i)
    i
}
