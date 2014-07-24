# Description: 
##  makeCacheMatrix creates a list of functions to store & retrieve a matrix & an optional LHS column vector of a linear system and the solution
## If the optional RHS column vector is missing, the cached solution represents the inverse of the matrix
## The output list is for use with the cacheSolve function to cache the solution to avoid re-computation when we need the solution again
# Usage
## makeCacheMatrix(a, b = NULL)
# Arguments
## 'a' is the matrix to cache 
## 'b' is an optional right-hand-side column vector of the linear system
# Return
## the result return is a list of four functions 
## - set(a, b): set the matrix 'a' and the optional RHS column vector 'b'
## - get() returns the matrix cached
## - getRHS() returns the RHS vector cached
## - setLinearSolution(i): sets the computed solution i (which is the matrix inverse if 'b' is null)
## - getLinearSolution() returns the cached solution (which is the matrix inverse if 'b' is null)
makeCacheMatrix <- function(a = matrix(), b = NULL) {
    i <- NULL
    get <- function() a
    getRHS <- function() b
    getLinearSolution <- function() i
    set <- function(y, z = NULL) {
        a <<- y
        b <<- z
        i <<- NULL
    }
    setLinearSolution <- function(solveLinear) i <<- solveLinear
    list (set = set, get = get, getRHS = getRHS, setLinearSolution = setLinearSolution, getLinearSolution = getLinearSolution)
}

# Description
## cacheSolve provides the solution of a %*% y = b for y if b is not null 
##            and returns the inverse matrix of a if b is null,
## where 'x' is special list of functions returned from makeCacheMatrix(a,b)
## If the solution has been computed before, it will not be recomputed and the
## cached result will be returned
# Usage
## cacheSolve(x)
# Arguments
## 'x' is the list of functions returned from makeCacheMatrix(a, b)
# Return
## * the solution of a %*% y = b for y if b is not null
## * the inverse matrix of a           if b is null
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getLinearSolution()
    if (!is.null(i)) {
        message("getting the cached result")
        return(i)
    }
    a <- x$get()
    b <- x$getRHS()
    if (is.null(b)) {
        i <- solve(a, ...)
    } else {
        i <- solve(a, b, ...)
    }
    x$setLinearSolution(i)
    i
}
