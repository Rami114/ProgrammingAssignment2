## 
## Author: Ben Pottier
## Date: 21st of July 2014
##
## Purpose: Provided here is a set of wrapper functions which allow the user to create or set a matrix x and cache a call to solve()
##          Note that similarly to solve, cacheSolve will create the inverse of the Matrix a second matrix is not provided in the arguments
##      
## Known issues: The cache wrapper will always store and return the first call of cacheSolve, irrespective of parameters to solve! 
##               Further calls will return the same output from the cache, _even_ if parameters to cacheSolve are changed. 
##               This can result in incorrect output from the cache depending on the parameters passed through.
##               The current work-around is to explicitly call set() on the wrapped matrix prior to make a secondary call to cacheSolve.
##

## makeCacheMatrix returns a wrapped matrix instance which will cache calls to cacheSolve.
## 
## Parameters:  x - a matrix to wrap, if no matrix is supplied a standard one-column matrix will be created. 
##  
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve allows the calling of solve on a matrix x and will cache the output of the call inside x prior to returning it.
##
## Parameters: x - a matrix wrapped via a call to makeCacheMatrix
##             ... - additional parameters to be passed to solve
##
## Note: the semantics of cacheSolve are the same as those of solve, see ?solve for details. 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        # Solve with a single matrix parameter returns the inverse of said matrix, see ?solve for more information
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
