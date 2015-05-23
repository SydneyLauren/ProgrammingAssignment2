## The function makeCacheMatrix reads in an invertible matrix x.  It returns a list
## of functions to set/get the matrix and set/get the inverse of the matrix.  This
## information is used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(solve) m <<- solve
    getmatrixinverse <- function() m
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
}


## The function cacheSolve checks if the inverse m of matrix x already exists.
## If it exists, the inverse is return without recalculating it.  If the inverse m
## is null, the solve function is used to calculate the inverse of matrix x, and
## this invers (m) is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getmatrixinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data,...)
    x$setmatrixinverse(m)
    return(m)
}
