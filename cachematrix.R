## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix takes a square, non-singular matrix
## (presumed, withouot any checks added) as input and
## outputs a list of four (closures) named functions:
## "set", "get", "setinv" and "getinv" to set and get
## the values of the input matrix and its inverse. 
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) inv <<- xinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
##
## cacheSolve takes as input the list functions
## associated with a cached input matrix from
## a call to "makeCacheMatrix(x)", retrievs the cached
## inverse matrix if available, or use the "solve()"
## to find and set the cached inverse.  A message is
## printed if a valid cached inverse is found.  In either
## case the cacheSolve returns a valid inverse matrix.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    mat <- x$get()
    iden <- diag(nrow(mat))
    inv_mat <- solve(mat, iden, ...)
    x$setinv(inv_mat)
    inv_mat    
}
