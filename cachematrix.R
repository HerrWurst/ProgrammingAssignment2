## The functions below enable users to cache the inverse of a matrix
## so that it does not have to be calculated anew every time it's needed.

## The function "makeCacheMatrix" returns a list object containing setter 
## and getter functions for both the original matrix and its inverse (which 
## is not calculated here). Both the original and inverse matrix objects 
## themselves can be accessed, as well. 

makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        setmtx <- function(a) {
                mtx <<- a
                inv <<- NULL
        }
        getmtx <- function() mtx
        setinv <- function(b) inv <<- b
        getinv <- function() inv
        list(setmtx = setmtx, 
             getmtx = getmtx, 
             setinv = setinv, 
             getinv = getinv)
}


## The function "cacheSolve" returns the inverse of a matrix if passed an
## object of type "makeCacheMatrix" (see above). Depending on whether an 
## inverse has already been cached, the cached value is returned or the inverse
## is calculated.

cacheSolve <- function(l, ...) {
        inv <- l$getinv()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        matrix <- l$getmtx()
        inv <- solve(matrix, ...)
        l$setinv(inv)
        inv
}
