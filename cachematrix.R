## The functions makeCacheMatrix and cacheSolve allow you to create a matrix
## that can then be set or retrieved, as well as find the inverse of the
## matrix and cache it for faster retrieval in the future.

## makeCacheMatrix returns a list of functions that allow you to
## create, set, and retrieve a matrix as well as cache and 
## retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse 
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve will return the inverse of a matrix created 
## with makeCacheMatrix.  If the inverse is cached, it returns the
## cached value, otherwise it finds the inverse and caches it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
            message("Retrieving cached inverse")
        }
        else{
            x_matrix <- x$get()
            inverse <- solve(x_matrix, ...)
            x$setinverse(inverse)
        }
        inverse
}
