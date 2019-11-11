## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will return a List of n functions 
## that allow you cache some values and get it


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function will save in cache the inverse of
## the matrix if this is not yet, after that, you 
## will get the cache value without process it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
