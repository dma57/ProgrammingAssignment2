## the following two functions have been designed to avoid calculating the inverse of a matrix repeatedly
## once calculated, the inverse matrix is stored in a cache

## makeCacheMatrix receives an invertible matrix and is able to store its inverse in a cache
## it returns a list (special "matrix" objet) which gives access to the matrix itself 
## and its inverse. The inverse is initially set to NULL
## the returned list contains functions to:
##      get/set the matrix
##      get/set the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(m){
        x <- m
        inverse <- NULL
    } 
    get <- function() x
    
    setInverse <- function(m) inverse <<- m
    getInverse <- function() inverse
    
    list(get=get, set=set, getInv = getInverse, setInv = setInverse)
}


## cacheSolve receives a special "matrix" object x (returned from makeCacheMatrix function) 
## as its first argument, and returns the inverse of the matrix contained in the special "matrix" object
## if the inverse has not been calculated before, cacheSolve calculates it, and stores it in the cache of the
## special "matrix" object

## example:
## m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
## x <- makeCacheMatrix(m)
## y <- cacheSolve(x)       # first call: the inverse matrix is calculated and stored in the cache
## y <- cacheSolve(x)       # second call: the inverse matrix is just retreived from the cache (as indicated by the message)
## getting cached data

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getInv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    # regular matrix extracted from the special "matrix" object
    data <- x$get()
    # inverse matrix calculated
    minv <- solve(data, ...) 
    # inverse matrix stored in the cache
    x$setInv(minv)
    
    # inverse matrix returned
    minv
}
