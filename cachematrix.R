## Set of two functions: 
## makeCacheMatrix is a factory to create an object containing
## a matrix and it's inverse. It also contains 4 accessors to get/set either the
## matrix or the inverse. 
## cacheSolve is a function that uses an object created with makeCacheMatrix and
## checks if the inverse has been calculated or not. If the inverse has been 
## calculated, it returns it otherwise it will compute the inverse, assign it to
## the cache of the object and return it.

## Creates an object encapsulating a matrix, it's inverse and accessors
## The purpose of storing the matrix along with the inverse 
## is to save from computing the inverse everytime we need it and acts as a cache
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set
         ,get = get
         ,setInv = setInv
         ,getInv = getInv)
}


## Should be used with instance created from the makeCacheMatrix function
## Allows the user to retrieve the inverse of the makeCacheMatrix object
## by looking if the inverse has already been cached or if it needs to be computed
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
