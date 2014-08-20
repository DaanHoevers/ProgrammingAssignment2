## This file contains 2 functions to cache the inverse of a matrix 
## and retrieve if present the cached inverse

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## the function requires an matrix as input and creates a list with 4 functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             # inverse of matrix is set to null
        set <- function(y) {                    # set value of matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     # get value of matrix
        setinv <- function(solve) inv <<- solve # set the value of the inverse
        getinv <- function() inv                # get the value of the inverse
        list(set = set, get = get,              # create list of 4 functions above
             setinv = setinv,
             getinv = getinv)
}


## cashSolve function verifies whether the inverse of matrix input in the 
## makeCacheMatrix already exists
## if yes, the function returns the cached inverse
## if not, the function the creates the inverse and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                       # get the value of the inverse
        if(!is.null(inv)) {                     # if inv not is null, return cached data
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         # if inv is null, get value of matrix
        inv <- solve(data, ...)                 # create inverse of matrix
        x$setinv(inv)                           # sets inverse of matrix in cache
        inv                                     # returns inverse of matrix
}
