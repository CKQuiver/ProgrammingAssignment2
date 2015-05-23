## From CKQuiver for Coursera R Programming Class (Johns Hopkins) 
## Week 3, Programming Assignment 2.  Due 24 May 2015  4:30pm PST
## 
## These two functions cache the inverse of a matrix, in the style of makeVector and cacheMean.
## Matrix supplied must be invertible.
##
## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmat <<- solve
        getinv <- function() invmat

        ## make it all available to cacheSolve by putting in a list
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ## First check that there is a value for invmat.
        ## If so, return that value and end the function.

        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        
        ## If there is nothing cached for invmat, then calculate and return the inverse.
        data <- x$get()
        invmat <- solve (data, ...)
        x$setinv(invmat)
        invmat
}
