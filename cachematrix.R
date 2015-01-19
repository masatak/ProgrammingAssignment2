
## Cached Inverse Matrix Functions. 
## In order to avoid the iterative calculation of the inverse of 
## the same matrix, define functions that provide the ability to
## implicitly save and reuse the calculated inverse matrix.

## Transform a matrix to a cached matrix. Using with 'cacheSolve'
## function, the pseudo matrix made by this functions allow you
## to cache the calculated inverse matrix. 
## Internally, this returns a list of functions to access variables
## in the closure which saves the original matrix named 'x' and  
## the inverse matrix named 'inverse'.
 
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse parameter in closure.
    inverse <- NULL
    
    ## Define function to set new matrix in closure.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## Define function to get original matrix from closure. 
    get <- function() x
    
    ## Define function to set inverse matrix in closure.
    setInv <- function(inv) inverse <<- inv
    
    ## Define function to get inverse matrix from closure.
    getInv <- function() inverse
    
    ## Return list of functions to access variables in closure.
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Using the pseudo matrix made by 'makeCacheMatrix' function, 
## perform the calculation of the inverse matrix. 
## Once calculating the inverse matrix of the same matrix, 
## this function caches the result. Afterward, without 
## calculating the inverse matrix, it will work in high speed 
## using the cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check if the inverse matrix has already calculated.
    m <- x$getInv()
    if(!is.null(m)) { ## The inverse matrix has cached.
        message("getting cached data")
        return(m)     ## return the cashed value.
    }
    
    ## When the inverse matrix has not calculated,
    ## get the original matrix, inverse it and 
    ## save the result in order to use later.
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    
    m  ## return the inverse matrix.
}
