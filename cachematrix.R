## Assignment 2
## Cache the inverse of a matrix

## Sample use cases
## Case 1 (with lazy initialization)
##     x <- matrix(c(1,2,3,4),2,2)
##     xCached <- makeCacheMatrix(x, lazyInit=TRUE)
##     xInverse <- cacheSolve(xCached) ## Inverse calc'ed and stored into cache
##     xInverse <- cacheSolve(xCached) ## Inverse read from cache
##     xCached$set(matrix(c(5,6,7,8),2,2)) 
##     xCached$set(matrix(c(9,10,11,12),2,2))
##     xInverse <- cacheSolve(xCached) ## Inverse calc'ed and stored into cache
## Case 2 (without lazy initialization)
##     x <- matrix(c(1,2,3,4),2,2)
##     xCached <- makeCacheMatrix(x, lazyInit=FALSE) ## Inverse calc'ed and stored into cache
##     xInverse <- cacheSolve(xCached) ## Inverse read from cache
##     xInverse <- cacheSolve(xCached) ## Inverse read from cache
##     xCached$set(matrix(c(5,6,7,8),2,2)) ## Inverse calc'ed and stored into cache
##     xCached$set(matrix(c(9,10,11,12),2,2)) ## Inverse calc'ed and stored into cache
##     xInverse <- cacheSolve(xCached) ## Inverse read from cache


##
## makeCacheMatrix : Accepts an invertible matrix and caches its inverse
## Input           : x <- Invertible matrix
##                   lazyInit <- logical, set true if you want to enable lazy initialization. 
##                   If false, constructor/setter are computationally expensive. 
##                   If true, computation load falls on first getInverse call.
## Output          : List used to interact with cache
##                   Returned list's elements
##                   $set/$get: setter/getter functions for the matrix whose inverse is cached
##                   $getInverse: getter function for the cached inverse matrix
##                   $init: lazy initialization of inverse matrix
makeCacheMatrix <- function(x = matrix(), lazyInit = FALSE) {
    if (lazyInit == FALSE) {
        message(msg_CallingSolve)
        xInverse <- solve(x)
        isCacheInitialized <- TRUE
    } else {
        ## inverse is not initialized
        inverse <- matrix()
        isCacheInitialized <- FALSE
    }
    ## setter
    set <- function(y) {
        x <<- y
        if (lazyInit == FALSE) {
            message(msg_CallingSolve)
            xInverse <<- solve(x)
            isCacheInitialized <<- TRUE
        } else {
            ## matrix has changed but inverse has not been re-calculated because of lazy initialization
            warning(warn_SetOnLazyInit)
            isCacheInitialized <<- FALSE
        }
    }
    ## getter
    get <- function() {
        x
    }
    ## getter for cached matrix
    getInverse <- function() {
        if (!isCacheInitialized) {
            ## matrix value changed but inverse not re-calculated
            stop(error_cacheNotInitialized)
        }
        if (all(is.na(x))) {
            warning(warn_InverseOfEmptyMatrix)
        }
        xInverse
    }
    ## init function for lazy initialization
    init <- function() {
        message(msg_CallingSolve)
        xInverse <<- solve(x)
        isCacheInitialized <<- TRUE
    }
    ## returned list object
    list(set = set, get = get,
         getInverse = getInverse,
         init = init)
}

##
## cacheSolve : calculates the inverse of the special "matrix" returned by makeCacheMatrix
## Input      : x <- List object returned by makeCacheMatrix
## Output     : Inverse of x$get() as stored in cache
cacheSolve <- function(x, ...) {
    if(!all(labels(xCached) == c("set","get","getInverse","init"))) {
        stop(error_UnexpectedObject)
    }
    if (!(nrow(x$get()) == ncol(x$get()))) {
        stop(error_NonSquareMatrix(x$get()))
    }
    result <- tryCatch(
        {
            xInverse <- x$getInverse()
        }, 
        error = function(err) {
            if (identical(err$message, error_cacheNotInitialized)) {
                ## matrix inverse does not exist in cache
                x$init()
                xInverse <- x$getInverse()
            } else {
                stop(err)
            }
        },
        finally = {
            return (xInverse)
        })
}


##
## Warnings, Errors and Messages
##
warn_SetOnLazyInit <- "Changing matrix value with lazy Initialization enabled! $init must be called before $getInverse for correct value. Ignore this warning if getting inverse via cacheSolve."
warn_InverseOfEmptyMatrix <- "Requesting inverse of empty matrix. Returned inverse is an empty matrix."
error_cacheNotInitialized <- "Unexpected state. Requested inverse when cache is not initialized. Init must be called before getInverse if lazy initialization is enabled."
error_NonSquareMatrix <- function(m) {
    paste("Supplied matrix must be invertible! Provided matrix has", nrow(m), "rows and", ncol(m), "columns.", sep=" ")
}
error_UnexpectedObject <- "Unexpected input for cachedSolve. Parameter 'x' must be a list object returned by makeCacheMatrix"
msg_CallingSolve <- "Calling solve() and caching result"
