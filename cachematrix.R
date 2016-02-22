makeCacheMatrix <- function(x = matrix()) {
        ## x is a maxtrix that is invertible
        
        ## Function will set the value of the matrix,    
        ## determine its inverse, and cache the result.

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <-function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        ## x is an invertible matrix

        ## This function first checks to see if the inverse of x   
        ## has been cached, and if so, returns it.  Otherwise, it
        ## determines the inverse, stores it to cache, and then
        ## returns it.
        
        m <-x$getsolve()
        if (!is.null(m)) {
                message("Returned cached inverse of x")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}