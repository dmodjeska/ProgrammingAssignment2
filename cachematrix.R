## These functions support creating, modifying and solving cached matrices
## The goal is to generate performance improvements for large data sets


## Return a cached matrix with accessors and mutators

makeCacheMatrix <- function(x = matrix()) {
        ## initalize solution
        m <- NULL
        
        ## create accessors and mutators
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        ## return cached matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of argument 'x'

cacheSolve <- function(x, ...) {              
        ## if solution is cached, print message and return cached solution
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if solution is not cached, solve matrix, cache new solution, and return this solution
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
