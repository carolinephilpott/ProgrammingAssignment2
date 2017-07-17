## A pair of functions that cache the inverse of a matrix

## This function creates a special list object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse
        setsolve <- function(solve) m <<- solve
        
        ## get the value of the inverse
        getsolve <- function() m
        
        ## what to return
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "vector" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
         m <- x$getsolve()
        
        if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}
