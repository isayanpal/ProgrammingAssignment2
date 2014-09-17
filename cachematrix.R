## Matrix inverse operation is generally an expensive
## operation, here we cache the result of the operation
## for a specific matrix

## This function creates a Cache Matrix object which
## has four operations, setting value for the matrix,
## getting value of the matrix, setting and getting
## inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Returns list of operayions for the input matrix
        ## Sets the inverse value to NULL initially
        s <- NULL
        ## Function to set the value of the matrix
        set <- function(y) {
                ## Set the value in target environment
                x <<- y
                ## Set inverse to NULL
                s <<- NULL
        }
        ## Getter function to get the matrix
        get <- function() x
        ## Cache the inverse of given matrix
        setinverse <- function(solve) s <<- solve
        ## Get the inverse of the matrix from cached value
        getinverse <- function() s
        ## Return the special object created as list
        list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function takes a CacheMatrix object
## computes inverse of the matrix if not available
## and caches it else returns already cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse of the matrix x
        m <- x$getinverse()
        ## Check for whether the value is NULL
        if(!is.null(m)) {
                ## If m is not null, cached value available
                message("getting cached data")
                ## return the cached inverse matrix
                return(m)
        }
        ## If cached inverse not available, 
        ## compute inverse, cache it again
        ## Get the original matrix
        data <- x$get()
        ## Compute inverse
        m <- solve(data, ...)
        ## Cache the inverse of the matrix
        x$setinverse(m)
        ## Return inverse of given CacheMatrix
        m
}
