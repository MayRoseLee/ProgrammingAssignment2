## Creates a cache matrix and computes its inverse

## This function creates a special matrix object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,makeCacheMatrix <- function(x = matrix()) 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- mean
        getinverse<- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
           


## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated(and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { Solve(x)
        ## Return a matrix that is the inverse of 'x'
}
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- Solve(data, ...)
        x$setinverse(m)
        m
