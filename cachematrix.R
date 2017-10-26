S## The following functions   are used to create a cache matrix and compute its inverse

## makeCacheInverse is a function that will
##   1. Set the value of the matrix
##   2. Get the value of the matrix
##   3. Set the value of the matrix inverse
##   4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { x
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse) 
        
        }
        
}
           


## cacheSolv ecomputes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has  already been calculated(and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.   Function assumes that the matrix is always invertible


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- Solve(data, ...)
        x$setinverse(inv)
        inv
        }
