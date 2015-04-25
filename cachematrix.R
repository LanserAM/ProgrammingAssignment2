## The functions in this file create a "matrix" object and then calculates the 
# inverse, but only after checking for the inverse in the cache. If the inverse is 
# not cached, then it will be calculated and cached. 


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve() computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        #'x' is assumed to be a  square, invertible matrix.

        #check cache for inverse
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Message: getting cached data")
                return(inv)
        }
        
        #solve for inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
