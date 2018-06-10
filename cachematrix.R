## The makeCacheMatrix and casheSolve functions are used to create a special object that 
#computes the inverse of a matrix and cache's this inverse.

## The makeCacheMatrix function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The casheSolve function calculates the inverse of the matrix returned by makeCacheMatrix function. 
#If the inverse has already been calculated, then the cacheSolve retrieves the inverse from the cache. 
#If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

