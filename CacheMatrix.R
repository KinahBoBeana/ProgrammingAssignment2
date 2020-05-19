## ProgrammingAssignment2
## A pair of functions that cache the inverse of a matrix

## This function creates a special object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        set <- function (y){
                x <<- y
                p <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) p <<- inverse
        getinverse <- function() p
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        p <- x$getinverse()
        if(!is.null(p)) {
                message("getting cached data")
                return(p)
        }
        data <- x$get()
        p <- solve(data, ...)
        x$setinverse(p)
        p
}

