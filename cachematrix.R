## The following functions serve to compute the inverse of an invertible matrix, 
## and then cache that inverse so as to avoid repetitions of the computation.

## This function takes as input an invertible matrix and returns a list of four 
## functions that set and get the input, and then set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        Inv <- NULL
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) Inv <<- inverse
        getsolve <- function() Inv
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function takes as input the output of makeCacheMatrix, and then returns
## the inverse (using solve first, and then getting the cached inverse after)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getsolve()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data)
        x$setsolve(Inv)
        Inv
}
