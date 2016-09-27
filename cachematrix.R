## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## sets/resets variable "inv" to NULL
    set <- function(y) { ## sets matrix x to a new matrix y & reset the inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## returns the original matrix x
    setInverse <- function() inv <<- solve(x) #calculates the inverse
    getInverse <- function() inv ## returns the inverse inmat
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse) ## returns the vector of all defined functions
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
