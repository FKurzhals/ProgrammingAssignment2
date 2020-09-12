## The following functions enable you to cache the
## inverse of a matrix and then return the result
## without the need of repeating the possibly costly
## computation on further iterations

## The 'makeCacheMatrix' function creates a special
## matrix object that can cache itself

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inv) x_inv <<- inv
        get_inv <- function() x_inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## The 'cacheSolve' function computes the inverse of the
## previously created special matrix. If the inverse has
## already been calculated and no changes were made to the
## matrix, then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$get_inv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$set_inv(x_inv)
        x_inv
}
