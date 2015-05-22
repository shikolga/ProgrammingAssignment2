# makeCacheMatrix is a function that returns the list of four functions 
# (set, get, setinverse and getinverse).
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  # i is declared and assigned the value NULL
        set <- function(y) {
                x <<- y  # sets the value of the variable
                i <<- NULL  # the value of i is updated by super-assignment operator 
        } 
        get <- function() x  # gets the value of the matrix
        setinverse <- function(value) i <<- value  # sets the value of inverse matrix (but does not calculate it)
        getinverse <- function() i  # gets the value of inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve computes the inverse of a matrix x
# Using the x$getinverse function it checks if the inverse of x has previously been calculated. 
# If it has, then cacheSolve retrieves the inverse from the cache
# otherwise it computes the inverse, sets the value of inverse in cache, and returns it
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data") 
                return(i)
        }  # if inverse was cached, returns the message and the cached value, no caluclations
        data <- x$get()  # otherwise, computes the inverse
        i <- solve(data) # calculates inverse matrix
        x$setinverse(i)  # calls setinverse function from makeCacheMatrix to cache the inverse
        i
}
