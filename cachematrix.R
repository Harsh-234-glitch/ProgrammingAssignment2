## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Variable to store the inverse of the matrix
    
    set <- function(y) {
        x <<- y    # Update the matrix
        inv <<- NULL # Reset the inverse
    }
    
    get <- function() x # Return the matrix
    
    setInverse <- function(inverse) inv <<- inverse # Cache the inverse
    
    getInverse <- function() inv # Return the cached inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse) # Return a list of the functions
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse() # Get cached inverse, if available
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv) # Return cached inverse
    }
    
    mat <- x$get() # Get the matrix
    inv <- solve(mat, ...) # Compute the inverse
    x$setInverse(inv) # Cache the computed inverse
    inv # Return the inverse
}

