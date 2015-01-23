## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Define function for special matrix
    set <- function(y) {                       # Set matrix and inverse matrix
        # Checks two things:
        # 1) if mx (global variable) does not exist, then proceed to solve
        # 2) if mx exist but the new matrix is different from current matrix,
        #    then proceed to solve
        if( !exists("mx")  || !all(mx == x) ){
            mx <<- y
            inv_mx <<- solve(y)
        }
    }
    get <- function() mx                       # Returns the matrix
    getInverse <- function() inv_mx            # Get the inverse matrix
    
    
    # Set matrix on make
    set(x)
    
    # Return list as special matrix
    list(
        set = set,
        get = get,
        getInverse = getInverse
    )
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    makeCacheMatrix(x)$getInverse()
}
