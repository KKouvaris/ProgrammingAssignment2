## Matrix inversion is usually a costly computation. 
## The following pair of functions are used to cache 
## the inverse of a matrix improving computational performance
## in situations when the inverse of a matrix has to be computed
## repeatedly.

## The following function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a set of functions: set, get
## setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_Matrix <- NULL #initialise the inverse of the matrix to NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        # re-initialise the inverse of the matrix to NULL 
        # with any new matrix assignments
        inverse_Matrix <<- NULL 
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the value of the inverse of the matrix
    setinverse <- function(inv) inverse_Matrix <<- inv
    
    #get the value of the inverse of the matrix
    getinverse <- function() inverse_Matrix
    
    #return a list of the respective functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function, makeCacheMatrix.
## If the inverse of the matrix has already been calculated, 
## the function gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix of the data and sets
## the respective value of the mean in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_Matrix <- x$getinverse()
    
    # If the inverse of the matrix already exists in cache
    # return it without calculating it again
    if(!is.null(inverse_Matrix)) {
        message("getting cached data")
        return(inverse_Matrix)
    }
    
    # If not, calculate the inverse of the matrix
    # using the 'solve' in-built function 
    # and set it in the cache
    data <- x$get()
    inverse_Matrix <- solve(data, ...)
    x$setinverse(inverse_Matrix)
    inverse_Matrix    
}
