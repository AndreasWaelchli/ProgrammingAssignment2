## Put comments here that give an overall description of what your
## functions do
## There are two functions:
## makeCacheMatrix: This function creates a special "matrix" (it is actually a
##      list) object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##      returned by makeCacheMatrix above. If the inverse has already been
##      calculated, then cacheSolve should retrieve the inverse from the cache.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # initialise the variable that stores the inverse
    inv <- NULL
    
    # create a function that returns the original input
    get <- function() x
    
    # create a function that assigns its input to the variable 'inv' in the 
    #       "parent" environment
    setinverse <- function(z) inv <<- z
    
    # create a function that returns the variable 'inv' from the parent env
    getinverse <- function() inv
    
    # the function gives back a list of functions
    list(get = get, setinverse = setinverse, getinverse = getinverse)

}

# the function cacheSolve expects a "special matrix" created by the function
#   makeCacheMatrix() and returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    # Use the getinverse() function
    inv <- x$getinverse()

    # check if the inverse has already been computed:
    if(!is.null(inv)) { ## Yes, inverse exists
        # print a message, letting the user know that a cached value is used
        message("getting cached data")
        # exit the function and return the inverse
        return(inv)
    } else { ## No, inverse does not yet exist
        # Get the original matrix
        matr <- x$get()
        
        # Calculate the inverse of this matrix
        inv <- solve(matr, ...)
        
        # Use setinverse() of the "special matrix" object to assign the inverse
        x$setinverse(inv)
    }
    # Return the inverse
    inv
}
