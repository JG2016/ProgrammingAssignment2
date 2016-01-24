## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
## Using the sample provided in the class, chaging meanto solve
makeCacheMatrix <- function(x = matrix()) {
        # Initilize insverse to NULL
        my_inverse <- NULL 
        set <- function(y) {
                # Use the <<- operator tp cache the values
                x <<- y
                my_inverse <<- NULL
        }
        get <- function() x
        set_matrix <- function(solve) my_inverse <<- solve
        get_matrix <- function() my_inverse
        list(set = set, get = get,
             set_matrix = set_matrix,
             get_matrix = get_matrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        # Initialize my_inverse to the original matrix
        my_inverse <- x$get_matrix()
        if(!is.null(my_inverse)) {
                message("getting cached data")
                return(my_inverse)
        }
        mydata <- x$get()
        my_matrix <- solve(mydata, ...)
        x$set_matrix(my_matrix)
        my_matrix
}
