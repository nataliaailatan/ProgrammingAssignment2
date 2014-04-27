## This function creates a list of 4 functions to store in cache the inverse
## of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL	
        set <- function(y) {				# function to set the matrix and initialize...
                x <<- y					# ... the inverse
                i <<- NULL
        }
        get <- function() x				# function to return the matrix
        setinv <- function(inv) i <<- inv		# function to compute the inverse of the matrix
        getinv <- function() i			# function to return the inverse of the matrix
        list(set = set, get = get,			# List with the 4 functions
             setinv = setinv,
             getinv = getinv)

}



## This function returns the inverse of a given matrix, either getting it 
## from cache or computing when it's not stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
								
        i <- x$getinv()					# Gets the inverse of the matrix
        if(!is.null(i)) {				# If it already exists, it returns it and stop computing
                message("getting cached data")
                return(i)
        }							# Otherwise, the function continues computing
        data <- x$get()					# Gets the matrix
        i <- solve(data, ...)				# Computes the inverse of the matrix
        x$setinv(i)					# Stores the result in the cache
        i							# Returns the result
}
