## This pair of functions computes the inverse of a matrix and cache the inverted matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	invermat <- NULL
	set <- function(y) {
		## To set a matrix, replace the old value with the new one
		## and reset the cache
		x <<- y
		invermat <<- NULL
	}
	get <- function() x # Returns matrix
	setinverse <- function(inverse) invermat <<- inverse #Stores the inverse in cache
	getinverse <- function() invermat # Returns invese

	## List of functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the matrix (solve) returned by makeCacheMatrix 
## and stores it in cache
## If the inverse has already been calculated retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	invermat <- x$getinverse()
	## Looks for the inverse in cache returning it if it has arleady been calculated
	if(!is.null(invermat)) { 
		message("getting cached data")
		return(invermat)
	}
	## If it has not been calculated gets the matrix and calculates the invese
	data <- x$get()
	invermat <- solve(data, ...)
	x$setinverse(invermat) # Store the inverse in cache
	invermat
}