## Put comments here that give an overall description of what your
## functions do
## The following functions are to be used together to create a square invertible matric
## and make the inverse available in the cache so that it does not have to be recomputed

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	cached = NULL

	set = function(y) {
		x <<- y
		cached <<- NULL
	}

	get = function() x
	setinverse = function(inverse) cached  <<- inverse
	getinverse = function() cached

	## return the list of functions
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.  If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## see if the inverse has already been cached
	cached <- x$getinverse()

	## return the inverse if it exists
	if (!is.null(cached)) { 
		message("Retrieving Data from Cache")
		return(cached)
	}

	##does not exist - calculate the inverse
	message("Calculating Matrix Inverse")
	m = x$get()
	inverse = solve(m, ...)

	x$setinverse(inverse)

	return(inverse)
	
}
