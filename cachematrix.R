## This script contain a pair of function who can cache the inverse of
## a regular matrix
## **!** I'm not an english native, sorry for the lingue mistakes

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y		# the <<- operator assigns the value on the right side to the object on the parent environment, x
		s <<- NULL		# clears any value of i that has been chached by a prior execution of cacheSolve() 
	}
	get <- function() x	# symbol x is not defined whithin get(). It is retrieved from the parent environment
	setsolve <- function(solve) s <<- solve	# s is defined in the parent env. and needs to be accessed after setmatrix() completes. The <<- operator assigns the input value to the value of s in the parent env.
	getsolve <- function() s
	# List who contain all the variables that we will need for invert
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)			# assigns each of these functions a named element within a list, returning it to the parent env.
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

## **!** If the matrix who we pass how arg isn't regular, R print a error message

cacheSolve <- function(x, ...) {
	## s <- the inverse of the matrix who we pass like arg 
	s <- x$getsolve()
	## Case when we already just have the inverse in cache
	if(!is.null(s)) {
		message("getting cached data")
		# return the invert
		return(s)
	}
	## Case when we haver to invert the matrix and cache for future times
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	# The next exp is the last exp. Return the invert
	s
}
