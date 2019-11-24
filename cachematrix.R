## Put comments here that give an overall description of what your
## functions do:
##
## These functions were elaborated following the same methodology
## of 'makeVector' and 'cachemean'

## makeCacheMatrix was built similar to makeVector defined as an
## example in the assignment course

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inverse <- function(i) m <<- solve(x)
	get_inverse <- function() m
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve was built similar to cachemean defined as an
## example in the assignment course

cacheSolve <- function(x, ...) {
	m <- x$get_inverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$set_inverse(m)
	m
}