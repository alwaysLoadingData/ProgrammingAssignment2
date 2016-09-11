## CacheMatrix.R contains functions to work with a matrix
## This file adds functionality to be able to cache the value of the
## inverse of the matrix, so that if the value hasn't changed, the 
## cached value will be used instead of re-solving the matrix.


## makeCacheMatrix contains functions for creating and modifying
## the base matrix values

## usage:
## Create matrix list object
## mtx <- makeCacheMatrix(initial_value)

## Update matrix value
## mtx$set(new_value)

## Get matrix value
## mtx$get()

makeCacheMatrix <- function(x = matrix()) {

	cached_inverse <- NULL
	
	get <- function() {
		x
	}
	
	set <- function(new_value) {
		cached_inverse <<- NULL
		x <<- new_value
	}
	
	get_inverse <- function() {
		cached_inverse
	}
	
	set_inverse <- function(inverse) {
		cached_inverse <<- inverse
	}
	
	list(get = get, set = set,
		 get_inverse = get_inverse, set_inverse = set_inverse)

}


## cacheSolve returns the inverse of the base matrix.
## if the matrix has been previously solved, then 
## the cached value is used.

## Usage:
## cacheSolve(mtx)
## mtx - was created by makeCacheMatrix

cacheSolve <- function(x, ...) {
	inverse <- x$get_inverse()

	if(!is.null(inverse))
	{
		message("returning cached data...")
		return(inverse)
	}
	
	inverse <- solve(x$get(), ...)
	x$set_inverse(inverse)
	
	## Return a matrix that is the inverse of 'x'
	inverse
}
