## The function makeCacheMatrix creates a special "matrix", a list containing 
## a function to set the value of the matrix, and get the value of the matrix, 
## then set the value of the inverse matrix, and get the value of the inverse 
## matrix and cache the inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	matrix_inv <- NULL
	set <- function(y) {
		x <<- y
		matrix_inv <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse) matrix_inv <<- inverse
	getInverse <- function () matrix_inv
	list(set = set,
	     get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, which means
## the matrix has not changed, then it will retrieve the inverse from the cache without 
## doing any computation. If the inverse has not been calculated, then cacheSolve() 
## will compute and get the inverse matrix. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrix_inv <- x$getInverse()
		if (!is.null(matrix_inv)) {
			message("getting cached data")
			return(matrix_inv)
		}
		matrix <- x$get()
		matrix_inv <- solve(matrix, ...)
		x$setInverse(matrix_inv)
		matrix_inv
}
