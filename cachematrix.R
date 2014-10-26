## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The functions do offer this functionality.

## This function creates a special "matrix" object that can cache its inverse.
## Arguments: a square invertible matrix
## Returns: an object containing the functions that can be called on the special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	matrix <- x
	set <- function(y){
		matrix <<- y
		inverse <<- NULL
	}
	get <- function(){
		matrix
	}
	setInverse <- function(i){
		inverse <<- i
	}
	getInverse <- function(){
		inverse
	}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
		
}


## This function computes the inverse of the special "matrix".
## If the inverse has already been calculated (and it has not changed),
## then the function returns the cached result.
## Arguments: a special "matrix" returned by the makeCacheMatrix function. 
## Returns: The inverse of the special matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if (!is.null(i)){
		message("getting cached data")
		return(i)
	}
	matrix <- x$get()
	i <- solve(matrix)
	x$setInverse(i)
	i
}


