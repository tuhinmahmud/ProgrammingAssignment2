## function creates a list containing functions to manipulate a matrix
##   the object created contains cache values of the matrix and its inverse 
##   and the list of following functions:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse matrix
##	4. get the value of the inverse matrix


## Function uses the matrix and returns a list of funcitons 
## that allows users to find inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <-NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get =get,
		setinverse = setinverse,
		getinverse = getinverse)

}
## the helper function used for finding and storing inverse matrix
## uses the cached value of the inverse if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <-x$getinverse()
	if(!is.null(i)) {
		message("getting cached data");
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
