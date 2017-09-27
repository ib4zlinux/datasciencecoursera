## These two functions are for programming assignment 2 based on the sample 
## in the assignment.
## The first will "cache" a matrix (i.e. read a matrix passed as input into
## a matrix, then build the inverse of that matrix
## The second will return either a.) the inverse of the cached matrix if 
## exists, or create the inverse of the cached matrix

## Read in a matrix and attempt to build the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
	## initialize a matrix m
	m <- NULL 

	## Set the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## Read the matrix in
	get <- function() x

	## Create the inverse of the matrix
	setinverse <- function(inverse) m <<- inverse

	## Read the inverse in
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Get the inverse of a matrix. If the inverse already exists, return it
## Otherwise set the inverse

cacheSolve <- function(x, ...) {
        ## Does the inverse exist?
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
	## if it does, return it and we're done
		return(m)
	}
	## inverse must not exist, so attempt to get the inverse
	data <- x$get()
	m <- solve(data, ...)
	
	## set the inverse and return
	x$setinverse(m)
	m
}
