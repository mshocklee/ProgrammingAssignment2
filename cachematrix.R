## makeCacheMatrix takes a matrix as an argument and returns a list of functions.
## cacheSolve takes a makeCacheMatrix function list as an argument and returns
## the inverse of the matrix accessible by the function list.


## The functions returned by makeCacheMatrix allow the setting of the matrix and
## caching of the inverse as well as the retrieval of the matrix and cached
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Passing in a makeCacheMatrix function list allows this function to check the
## cached inverse variable, return it if it is not NULL and otherwise compute,
## cache, and return the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
